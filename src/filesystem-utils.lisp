;; fulci: a program to organize your movies collection
;; Copyright (C) 2019  cage

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :filesystem-utils)

(define-constant +preprocess-include+ "^%include"              :test #'string=)

(define-constant +file-path-regex+ "[\\p{L},\\/,\\\\,\\.]+"    :test 'string=)

(defparameter *directory-sep-regexp*
  #+windows "\\"
  #-windows "\\/")

(defparameter *directory-sep*
  #+windows "\\"
  #-windows "/")

(defun copy-a-file (in out &key (overwrite nil))
  (if (and in
           (file-exists-p in)
           out
           (or (not (file-exists-p out))
               overwrite))
      (progn
        (uiop:copy-file in out)
        out)
      nil))

(defun file-size (filename)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8)
                          :if-does-not-exist nil)
    (if (null stream)
        0
        (file-length stream))))

(defmacro with-file-lines ((line file line-count) &body body)
  (with-gensyms (stream)
  `(with-open-file (,stream ,file :direction :input :if-does-not-exist :error)
     (when ,stream
       (do ((,line (read-line ,stream nil nil) (read-line ,stream nil nil))
	    (,line-count 0 (1+ ,line-count)))
	   ((not ,line) ,line-count)
         ,@body)))))

(defun count-lines-in-file (file)
  (with-file-lines (line file ct)))

(defun file->list-lines (file)
  (let ((res '()))
    (with-file-lines (line file ct)
      (push line res))
    (reverse res)))

(defun list-lines->file (data file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (loop for i in data do
         (format stream "~a~%" i))))

(defun append-line->file (data file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :error)
    (format stream "~a~%" data)))

(defun slurp-file (filename &key (convert-to-string t))
  "A simple way to slurp a file."
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      (if convert-to-string
          (babel:octets-to-string seq)
          seq))))

(defun dump-sequence-to-file (seq file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sequence seq stream)))

(defun create-file (file)
  "create file and parent dir, if necessary"
  (let ((path-splitted (fs:split-path-elements file)))
    (when (and path-splitted
               (> (length path-splitted) 1))
      (do* ((path-rest (subseq path-splitted 0 (1- (length path-splitted))) (rest path-rest))
            (path-so-far "" (if (and path-rest
                                     (not (string= "" (first-elt path-rest))))
                                (concatenate 'string
                                             path-so-far
                                             *directory-sep*
                                             (first-elt path-rest)
                                             *directory-sep*)
                                path-so-far)))
           ((null path-rest))
        (when (not (directory-exists-p path-so-far))
          (make-directory path-so-far)))
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create))
      file)))

(defun create-file-if-not-exists (path)
  (and (not (file-exists-p path))
       (create-file path)))

(defun has-extension (path ext)
  (let ((re (concatenate 'string ext "$")))
    (cl-ppcre:scan re path)))

(defun cat-parent-dir (parent direntry)
  (format nil "~a~a~a" parent *directory-sep* direntry))

(defmacro do-directory ((var) root &body body)
  (with-gensyms (dir)
    `(let ((,dir (nix:opendir ,root)))
       (unwind-protect
            (handler-case
                (do ((,var (cat-parent-dir ,root (nix:readdir ,dir))
                           (cat-parent-dir ,root (nix:readdir ,dir))))
                    ((cl-ppcre:scan "NIL$" ,var))
                  ,@body)
              (nix::enotdir () 0)
              (nix:eacces () 0)
              (nix:eloop () 0))
       (nix:closedir ,dir)))))

(defun search-matching-file (root-directory &key (name ".*"))
  "Scan a filesystem saving files that match the provided criteria,
   does not follow symlinks."
  (let ((matched '())
        (scanner (cl-ppcre:create-scanner name)))
    (labels ((match (dir)
               (do-directory (path) dir
                   (let ((filename (path-last-element path)))
                     (cond
                       ((regular-file-p path)
                        (when (cl-ppcre:scan scanner filename)
                          (push path matched)))
                       ((and (not (cl-ppcre:scan "^\\.\\." filename))
                             (not (cl-ppcre:scan "^\\."   filename))
                             (dirp path))
                        (match path)))))))
      (match root-directory)
      matched)))

(defun regular-file-p (path)
  (nix:s-isreg (nix:stat-mode (nix:stat path))))

(defun dirp (path)
  (nix:s-isdir (nix:stat-mode (nix:stat path))))

(defun split-path-elements (path)
  (cl-ppcre:split *directory-sep-regexp* path))

(defun path-last-element (path)
  (let ((elements (cl-ppcre:split *directory-sep-regexp* path)))
    (and elements
         (last-elt elements))))

(defun path-first-element (path)
  (let ((elements (cl-ppcre:split *directory-sep-regexp* path)))
    (and elements
         (first-elt elements))))

(defun path-to-hidden-file-p (path)
  "unix-like only"
  (let ((last-element (path-last-element path)))
    (and path (cl-ppcre:scan "^\\." last-element))))

(defun strip-dirs-from-path (p)
  (multiple-value-bind (all registers)
      (cl-ppcre:scan-to-strings (concatenate 'string
                                             *directory-sep*
                                             "([^"
                                             *directory-sep*
                                             "]+)$")
                                p)
    (declare (ignore all))
    (and (> (length registers) 0)
         (elt registers 0))))

(defun parent-dir-path (path)
  (let ((splitted (remove-if #'(lambda (a) (string= "" a))
                             (split-path-elements path))))
    (cond
      ((> (length splitted) 1)
       (let ((res (if (string= (string (elt path 0)) *directory-sep*)
                      (concatenate 'string *directory-sep* (first splitted))
                      (first splitted))))
         (loop for i in (subseq splitted 1 (1- (length splitted))) do
              (setf res (concatenate 'string res *directory-sep* i)))
         (setf res (concatenate 'string res *directory-sep*))
         res))
      ((null splitted)
       *directory-sep*)
      (t
       path))))

(defmacro define-stat-time (slot-name)
  (with-gensyms (stat)
    `(defun ,(format-symbol t "~:@(get-stat-~a~)" slot-name) (file)
       (restart-case
           (let ((,stat (nix:stat file)))
             (when ,stat
               (misc:time-unix->universal (,(format-symbol :nix "~:@(stat-~a~)" slot-name)
                                            ,stat))))
         (use-value (value) value)))))

(define-stat-time mtime)

(define-stat-time ctime)

(define-stat-time atime)

(defun file-outdated-p (file &rest dependencies)
  (handler-bind ((nix:enoent #'(lambda (c)
                                 (declare (ignore c))
                                 (invoke-restart 'use-value nil))))
    (let ((mtime-file  (get-stat-mtime file))
          (mtimes-deps (remove-if #'null (mapcar #'get-stat-mtime dependencies))))
      (if mtime-file
          (remove-if #'(lambda (mtime) (<= mtime mtime-file)) mtimes-deps)
          t))))

(defun file-exists-p (f)
  (uiop:file-exists-p f))

(defun directory-exists-p (d)
  (uiop:directory-exists-p d))

(defun delete-file-if-exists (f)
  (uiop:delete-file-if-exists f))

(defun file-length-if-exists (f)
  (when (file-exists-p f)
    (with-open-file (stream f :element-type '(unsigned-byte 8))
      (file-length stream))))

(defmacro gen-home-dirs (name fn)
  (with-gensyms (dir)
    `(defun ,(misc:format-fn-symbol t "~a-dir" name) (&key (add-separator-ends nil))
       (let ((,dir (pathname->namestring (funcall ,fn))))
         (if add-separator-ends
             (text-utils:strcat ,dir *directory-sep*)
             ,dir)))))

(gen-home-dirs home (lambda () (nix:getenv "HOME")))

(gen-home-dirs user-data #'uiop:xdg-data-home)

(gen-home-dirs user-config #'uiop:xdg-config-home)

(gen-home-dirs user-cache  #'uiop:xdg-cache-home)

(defun temporary-filename (&optional (temp-directory nil))
  (let ((tmpdir (or temp-directory (nix:getenv "TMPDIR"))))
    (if tmpdir
        (nix:mktemp (format nil "~a~a~a" tmpdir *directory-sep*
                    config:+program-name+))
        (nix:mktemp (format nil "~atmp~a~a" *directory-sep* *directory-sep*
                    config:+program-name+)))))

(defmacro with-anaphoric-temp-file ((stream &key (prefix nil) (unlink nil)) &body body)
  `(let ((temp-file (temporary-filename ,prefix))) ; anaphora
       (unwind-protect
            (with-open-file (,stream temp-file
                                     :direction         :output
                                     :if-exists         :error
                                     :if-does-not-exist :create)
              ,@body)
         ,(if unlink
              `(delete-file-if-exists temp-file)
              nil))))

(defun has-file-permission-p (file permission)
  (find permission (osicat:file-permissions file) :test #'eq))

(defun file-can-write-p (file)
  (has-file-permission-p file :user-write))

(misc:defcached cached-directory-files ((path) :test equal)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (if (gethash path cache)
      (gethash path cache)
      (progn
        (setf (gethash path cache) (uiop:directory-files path))
        (cached-directory-files path))))

(defun directory-files (path)
  (and path
       (uiop:directory-files path)))

(defun make-directory (path)
  (if (not (cl-ppcre:scan (concatenate 'string *directory-sep* "$") path))
      (make-directory (concatenate 'string path *directory-sep*))
      (ensure-directories-exist path)))

(defun package-path ()
  (uiop:pathname-parent-directory-pathname
   (asdf:component-pathname
    (asdf:find-component (symbolicate (string-upcase config:+program-name+))
                         nil))))

(defun file-in-package (name)
  (concatenate 'string (namestring (package-path)) name))

(defun pathname->namestring (p)
  (uiop:native-namestring p))

(defun namestring->pathname (p)
  (uiop:ensure-pathname p))

(defun read-single-form (file)
  (with-open-file (stream file :direction :input :if-does-not-exist nil)
    (when stream
      (read stream))))

(defun eq-filename (a b)
  (flet ((strip (a) (strip-dirs-from-path (pathname->namestring a))))
    (string= (strip a)
             (strip b))))
