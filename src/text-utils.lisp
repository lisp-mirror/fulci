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

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; derived from

;; niccolo': a chemicals inventory
;; Copyright (C) 2016  Universita' degli Studi di Palermo

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published  by  the  Free  Software Foundation,  version  3  of  the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :text-utils)

(alexandria:define-constant +float-regexp+ "-?[0-9]+(\\.[0-9]+([eE]-?[0-9]+)?)?" :test 'string=)

(alexandria:define-constant +integer-regexp+ "0|[1-9][0-9]+|[1-9]" :test 'string=)

(defun uchar-length (leading-byte)
  (let ((ones (do* ((ct 7 (1- ct))
                    (bit (ldb (byte 1 ct) leading-byte)
                         (ldb (byte 1 ct) leading-byte))
                    (ones-ct 0))
                   ((= bit 0) ones-ct)
                (incf ones-ct))))
    (cond
      ((= ones 0)
       1)
      ((= ones 1)
       0)
      (t
       ones))))

(defun utf8-encoded-p (file)
  (with-open-file (stream file :direction :input
                          :if-does-not-exist :error
                          ::element-type '(unsigned-byte 8))
    (let* ((leading-byte (read-byte stream))
           (leading-byte-length (uchar-length leading-byte)))
      (cond
        ((= leading-byte-length 0)
         nil)
        ((> leading-byte-length 6)
         nil)
        (t
         (loop for i from 0 below (1- leading-byte-length) do
              (let* ((ch (read-byte stream))
                     (ll (uchar-length ch)))
                (when (> ll 0)
                  (return-from utf8-encoded-p nil))))
         t)))))

(defun to-s (v)
  (format nil "~a" v))

(defun clean-unprintable-chars (string)
  (cl-ppcre:scan-to-strings "[\\p{Letter}\\p{Number}\\p{Punctuation}]+" string))

(defun strcat (&rest chunks)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (the string (apply #'concatenate (concatenate 'list (list 'string) chunks))))

(defun strip-prefix (string prefix)
  (let ((re (strcat "^" prefix)))
    (cl-ppcre:regex-replace re string "")))

(defun strip-withespaces (string)
  (let ((re "\\s"))
    (cl-ppcre:regex-replace re string "")))

(defun string-empty-p (s)
  (or (not s)
      (string= s "")))

(defun string-not-empty-p (s)
  (not (string-empty-p s)))

(defun join-with-strings (strings junction)
  (reduce #'(lambda (a b) (text-utils:strcat a junction b)) strings))

(defun join-with-strings* (junction &rest strings)
  (reduce #'(lambda (a b) (text-utils:strcat a junction b)) strings))

(defun split-words (text)
  (cl-ppcre:split "\\p{White_Space}" text))

(defun split-lines (text)
  (cl-ppcre:split "[\\n\\r]" text))

(defun min-length-word (text)
     (loop for i in (split-words text)
        minimizing (length i) into min
        finally (return min)))

(defun max-length-word (text)
  (loop for i in (split-words text)
     maximizing (length i) into max
     finally (return max)))

(defun basename (file)
  (let ((pos (cl-ppcre:scan "\\." file)))
    (if pos
        (subseq file 0 pos)
        file)))

(defun wrap-with (s wrapper)
  (strcat wrapper s wrapper))

(defun right-padding (str total-size &key (padding-char #\Space))
  (strcat str
          (make-string (max 0 (- total-size (length str)))
                       :initial-element padding-char)))

(defun left-padding (str total-size &key (padding-char #\Space))
  (strcat (make-string (max 0 (- total-size (length str)))
                       :initial-element padding-char)
          str))

(defun justify-monospaced-text (text &optional (chars-per-line 30))
  (if (null (split-words text))
      (list " ")
      (let  ((text  (split-words text))
             (chars-per-line (round chars-per-line)))

        (labels ((spaces-pos-per-line (line) (floor (/ (length line) 2)))
                 (wline<= (l) (<= l  chars-per-line))
                 (line-length (line)
                   (reduce #'+ (mapcar #'length line) :initial-value 0))
                 (line-fit-p (line word)
                   (wline<= (+ (line-length line) (length word))))
                 (add-until-fit (text &optional (res '()))
                   (if (not (line-fit-p res (first text)))
                       (subseq res 0 (1- (length res)))
                       (add-until-fit (rest text) (append res (list (first text) " ")))))
                 (get-spacepos (line how-much)
                   (do ((pos '()))
                       ((>= (length pos) how-much) pos)
                     (let ((ranpos (random (length line))))
                       (when (and (oddp ranpos)
                                  (not (find ranpos pos :test #'=)))
                         (push ranpos pos)))))
                 (increment-each-space (line)
                   (loop for i in line collect (if (cl-ppcre:scan "\\p{White_Space}+" i)
                                                   (concatenate 'string i (string " "))
                                                   i)))
                 (justify-line (line &optional
                                     (spaces-left (- chars-per-line (line-length line))))
                   (cond
                     ((= (spaces-pos-per-line line) 0)
                      (copy-list line))
                     ((= spaces-left 0)
                      (copy-list line))
                     ((= spaces-left (spaces-pos-per-line line))
                      (increment-each-space line))
                     ((< spaces-left (spaces-pos-per-line line))
                      (loop for i in (get-spacepos line spaces-left) do
                           (setf (nth i line) (concatenate 'string (nth i line) (string " "))))
                      (copy-list line))
                     ((> spaces-left (spaces-pos-per-line line))
                      (justify-line
                       (increment-each-space line)
                       (- spaces-left (spaces-pos-per-line line)))))))
          (mapcar #'(lambda (l) (reduce #'(lambda (a b) (concatenate 'string a b)) l))
                  (do ((results '()))
                      ((null text) (reverse results))
                    (progn
                      (let* ((line (add-until-fit text))
                             (rest-text (if (> (1+ (floor (/ (length line) 2)))
                                               (length text))
                                            nil
                                            (subseq text (1+ (floor (/ (length line) 2)))))))
                        (setf text rest-text)
                        (push (justify-line line) results)))))))))

(defun flush-left-mono-text (text-words box-width &optional (lines '()))
  (flet ((join (words)
           (if words
               (join-with-strings words " ")
               "")))
    (if (null text-words)
        (reverse lines)
        (multiple-value-bind (line rest-of-words)
            (do ((words  text-words (rest words))
                 (line   '()        (misc:lcat line (list (first words))))
                 (line+1 '()        (if (> (length words) 1)
                                        (misc:lcat line (subseq words 0 2))
                                        line)))
                ((or (null words)
                     (> (length (join line+1)) box-width))
                 (values (join line) words)))
          (flush-left-mono-text rest-of-words box-width (misc:lcat (list line) lines))))))

(defun escape-tilde (s)
  "~ -> ~~, for format"
  (cl-ppcre:regex-replace-all "~" s "~~"))

(defun ellipsize (value &key (len 15) (truncate-string "..."))
  "If \"string\"'s  length is bigger  than \"len\", cut  the exceeding
  characters out.  Also  replaces the last character  of the shortened
  string with truncate-string. It defaults  to \"...\", but can be nil
  or the empty string."
  (let* ((string-value (to-s value))
         (string-len   (length string-value)))
    (if (<= string-len len)
        string-value
        (strcat (subseq string-value 0 len)
                truncate-string))))
