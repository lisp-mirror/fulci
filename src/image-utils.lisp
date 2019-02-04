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

(in-package :image-utils)

(define-constant +gimp-run-non-interactive+      1 :test #'=)

(define-constant +gimp-false+                    0 :test #'=)

(define-constant +gimp-tga-tempfilename+   "i.tga" :test #'string=)

(define-constant +gimp-img-tempfilename+   "i.jpg" :test #'string=)

(defun img-tmp-path (filename)
  (uiop:unix-namestring (text-utils:strcat (user-cache-dir) "/"
                                           +tmp-dir+
                                           filename)))

(defun gimp-batch-cmd (script)
  (let ((gimp-bin (pref:preferences-gimp-bin)))
    (if (text-utils:string-empty-p gimp-bin)
        (error (_ "No gimp program found in preferences"))
        (format nil "echo \"~a\" | ~a -i -b -" script gimp-bin))))

(defun gimp-image->tga-script (img-file tga-file)
  (let* ((img-path-string (format nil "\\\"~a\\\"" img-file))
         (tga-path-string (format nil "\\\"~a\\\"" tga-file))
         (script `(let* ((img     (car (gimp-file-load ,+gimp-run-non-interactive+
                                                       ,img-path-string
                                                       ,img-path-string)))
                         (surface (car (gimp-image-get-active-drawable img))))
                    (gimp-layer-add-alpha surface)
                    (file-tga-save ,+gimp-run-non-interactive+
                                   img
                                   surface
                                   ,tga-path-string
                                   ,tga-path-string
                                   0
                                   0)
                    (gimp-image-delete img)
                    (gimp-quit ,+gimp-false+))))
    (format nil "~(~a~)" script)))

(defun image->tga-file (image-file tga-file)
  (when (and (file-exists-p image-file)
             (file-exists-p tga-file))
    (misc:launch-command (gimp-batch-cmd (gimp-image->tga-script image-file tga-file)))))

(defun image->tga-memory (image-data)
  (let ((image-file (create-file (img-tmp-path +gimp-img-tempfilename+)))
        (tga-file   (create-file (img-tmp-path +gimp-tga-tempfilename+)))
        (tga-data nil))
    (unwind-protect
         (progn
           (with-open-file (stream image-file
                                   :direction         :output
                                   :if-does-not-exist :create
                                   :if-exists         :supersede
                                   :element-type      '(unsigned-byte 8))
             (write-sequence image-data stream))
           (image->tga-file image-file tga-file)
           (with-open-file (stream tga-file
                                   :direction         :input
                                   :if-does-not-exist :error
                                   :element-type      '(unsigned-byte 8))
             (setf tga-data (slurp-file tga-file :convert-to-string nil)))
           tga-data)
      (delete-file-if-exists image-file)
      (delete-file-if-exists tga-file))))

(defgeneric load-from-vector (object data))

(defmethod load-from-vector ((object tga) (data vector))
  (flexi-streams:with-input-from-sequence (stream data)
    (load-from-stream object stream)
    (if (not (null (errors object)))
        (error (format nil "Error loading image: ~a" (errors object)))
        object)))

(defun tga-as-vector (tga)
  (flexi-streams:with-output-to-sequence (stream)
    (write-sequence (pixmap->tga-file tga) stream)))
