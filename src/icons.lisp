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

(in-package :icons)

(define-constant +icon-dir+                                 "/icons/" :test #'string=)

(define-constant +icon-search+                               "search" :test #'string=)

(define-constant +icon-movie                                  "movie" :test #'string=)

(define-constant +icon-delete-movie+                   "delete-movie" :test #'string=)

(define-constant +icon-add-movie+                         "add-movie" :test #'string=)

(define-constant +icon-edit-movie+                       "edit-movie" :test #'string=)

(define-constant +icon-lookup-internet-movie+ "lookup-internet-movie" :test #'string=)

(define-constant +icon-dvd+                                     "dvd" :test #'string=)

(define-constant +icon-delete-dvd+                       "delete-dvd" :test #'string=)

(define-constant +icon-add-dvd+                             "add-dvd" :test #'string=)

(define-constant +icon-edit-dvd+                           "edit-dvd" :test #'string=)

(define-constant +icon-search+                               "search" :test #'string=)

(define-constant +icon-movie+                                 "movie" :test #'string=)

(define-constant +icon-delete-movie+                   "delete-movie" :test #'string=)

(define-constant +icon-add-movie+                         "add-movie" :test #'string=)

(define-constant +icon-edit-movie+                       "edit-movie" :test #'string=)

(define-constant +icon-lookup-internet-movie+ "lookup-internet-movie" :test #'string=)

(define-constant +icon-dvd+                                     "dvd" :test #'string=)

(define-constant +icon-delete-dvd+                       "delete-dvd" :test #'string=)

(define-constant +icon-add-dvd+                             "add-dvd" :test #'string=)

(define-constant +icon-edit-dvd+                           "edit-dvd" :test #'string=)

(define-constant +icon-persons+                             "persons" :test #'string=)

(define-constant +icon-genre+                                 "genre" :test #'string=)

(define-constant +icon-country+                               "world" :test #'string=)

(define-constant +icon-add+                                     "add" :test #'string=)

(define-constant +icon-delete+                               "delete" :test #'string=)

(define-constant +icon-add-small+                         "add-small" :test #'string=)

(define-constant +icon-delete-small+                   "delete-small" :test #'string=)

(define-constant +icon-edit+                                   "edit" :test #'string=)

(define-constant +icon-edit-small+                       "edit-small" :test #'string=)

(define-constant +icon-fulci+                            "fulci-icon" :test #'string=)

(define-constant +icon-goto-wiki+                        "goto-wiki"  :test #'string=)

(define-constant +icon-wiki-fetch-data+            "wiki-fetch-data"  :test #'string=)

(define-constant +icon-wiki-fetch-image+          "wiki-fetch-image"  :test #'string=)

(define-constant +icon-www-fetch-image+            "www-fetch-image"  :test #'string=)

(define-constant +icon-dvd-case+                       "dvd-in-case"  :test #'string=)

(define-constant +icon-barcode+                            "barcode"  :test #'string=)

(define-constant +icon-details+                            "details"  :test #'string=)

(define-constant +icon-goto-copy+                       "go-to-copy"  :test #'string=)

(defparameter *icon-search*                nil)

(defparameter *icon-movie*                 nil)

(defparameter *icon-delete-movie*          nil)

(defparameter *icon-add-movie*             nil)

(defparameter *icon-edit-movie*            nil)

(defparameter *icon-lookup-internet-movie* nil)

(defparameter *icon-dvd*                   nil)

(defparameter *icon-delete-dvd*            nil)

(defparameter *icon-add-dvd*               nil)

(defparameter *icon-edit-dvd*              nil)

(defparameter *icon-persons*               nil)

(defparameter *icon-genre*                 nil)

(defparameter *icon-country*               nil)

(defparameter *icon-add*                   nil)

(defparameter *icon-delete*                nil)

(defparameter *icon-add-small*             nil)

(defparameter *icon-delete-small*          nil)

(defparameter *icon-edit*                  nil)

(defparameter *icon-edit-small*            nil)

(defparameter *icon-goto-wiki*             nil)

(defparameter *icon-wiki-fetch-data*       nil)

(defparameter *icon-wiki-fetch-image*      nil)

(defparameter *icon-www-fetch-image*       nil)

(defparameter *icon-dvd-case*              nil)

(defparameter *icon-barcode*               nil)

(defparameter *icon-details*               nil)

(defparameter *icon-fulci*                 nil)

(defparameter *icon-goto-copy*             nil)

(defun load-icon (filename)
  (let ((path (if (not (scan "(?i)png$" filename))
                  (strcat +sys-data-dir+ +icon-dir+ filename ".png")
                  (strcat +sys-data-dir+ +icon-dir+ filename))))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (let ((data (nodgui.utils:read-into-array stream (file-length stream))))
        (make-image data)))))

(defun load-icons ()
  (setf *icon-search*                (load-icon +icon-search+))
  (setf *icon-delete-movie*          (load-icon +icon-delete-movie+))
  (setf *icon-search*                (load-icon +icon-search+))
  (setf *icon-movie*                 (load-icon +icon-movie+))
  (setf *icon-delete-movie*          (load-icon +icon-delete-movie+))
  (setf *icon-add-movie*             (load-icon +icon-add-movie+))
  (setf *icon-edit-movie*            (load-icon +icon-edit-movie+))
  (setf *icon-lookup-internet-movie* (load-icon +icon-lookup-internet-movie+))
  (setf *icon-dvd*                   (load-icon +icon-dvd+))
  (setf *icon-delete-dvd*            (load-icon +icon-delete-dvd+))
  (setf *icon-add-dvd*               (load-icon +icon-add-dvd+))
  (setf *icon-edit-dvd*              (load-icon +icon-edit-dvd+))
  (setf *icon-persons*               (load-icon +icon-persons+))
  (setf *icon-genre*                 (load-icon +icon-genre+))
  (setf *icon-country*               (load-icon +icon-country+))
  (setf *icon-add*                   (load-icon +icon-add+))
  (setf *icon-delete*                (load-icon +icon-delete+))
  (setf *icon-add-small*             (load-icon +icon-add-small+))
  (setf *icon-delete-small*          (load-icon +icon-delete-small+))
  (setf *icon-edit*                  (load-icon +icon-edit+))
  (setf *icon-edit-small*            (load-icon +icon-edit-small+))
  (setf *icon-goto-wiki*             (load-icon +icon-goto-wiki+))
  (setf *icon-wiki-fetch-data*       (load-icon +icon-wiki-fetch-data+))
  (setf *icon-wiki-fetch-image*      (load-icon +icon-wiki-fetch-image+))
  (setf *icon-www-fetch-image*       (load-icon +icon-www-fetch-image+))
  (setf *icon-dvd-case*              (load-icon +icon-dvd-case+))
  (setf *icon-barcode*               (load-icon +icon-barcode+))
  (setf *icon-details*               (load-icon +icon-details+))
  (setf *icon-goto-copy*             (load-icon +icon-goto-copy+))
  (setf *icon-fulci*                 (load-icon +icon-fulci+)))
