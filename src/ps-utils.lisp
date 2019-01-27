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

;; derived from:

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

(in-package :ps-utils)

(define-constant +page-rendering-max-w+    200                  :test #'=)

(define-constant +font-name+            "font"                  :test #'string=)

(define-constant +font-file+ (strcat +sys-data-dir+ "/" "font.afm") :test #'string=)

(define-constant +page-margin-top+          20                  :test #'=)

(defun encode-barcode (id)
  (format nil "$START-B~8,'0d" id))

(defmacro with-save-restore ((doc) &body body)
  `(progn
     (save ,doc)
     ,@body
     (restore ,doc)))

(let ((memoized-font nil)
      (memoized-doc  nil))
  (defun default-font (doc)
    "Note assume +parameter-key-searchpath+ is correctly set"
    (if (and memoized-font
             (or (not memoized-doc)
                 (eq  memoized-doc doc))
             (> memoized-font 0))       ; find-font  return a positive number on
                                        ; success, we  do not want to  memoize a
                                        ; failure
        memoized-font
        (let ((font-handle (findfont doc +font-name+ "" t)))
          (setf memoized-font font-handle
                memoized-doc  doc)
          memoized-font))))

(defun render-simple-label-barcode (doc barcode-text label &key
                                                             (w 80.0)
                                                             (h 40.0)
                                                             (font-size (/ h 2))
                                                             (padding 5.0))
  (let ((font    (default-font doc))
        (barcode (make-instance 'brcd:code128)))
    (ps:setcolor doc ps:+color-type-fillstroke+ (cl-colors:rgb 0.0 0.0 0.0))
    (ps:setfont doc font font-size)
    (brcd:parse barcode (encode-barcode barcode-text))
    (with-save-restore (doc)
      (ps:scale doc
                (/ w (brcd:width barcode))
                (/ h (+ padding font-size (brcd:height barcode))))
      (with-save-restore (doc)
        (ps:translate doc padding padding)
        (brcd:draw barcode doc))
      (with-save-restore (doc)
        (ps:draw-text-confined-in-box doc
                                      font
                                      label
                                      padding                           ; left
                                      (+ padding (brcd:height barcode)) ; top
                                      (brcd:width barcode)              ; width
                                      font-size                         ; height
                                      :vertical-align :bottom)))
    h))

(define-constant +sample-labels-padding+ 1.0 :test #'=)

(defun render-barcode-table (barcodes labels page-w page-h  w h)
  (let* ((page-size         (make-instance 'page-size
                                           :width  page-w
                                           :height page-h))
         (doc               (make-instance 'psdoc
                                           :page-size page-size))
         (*callback-string* ""))
    (assert (length= barcodes labels))
    (open-doc doc nil)
    (set-parameter doc
                   +parameter-key-searchpath+
                   +sys-data-dir+)
    (begin-page doc)
    (do ((label   labels   (rest label))
         (barcode barcodes (rest barcode))
         (y       +page-margin-top+)
         (x       +sample-labels-padding+))
        ((null label))
      (when (>= (+ x w) +page-rendering-max-w+)
        (setf x +sample-labels-padding+)
        (incf y h))
      (when (> (+ y h) (- (height page-size) +page-margin-top+))
        (end-page doc)
        (begin-page doc)
        (setf y +page-margin-top+)
        (setf x +sample-labels-padding+))
      (with-save-restore (doc)
        (translate doc x y)
        (with-save-restore (doc)
          (ps:setlinewidth doc (max 0.1 (/ w 500)))
          (ps:setcolor doc ps:+color-type-fillstroke+ cl-colors:+red+)
          ;; hline
          (ps:moveto doc +sample-labels-padding+ +sample-labels-padding+)
          (ps:lineto doc w +sample-labels-padding+)
          ;; vline
          (ps:moveto doc +sample-labels-padding+ 0)
          (ps:lineto doc +sample-labels-padding+ h)
          (ps:stroke doc))
        (render-simple-label-barcode doc
                                     (first barcode)
                                     (first label)
                                     :w w
                                     :h h))
      (incf x w))
    (end-page doc)
    (close-doc doc)
    (shutdown)
    *callback-string*))
