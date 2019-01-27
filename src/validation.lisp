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

(in-package :validation)

(define-constant +email-re+ "(?i)[a-z,0-9,\\-,_]+\\.?[a-z,0-9,\\-,_]+@[a-z,0-9,\\-,_]+\\.[a-z,0-9,\\-,_]+" :test #'string=)

(define-constant +integer-re+                "^-?[1-9][0-9]*$"                   :test #'string=)

(define-constant +pos-integer-re+            "^[1-9][0-9]*$"                     :test #'string=)

(define-constant +barcode-id-re+             "^[0-9][0-9]*$"                     :test #'string=)

(define-constant +internet-address-re+       "^[0-9]?[0-9]?[0-9]\.[0-9]?[0-9]?[0-9]\.[0-9]?[0-9]?[0-9]\.[0-9]?[0-9]?[0-9]$"
  :test #'string=)

(define-constant +free-text-re+              ".+"                                :test #'string=)

(define-constant +copy-position-re+          "[^,]+,[^,]+,[^,]+,[^,]+"           :test #'string=)

(defun all-not-null-p (&rest vals)
  (notany #'null vals))

(defun all-null-p (&rest vals)
  (every #'null vals))

(defun regexp-validate (data)
  "data -> '(datum regexp error-message)"
  (loop for (data regexp message) in data when (not (scan regexp data)) collect
       message))

(defun id-valid-and-used-p (class id)
  (and (null (regexp-validate (list (list id +pos-integer-re+ (_ "Id invalid")))))
       (db-utils:object-exists-in-db-p class id)))

(defmacro with-id-valid-and-used (class id msg-not-exists)
  (with-gensyms (error-msg-not-integer error-msg-not-exists)
    `(let* ((,error-msg-not-integer (validation:regexp-validate (list (list ,id
                                                                            +pos-integer-re+
                                                                            (_ "Id invalid")))))
            (,error-msg-not-exists  (when (and (not ,error-msg-not-integer)
                                               (not (db-utils:object-exists-in-db-p ,class ,id)))
                                      ,msg-not-exists)))
       (concatenate 'list ,error-msg-not-exists ,error-msg-not-integer))))


(defun boolean-p-validate (var)
  (or (string= var "0")
      (string= var "1")))

(defun integer-validate (i &key (default nil))
  (handler-case
      (parse-integer i :junk-allowed nil)
    (error () default)))

(defun integer-positive-validate (v)
  (handler-case
      (let ((parsed (parse-integer v :junk-allowed nil)))
        (and parsed
             (> parsed 0)))
    (error () nil)))

(defun date-validate-p (d)
  (local-time:parse-timestring d :fail-on-error nil))

(defun magic-validate-p (file magic)
  (if file
      (with-open-file (stream file
                              :direction :input
                              :if-does-not-exist nil
                              :element-type '(unsigned-byte 8))
        (and stream
             (> (file-length stream) (length magic))
             (equalp magic (loop repeat (length magic) collect (read-byte stream)))))
      nil))

(defun png-validate-p (file)
  (magic-validate-p file '(#x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)))

(defun integer-%-validate (v)
  (let ((parsed (parse-integer v :junk-allowed t)))
    (and parsed
         (>= parsed 0)
         (<= parsed 100))))
