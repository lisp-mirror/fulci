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

(in-package :constants)

(define-constant +help-about-message-template+
    " ~a
 Copyright (C) 2019  cage

 This program is free software:  you can redistribute it and/or modify
 it under the terms of the  GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is  distributed in the hope that it  will be useful, but
 WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program."
  :test #'string=)

(define-constant +http-code-ok+            200  :test #'=)

(define-constant +mime-type-jpg+           "image/jpeg"  :test #'string=)

(define-constant +mime-type-png+           "image/png"  :test #'string=)

(define-constant +mime-type-html+          "text/html"  :test #'string=)

(define-constant +db-file+                 (concatenate 'string
                                                        +home-data-dir+
                                                        "db.sqlite3")
  :test #'string=)

(define-constant +search-history-file+    (concatenate 'string
                                                        +home-data-dir+
                                                        "history")
  :test #'string=)

(define-constant +tmp-dir+                 (concatenate 'string
                                                        +home-data-dir+
                                                        "tmp/")
  :test #'string=)

(define-constant +country-csv-file+        (concatenate 'string
                                                        +sys-data-dir+
                                                        "countries.csv")
  :test #'string=)

(define-constant +min-padding+                       2 :test #'=)

(define-constant +char-start-search-expr+          #\! :test #'char=)

(define-constant +search-latest-id-re+            "^<" :test #'string=)

(define-constant +image-max-w+                     300 :test #'=)

(define-constant +image-max-h+                     480 :test #'=)

(define-constant +max-history-items-count+         100 :test #'=)

(define-constant +maximum-vote+                      5 :test #'=)
