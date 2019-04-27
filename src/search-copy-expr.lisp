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

(in-package :search-copy-expr)

(define-constant +equal-re+                     "=" :test #'string=)

(define-constant +lt-re+                        "<" :test #'string=)

(define-constant +gt-re+                        ">" :test #'string=)

(define-constant +and-re+               "\\band\\b" :test #'string=)

(define-constant +or-re+                 "\\bor\\b" :test #'string=)

(define-constant +value-re+      "[a-zA-Z0-9%\\-]+" :test #'string=)

(define-constant +key-director-re+       "director" :test #'string=)

(define-constant +key-year-re+             "year" :test #'string=)

(define-constant +key-title-re+             "title" :test #'string=)

(define-constant +key-note-re+               "note" :test #'string=)

(define-constant +key-tags-re+               "tags" :test #'string=)

(define-constant +key-genres-re+           "genres" :test #'string=)

(define-constant +key-country-re+         "country" :test #'string=)

(define-constant +key-building-re+       "building" :test #'string=)

(define-constant +key-room-re+               "room" :test #'string=)

(define-constant +key-storage-re+         "storage" :test #'string=)

(define-constant +key-shelf-re+             "shelf" :test #'string=)

(define-constant +open-expr-re+               "\\(" :test #'string=)

(define-constant +close-expr-re+              "\\)" :test #'string=)

(defmacro gen-lexer-constant (name)
  `(define-constant
       ,(format-fn-symbol t "+~a+" name)
       ,(make-keyword name)
     :test #'eq))

(gen-lexer-constant and)

(gen-lexer-constant or)

(gen-lexer-constant equal)

(gen-lexer-constant lt)

(gen-lexer-constant gt)

(gen-lexer-constant value)

(gen-lexer-constant key-director)

(gen-lexer-constant key-year)

(gen-lexer-constant key-title)

(gen-lexer-constant key-note)

(gen-lexer-constant key-tags)

(gen-lexer-constant key-genres)

(gen-lexer-constant key-country)

(gen-lexer-constant key-building)

(gen-lexer-constant key-room)

(gen-lexer-constant key-storage)

(gen-lexer-constant key-shelf)

(gen-lexer-constant open-expr)

(gen-lexer-constant close-expr)

(defmacro gen-lexer ()
  `(define-string-lexer lexer
     (,+equal-re+
      (return (values +equal+    +equal+)))
     (,+lt-re+
      (return (values +lt+    +lt+)))
     (,+gt-re+
      (return (values +gt+    +gt+)))
     (,+and-re+
      (return (values +and+      +and+)))
     (,+or-re+
      (return (values +or+       +or+)))
     (,+key-note-re+
      (return (values +key-note+     +key-note+)))
     (,+key-director-re+
      (return (values +key-director+ +key-director+)))
     (,+key-year-re+
      (return (values +key-year+    +key-year+)))
     (,+key-title-re+
      (return (values +key-title+    +key-title+)))
     (,+key-tags-re+
      (return (values +key-tags+     +key-tags+)))
     (,+key-genres-re+
      (return (values +key-genres+    +key-genres+)))
     (,+key-country-re+
      (return (values +key-country+   +key-country+)))
     (,+key-building-re+
      (return (values +key-building+   +key-building+)))
     (,+key-room-re+
      (return (values +key-room+       +key-room+)))
     (,+key-storage-re+
      (return (values +key-storage+   +key-storage+)))
     (,+key-shelf-re+
      (return (values +key-shelf+     +key-shelf+)))
     (,+open-expr-re+
      (return (values +open-expr+ +open-expr+)))
     (,+close-expr-re+
      (return (values +close-expr+ +close-expr+)))
     (,+value-re+
      (return (values +value+    $@)))))

;; EXPRESSION    := EXPRESSION AND EXPRESSION |
;;                  EXPRESSION OR  EXPRESSION |
;;                  '(' EXPRESSION ')'        |
;;                  TERM
;; TERM          := KEY = VALUE | KEY < VALUE | KEY > VALUE
;; KEY           := 'director' | 'year' | 'title' | 'note' | 'tags' | 'genres' | 'country' |
;;                  'building' | 'room' | 'storage' | 'shelf'
;; VALUE         := [a-z,A-Z,0-9,%]
;; AND           := 'and'
;; OR            := 'or'
;; =             := '='
;; <             := '<'
;; >             := '>'

(defmacro gen-parser ()
  `(yacc:define-parser *parser*
     (:print-derives-epsilon t)
     (:start-symbol expression)
     (:terminals  ,(list +and+ +or+ +equal+ +lt+ +gt+ +value+
                         +key-director+ +key-year+ +key-title+ +key-genres+ +key-country+
                         +key-note+ +key-tags+ +key-building+ +key-room+ +key-storage+
                         +key-shelf+ +open-expr+ +close-expr+))
     (:precedence ,(list (list :left +and+) (list :left +or+)))
     (expression
      (expression ,+and+ expression #'infix->prefix)
      (expression ,+or+ expression  #'infix->prefix)
      (,+open-expr+ expression ,+close-expr+ #'keep-middle)
      term
      ())
     (term
      (key ,+equal+ value #'make-equal-clause)
      (key ,+gt+ value-cmp #'make-greater-than-clause)
      (key ,+lt+ value-cmp #'make-less-than-clause))
     (key
      ,+key-director+
      ,+key-year+
      ,+key-title+
      ,+key-genres+
      ,+key-note+
      ,+key-tags+
      ,+key-country+
      ,+key-building+
      ,+key-room+
      ,+key-storage+
      ,+key-shelf+)
     (value-cmp
      (,+value+ #'quote-value))
     (value
      (,+value+ #'quote-value-for-like))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun quote-value-for-like (a)
    (format nil "\"~a\"" (prepare-for-sql-like a)))

  (defun quote-value (a)
    (format nil "\"~a\"" a))

  (defun infix->prefix (a b c)
    (list (quote-symbol b) a c))

  (defun keep-middle (a b c)
    (declare (ignore a c))
    b)

  (defun key->col (k)
    (let ((as-symbol (cond
                       ((eq k +key-director+)
                        +search-expr-director-col+)
                       ((eq k +key-year+)
                        +search-expr-year-col+)
                       ((eq k +key-title+)
                        +search-expr-primary-title-col+)
                       ((eq k +key-note+)
                        +search-expr-notes-col+)
                       ((eq k +key-tags+)
                        +search-expr-tags-col+)
                       ((eq k +key-genres+)
                        +search-expr-genres-col+)
                       ((eq k +key-country+)
                        +search-expr-country-col+)
                       ((eq k +key-building+)
                        +search-expr-building-col+)
                       ((eq k +key-room+)
                        +search-expr-room-col+)
                       ((eq k +key-storage+)
                        +search-expr-storage-col+)
                       ((eq k +key-shelf+)
                        +search-expr-shelf-col+)
                       (t
                        (error "invalid key name")))))
      (quote-symbol as-symbol)))

  (defun make-equal-clause (a b c)
    (declare (ignore b))
    (let ((actual-key (key->col a)))
      ;;workaround...
      (if (string= actual-key (quote-symbol +search-expr-primary-title-col+))
          `("or" ("or" ("like" ,actual-key                                      ,c)
                       ("like" ,(quote-symbol +search-expr-original-title-col+) ,c))
                 ("or" ("like" ,(quote-symbol :add-ot) ,c)
                       ("like" ,(quote-symbol :add-pt) ,c)))
          `("like" ,actual-key ,c))))

  (defun make-compare-clause (op key value)
    (let ((actual-key (key->col key)))
      `(,op ,actual-key ,value)))

  (defun make-less-than-clause (a b c)
    (declare (ignore b))
    (make-compare-clause "<" a c))

  (defun make-greater-than-clause (a b c)
    (declare (ignore b))
    (make-compare-clause ">" a c))

  (gen-lexer)

  (gen-parser))

(defun parse (query)
  (conditions:with-default-on-error (nil)
    (let ((raw   (parse-with-lexer (lexer query) *parser*))
          (stack '()))
      (labels ((expr->stack (expr)
                 (if (atom expr)
                     (push expr stack)
                     (progn
                       (push ")" stack)
                       (expr->stack (third  expr))
                       (expr->stack (first  expr))
                       (expr->stack (second expr))
                       (push "(" stack)))))
        (when raw
          (values (strcat " where " (text-utils:join-with-strings (expr->stack raw) " "))
                  raw))))))
