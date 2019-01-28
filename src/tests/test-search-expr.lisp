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

(in-package :test-search-title-expr)

(defsuite search-title-expr-suite (all-suite))

(defun tokenize (input)
  (let* ((tokenizer-fn (lexer input)))
    (funcall tokenizer-fn)))

(defun tokenize-fn (input)
  (let* ((tokenizer-fn (lexer input)))
    tokenizer-fn))

(defun token-list-eq (l &rest expected)
  (every 'eq l expected))

(defmacro with-tokenizer (input &body body)
  (with-gensyms (tokenizer-fn)
    `(let ((,tokenizer-fn (lexer ,input)))
       (flet ((tok ()
                (funcall ,tokenizer-fn)))
         ,@body))))

(deftest test-single (search-title-expr-suite)
  (assert-true
      (with-tokenizer "and"
        (token-list-eq (list (tok))
                       +and+)))
  (assert-true
      (with-tokenizer " or "
        (token-list-eq (list (tok))
                       +or+)))
  (assert-true
      (with-tokenizer " = "
        (token-list-eq (list (tok))
                       +equal+)))
  (assert-false
      (with-tokenizer "andy"
        (token-list-eq (list (tok))
                       +and+)))
  (assert-false
      (with-tokenizer "torch"
        (token-list-eq (list (tok))
                       +or+))))

(deftest test-multiple (search-title-expr-suite)
  (assert-true
      (with-tokenizer "foo and bar"
        (token-list-eq (list (tok)   (tok) (tok)
                             +value+ +and+ +value+))))
    (assert-true
        (with-tokenizer "foo or bar"
          (token-list-eq (list (tok)  (tok) (tok)
                               +value+ +or+ +value+))))
    (assert-true
        (with-tokenizer "forest and bar"
          (token-list-eq (list (tok)  (tok) (tok)
                               +value+ +and+ +value+))))
    (assert-true
        (with-tokenizer "forest and bar"
          (token-list-eq (list (tok)  (tok) (tok)
                               +value+ +and+ +value+)))))

(deftest test-term (search-title-expr-suite)
  (assert-true
      (with-tokenizer "director = foo and note = bar"
        (token-list-eq (list (tok)          (tok)   (tok)   (tok) (tok)      (tok)   (tok)
                             +key-director+ +equal+ +value+ +and+ +key-note+ +equal+ +value+)))))

(deftest test-subexpr (search-title-expr-suite)
  (assert-true
      (with-tokenizer "(director = foo and note = bar) or (tags=class and genres=drama)"
        (token-list-eq (list (tok)  ; (
                             (tok)  ; director
                             (tok)  ; =
                             (tok)  ; foo
                             (tok)  ; and
                             (tok)  ; note
                             (tok)  ; =
                             (tok)  ; bar
                             (tok)  ; )
                             (tok)  ; or
                             (tok)  ; (
                             (tok)  ; tags
                             (tok)  ; =
                             (tok)  ; class
                             (tok)  ; and
                             (tok)  ; genre
                             (tok)  ; =
                             (tok)  ; drama
                             (tok)) ; )
                       +open-expr+ +key-director+ +equal+ +value+ +and+ +key-note+ +equal+ +value+
                       +close-expr+
                       +or+
                       +open-expr+ +key-tags+ +equal+ +value+ +and+ +key-genres+ +equal+ +value+
                       +close-expr+))))

(defmacro compare-code (input output-tree output-sql)
  `(progn
     (assert-true
         (multiple-value-bind (sql tree)
             (parse ,input)
           (declare (ignore sql))
           (tree-equal tree
                       ,output-tree
                       :test #'string=)))
     (assert-true
         (let ((sql (parse ,input)))
           (string= sql ,output-sql)))))

(deftest test-parse-subexpr (search-title-expr-suite)
  (compare-code "director = 1 and (title=4 or note = 5)"
                `("and" ("like" "director" "\"%1%\"")
                        ("or" ("or" ("like" "pt" "\"%4%\"") ("like" "ot" "\"%4%\""))
                              ("like" "notes" "\"%5%\"")))
                (text-utils:strcat " where ( ( director like \"%1%\" ) and "
                                   "( ( ( pt like \"%4%\" ) or "
                                   "( ot like \"%4%\" ) ) or ( notes like \"%5%\" ) ) )")))
