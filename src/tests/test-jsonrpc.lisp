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

(in-package :test-jsonrpc)

(defsuite jsonrpc-suite (all-suite))

(defun dummy-update (&rest params)
  (mapcar #'1+ params))

(defmacro prepare-rpc (&body body)
  `(let ((*function-db* '()))
     (register-function "add"          '+            (list (cons "a" 0) (cons "b" 1)))
     (register-function "subtract"     '-            (list (cons "subtrahend" 1)
                                                         (cons "minuend"    0)))
     (register-function "update"       'dummy-update (list (cons "values0" 0)
                                                         (cons "values1" 1)
                                                         (cons "values2" 2)
                                                         (cons "values3" 3)
                                                         (cons "values4" 4)))
     (register-function "notify_sum"   '+            (list (cons "values0" 0)
                                                         (cons "values1" 1)
                                                         (cons "values2" 2)))
     (register-function "notify_hello" '+            (list (cons "values0" 0)))
     ,@body))

(deftest test-simple (jsonrpc-suite)
  (prepare-rpc
    (assert-true
        (string= (jsonify (elaborate-request (jsonify (make-request "add"
                                                                    1
                                                                    '("b" . 10)
                                                                    '("a" . 20)))))
                 "{\"jsonrpc\":\"2.0\",\"result\":30,\"id\":1}"))))

(defun transaction-test (req expected-req expected-response &optional (show-json-p nil))
  (prepare-rpc
    (let* ((json-req  (jsonify req))
           (json-resp (jsonify (elaborate-request json-req))))
      (when show-json-p
        (format t "~%~a~%~a~%" json-req json-resp))
      (assert-true (string= json-req  expected-req))
      (assert-true (string= json-resp expected-response)))))

(deftest test-sub-positional (jsonrpc-suite)
  (transaction-test (make-request "subtract" 1 42 23)
                   "{\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23],\"id\":1}"
                   "{\"jsonrpc\":\"2.0\",\"result\":19,\"id\":1}"))

(deftest test-sub-positional-2 (jsonrpc-suite)
  (transaction-test (make-request "subtract" 2 23 42)
                    "{\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[23,42],\"id\":2}"
                    "{\"jsonrpc\":\"2.0\",\"result\":-19,\"id\":2}"))

(deftest test-sub-named (jsonrpc-suite)
  (transaction-test (make-request "subtract" 3 (cons "subtrahend" 23)
                                               (cons "minuend"    42))
                    (strcat "{\"jsonrpc\":\"2.0\",\"method\":\"subtract\","
                            "\"params\":{\"subtrahend\":23,\"minuend\":42},\"id\":3}")
                    "{\"jsonrpc\":\"2.0\",\"result\":19,\"id\":3}"))

(deftest test-sub-named-2 (jsonrpc-suite)
  (transaction-test (make-request "subtract" 4
                                  (cons "minuend"    42)
                                  (cons "subtrahend" 23))
                    (strcat "{\"jsonrpc\":\"2.0\",\"method\":\"subtract\","
                            "\"params\":{\"minuend\":42,\"subtrahend\":23},\"id\":4}")
                    "{\"jsonrpc\":\"2.0\",\"result\":19,\"id\":4}"))

(deftest test-notifications (jsonrpc-suite)
  (transaction-test (make-notification* "update" '(1 2 3 4 5))
                    "{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2,3,4,5]}"
                    nil))

(deftest test-non-existent-method (jsonrpc-suite)
  (transaction-test (make-request "foobar" 1)
                    "{\"jsonrpc\":\"2.0\",\"method\":\"foobar\",\"id\":1}"
                    (strcat "{\"jsonrpc\":\"2.0\","
                            "\"error\":{\"code\":-32601,"
                            "\"message\":\"Method not found: \\\"foobar\\\"\"},\"id\":1}")))

(deftest test-invalid-json (jsonrpc-suite)
  (let* ((json-req  "{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]")
         (json-resp (jsonify (elaborate-request json-req))))
    (assert-true
        (string= json-resp
                 (strcat "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,"
                         "\"message\":\"Parse error\"},\"id\":null}")))))

(deftest test-invalid-request (jsonrpc-suite)
  (let* ((json-req  "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}")
         (json-resp (jsonify (elaborate-request json-req))))
    (assert-true
        (string= json-resp
                 (strcat "{\"jsonrpc\":\"2.0\",\"error\":"
                         "{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":null}")))))

(deftest test-batch (jsonrpc-suite)
  (prepare-rpc
    (assert-true
        (let ((req (make-batch (make-request "add"
                                             2
                                             '("b" . 10)
                                             '("a" . 20))
                               (make-request "foo" 1 1 2)
                               (make-request "add" 1 1 2))))
          (string= (jsonify (elaborate-request (jsonify req)))
                   (strcat "["
                           "{\"jsonrpc\":\"2.0\",\"result\":30,\"id\":2},"
                           "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32601,"
                           "\"message\":\"Method not found: \\\"foo\\\"\"},\"id\":1},"
                           "{\"jsonrpc\":\"2.0\",\"result\":3,\"id\":1}"
                           "]"))))))

(deftest test-batch-json-invalid (jsonrpc-suite)
  (let* ((json-req  (strcat "["
                            "{\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\":"
                            "[1,2,4], \"id\": \"1\"},"
                            "{\"jsonrpc\": \"2.0\", \"method\""
                            "]"))
         (json-resp (jsonify (elaborate-request json-req))))
    (assert-true
        (string= json-resp
                 (strcat "{\"jsonrpc\":\"2.0\",\"error\":"
                         "{\"code\":-32700,\"message\":\"Parse error\"},\"id\":null}")))))

(deftest test-batch-empty-array (jsonrpc-suite)
  (let* ((json-req "[]")
         (json-resp (jsonify (elaborate-request json-req))))
    (assert-true
        (string= json-resp
                 (strcat "{\"jsonrpc\":\"2.0\",\"error\":"
                         "{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":null}")))))

(deftest test-batch-invalid (jsonrpc-suite)
  (let* ((json-req "[1]")
         (json-resp (jsonify (elaborate-request json-req))))
    (assert-true
        (string= json-resp
                 (strcat "["
                         "{\"jsonrpc\":\"2.0\",\"error\":"
                         "{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":null}"
                         "]")))))

(deftest test-batch-invalid-2 (jsonrpc-suite)
  (let* ((json-req "[1, 2, 3]")
         (json-resp (jsonify (elaborate-request json-req))))
    (assert-true
        (string= json-resp
                 (strcat "["
                         "{\"jsonrpc\":\"2.0\",\"error\":"
                         "{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":null},"
                         "{\"jsonrpc\":\"2.0\",\"error\":"
                         "{\"code\":-32600,\"message\":\"Invalid Request\"},\"id\":null},"
                         "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":"
                         "-32600,\"message\":\"Invalid Request\"},\"id\":null}"
                         "]")))))

(deftest test-batch-notification (jsonrpc-suite)
  (transaction-test (make-batch (make-notification "notify_sum" 1 2 4)
                                (make-notification "notify_hello" 7))
                    (strcat "["
                            "{\"jsonrpc\":\"2.0\",\"method\":\"notify_sum\",\"params\":[1,2,4]},"
                            "{\"jsonrpc\":\"2.0\",\"method\":\"notify_hello\",\"params\":[7]}"
                            "]")
                    nil))
