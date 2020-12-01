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

(in-package :json-rpc2)

(define-constant +protocol-version+               "2.0" :test #'string=)

(define-constant +key-name+                   "jsonrpc" :test #'string=)

(define-constant +key-method+                  "method" :test #'string=)

(define-constant +key-params+                  "params" :test #'string=)

(define-constant +key-id+                          "id" :test #'string=)

(define-constant +key-error+                    "error" :test #'string=)

(define-constant +key-error-code+                "code" :test #'string=)

(define-constant +key-error-message+          "message" :test #'string=)

(define-constant +key-result+                 "result" :test #'string=)

(define-constant +error-reserved-min+            -32099 :test #'=)

(define-constant +error-reserved-max+            -32000 :test #'=)

(define-constant +error-reserved-method-name+ "^rpc\\." :test #'string=)

(defun invalid-method-name-p (n)
  (cl-ppcre:scan +error-reserved-method-name+ n))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-response-error (code message)
    (list (cons +key-error-code+    code)
          (cons +key-error-message+ message)))

  (defun response-error-code (err)
    (cdar err))

  (defun response-error-message (err)
    (cdadr err)))

(defmacro define-error-code (name number message &key (customp nil))
  (if (and customp
           (not (<= +error-reserved-min+ number +error-reserved-max+)))
      (error (format nil "implementation defined error bust be between ~a  and ~a"
                     +error-reserved-min+ +error-reserved-max+))
      `(define-constant ,(misc:format-fn-symbol t "+error-~a+" name)
           (make-response-error ,number ,message)
         :test #'equalp)))

(define-error-code parse                -32700  "Parse error")

(define-error-code invalid-request      -32600  "Invalid Request")

(define-error-code method-not-found     -32601  "Method not found")

(define-error-code invalid-params       -32602  "Invalid params")

(define-error-code internal-error       -32603  "Internal error")

(define-error-code unsupported-protocol -32098  "Only protocol version 2.0 is supported"
                   :customp t)

(defclass function-param ()
  ((name
    :initarg  :name
    :initform ""
    :accessor name
    :type     string)
   (pos
    :initarg  :pos
    :initform -1
    :accessor pos
    :type     integer)))

(defmethod print-object ((object function-param) stream)
  (format stream "[~s @ ~a]" (name object) (pos object)))

(defparameter *request-id*  1)

(defun generate-request-id ()
  (incf *request-id*)
  *request-id*)

(define-condition json-rpc-error (error)
  ((transaction-id
    :initform nil
    :initarg :transaction-id
    :reader  transaction-id)
   (code
    :initarg :code
    :reader  code)
   (text
    :initarg :text
    :reader text))
  (:report (lambda (condition stream)
             (format stream "~a" (text condition))))
  (:documentation "Error for all jsonrpc related problems"))

(defparameter *function-db* '())

(defun make-fun-params (name position)
  (cond
    ((not (stringp name))
     (error 'json-rpc-error
            :text (format nil "Name of a function parameter must be a string not ~a" name)))
    ((or (not (integerp position))
         (<   position 0))
     (error 'json-rpc-error
            :text (format nil
                          "The position of a function parameter must be positive integer not ~a"
                          position)))
    (t
     (make-instance 'function-param  :name name :pos position))))

(defun find-function (sym)
  (conditions:with-default-on-error (nil)
    (symbol-function sym)))

(defun register-function (function-id fun &optional params)
  (let* ((actual-params (loop for param in params collect
                             (make-fun-params (car param)
                                              (cdr param))))
         (fun           (make-instance 'rpc-function
                                       :function-id function-id
                                       :fun-symbol  fun
                                       :params      actual-params)))
    (push fun *function-db*)))

(defun compare-functions-fn (function-id)
  (lambda (a)
    (string= function-id (function-id a))))

(defun unregister-function (function-id)
  (setf *function-db* (remove-if (compare-functions-fn function-id) *function-db*)))

(defun lookup-function (function-id)
  (find-if (compare-functions-fn function-id) *function-db*))

(defclass identificable ()
  ((id
    :initarg  :id
    :initform -1
    :accessor id
    :type     integer)))

(defclass fun-symbol-box ()
  ((fun-symbol
    :initarg  :fun-symbol
    :initform nil
    :accessor fun-symbol
    :type     symbol)))

(defmethod initialize-instance :after ((object fun-symbol-box) &key &allow-other-keys)
  (with-accessors ((fun-symbol fun-symbol)) object
    (cond
      ((not (symbolp fun-symbol))
       (error 'json-rpc-error
              :text (format nil "function symbol of a function be a symbol not ~a"
                            fun-symbol)))
      ((not (find-function fun-symbol))
       (error 'json-rpc-error
              :text (format nil "~a is not a function" fun-symbol))))))

(defclass fun-box ()
  ((function-id
    :initarg  :function-id
    :initform ""
    :accessor function-id
    :type     string)
   (params
    :initarg  :params
    :initform '()
    :accessor params
    :type     list)))

(defmethod initialize-instance :after ((object fun-box) &key &allow-other-keys)
  (with-accessors ((function-id function-id)) object
    (cond
      ((not (stringp function-id))
       (error 'json-rpc-error
              :text (format nil "ID of a function must be a string not ~a" function-id)))
      ((invalid-method-name-p function-id)
       (error 'json-rpc-error
              :text (format nil
                            "function ID invalid, starts with a reserved prefix ~s"
                            +error-reserved-method-name+))))))

(defmethod print-object ((object fun-box) stream)
  (format stream "function-id: ~s params ~s" (function-id object) (params object)))

(defclass rpc-function (fun-symbol-box fun-box) ())

(defmethod print-object ((object rpc-function) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "symbol ~s " (fun-symbol object))
    (call-next-method)))

(defclass rpc-request (fun-box identificable) ())

(defclass rpc-request-batch ()
  ((requests
    :initform '()
    :initarg  :requests
    :accessor requests)))

(defun make-batch (&rest requests)
  (make-instance 'rpc-request-batch :requests requests))

(defun %make-request (method id  &rest params)
  (let ((req (make-instance 'rpc-request
                            :function-id method
                            :id          id
                            :params      params)))
    req))

(defun make-request (method id &rest params)
  (apply #'%make-request method id params))

(defun make-request* (method id param)
  (if (typep param 'proper-list)
      (apply #'%make-request method id param)
      (apply #'%make-request method id (list param))))

(defun make-notification (method &rest params)
  (apply #'%make-request method nil params))

(defun make-notification* (method params)
  (apply #'%make-request method nil params))

(defgeneric jsonify (object))

(defmethod jsonify (object)
  (encode-json-to-string (render-flat object)))

(defmethod jsonify ((object (eql nil)))
  nil)

(defgeneric render-flat (object))

(defmethod render-flat (object)
  object)

(defmethod render-flat ((object rpc-request-batch))
  (loop for i in (requests object) collect (render-flat i)))

(defmethod render-flat ((object rpc-request))
  (with-accessors ((id          id)
                   (function-id function-id)
                   (params      params)) object
    (let ((default  (list (cons +key-name+   +protocol-version+)
                          (cons +key-method+ function-id))))
      (when (not (null (first params)))
        (setf default (append default (list (cons +key-params+ params)))))
      (if id
          (append default
                  (list (cons +key-id+ id)))
          default))))

(defun make-response (results request-id &key (error-object nil))
  (if error-object
      (list (cons +key-name+    +protocol-version+)
            (cons +key-error+   error-object)
            (cons +key-id+      request-id))
      (list (cons +key-name+    +protocol-version+)
            (cons +key-result+  results)
            (cons +key-id+      request-id))))

(defun supported-version-p (v)
  (and v
       (stringp v)
       (string= v +protocol-version+)))

(defun called-by-name-p (fun params)
  (let ((names          (loop for i in params when (consp i) collect (car i)))
        (template-names (loop for i in (params fun) collect (name i))))
    (and names
         (every (lambda (a) (or (symbolp a) (stringp a))) names)
         (null (set-difference names template-names :test #'string-equal)))))

(defun call-function (request)
  (flet ((call-fun (fn params)
           (apply (symbol-function (fun-symbol fn)) params)))
    (let ((fun (lookup-function (function-id request))))
      (cond
        ((not fun)
         (error 'json-rpc-error
                :code           (response-error-code +error-method-not-found+)
                :transaction-id (id request)
                :text           (format nil
                                        "~a: ~s"
                                        (response-error-message +error-method-not-found+)
                                        (function-id request))))
        ((/= (length (params request))
             (length (params fun)))
         (error 'json-rpc-error
                :code (response-error-code +error-invalid-params+)
                :transaction-id (id request)
                :text (format nil
                              "Number of parameters (arity) not compatible with function")))
        (t
         (let* ((params           (params request))
                (called-by-name-p (called-by-name-p fun params)))
           (if called-by-name-p
               (let ((params-list (make-list (length (params fun)))))
                 (loop for p in (params fun) do
                      (let* ((param-name  (name p))
                             (param-pos   (pos p))
                             (param-value (cdr (assoc param-name params
                                                      :test #'string-equal))))
                        (setf (elt params-list param-pos) param-value)))
                 (call-fun fun params-list))
               (call-fun fun params))))))))

(defun displace-single-request (request)
  (flet ((lookup (k)
           (and (consp request)
                (every #'consp request)
                (cdr (assoc k request :test #'string-equal)))))
    (let ((protocol-version (lookup +key-name+))
          (method           (lookup +key-method+))
          (params           (lookup +key-params+))
          (id               (lookup +key-id+)))
      (cond
        ((null protocol-version)
         (error 'json-rpc-error
                :code (response-error-code    +error-invalid-request+)
                :text (response-error-message +error-invalid-request+)))
        ((not (supported-version-p protocol-version))
         (error 'json-rpc-error
                :code (response-error-code    +error-unsupported-protocol+)
                :text (response-error-message +error-unsupported-protocol+)))
        ((not (listp params))
         (error 'json-rpc-error
                :code (response-error-code    +error-invalid-request+)
                :text (response-error-message +error-invalid-request+)))
        (t
         (values method id params))))))

(defun elaborate-single-request (request)
  (handler-case
      (multiple-value-bind (method id params)
          (displace-single-request request)
        (let* ((request      (apply #'make-request method id params))
               (elaborated   (call-function request)))
          (when id
            ;; if id is null is  a notification (i.e. the client
            ;; does not care about an answer)
            (make-response elaborated id :error-object nil))))
    (json-rpc-error (e)
      (make-response nil
                     (transaction-id e)
                     :error-object
                     (make-response-error (or (code e)
                                              (response-error-code +error-invalid-request+))
                                          (text e))))
    (json-syntax-error ()
      (make-response nil nil :error-object +error-parse+))
    (error ()
      (make-response nil nil :error-object +error-internal-error+))))

(defun likely-not-batch-p (request)
  (and (every (lambda (a) (and (consp a)
                               (car   a)
                               (cdr   a)))
              request)
       (assoc +key-name+   request :test #'string-equal)
       (assoc +key-method+ request :test #'string-equal)
       (assoc +key-params+ request :test #'string-equal)))

(defun request-batch-p (request)
  (handler-case
      (progn
        (displace-single-request request)
        nil)
    (json-rpc-error    ()
      (if (likely-not-batch-p request)
          nil
          t))
    (json-syntax-error () nil)
    (error             () t)))

(defun elaborate-request (raw-request)
  (handler-case
      (with-input-from-string (stream raw-request)
        (let ((decoded (decode-json stream)))
          (if (request-batch-p decoded)
              (if (null decoded)
                  (elaborate-single-request decoded) ;; will build an error response
                  (remove-if #'null
                             (mapcar #'elaborate-single-request decoded)))
              (elaborate-single-request decoded))))
    (json-syntax-error ()
      (make-response nil nil :error-object +error-parse+))))
