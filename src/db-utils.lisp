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

(in-package :db-utils)

(define-constant +db-invalid-id-number+ 0         :test #'=)

(define-constant +characters-trouble-name+ '(#\-) :test #'equalp)

(define-constant +separator-re+             "\\." :test #'equalp)

(define-constant +separator+                  "." :test #'equalp)

(define-constant +directive-no-journaling+      "PRAGMA journal_mode = MEMORY" :test #'string=)

(define-constant +directive-no-sync-os+         "PRAGMA synchronous = OFF"     :test #'string=)

(define-constant +directive-foreign-keys+       "PRAGMA foreign_keys = ON"     :test #'string=)

(define-constant +directive-foreign-keys-off+   "PRAGMA foreign_keys = OFF"    :test #'string=)

(define-constant +sqlite3-db-scheme-table+      :sqlite_master                 :test #'eq)

(define-constant +sqlite3-db-scheme-table-type+ "table"                        :test #'string=)

(define-constant +sqlite3-db-scheme-type+       :type                          :test #'eq)

(define-constant +sqlite3-db-scheme-table-name+ :tbl_name                      :test #'eq)

(defmacro with-disabled-foreign (&body body)
  `(unwind-protect
        (progn
          (query-low-level +directive-foreign-keys-off+)
          ,@body)
     (query-low-level +directive-foreign-keys+)))

(defparameter *connection* nil)

(defun connectedp ()
  *connection*)

(defun close-db ()
  (when (connectedp)
    (sqlite:disconnect *connection*)))

(defgeneric quote-symbol (s))

(defmethod quote-symbol ((s string))
  (if (scan +separator-re+ s)
      (let* ((splitted (split +separator-re+ s))
             (res (flatten (loop for i in splitted collect
                                (format nil "\"~a\"" i)))))
        (join-with-strings res +separator+))
      (if (null (every  #'(lambda (a) (null (find a s)))
                        +characters-trouble-name+))
          (format nil "\"~(~a~)\"" s)
          (format nil "~(~a~)" s))))

(defmethod quote-symbol ((s symbol))
  (quote-symbol (symbol-name s)))

(defun prepare-query (sql)
  #+:print-sql (misc:dbg "compiling ~a~%" sql)
  (sqlite:prepare-statement *connection* sql))

(defun execute-query (prepared-sql &optional (parameters nil))
  #+:print-sql (misc:dbg "parameters: ~a~%" parameters)
  (let* ((columns-name   (mapcar (lambda (a) (make-keyword (string-upcase a)))
                                 (sqlite:statement-column-names prepared-sql))))
    (loop
         for param in parameters
         for i from 1 do
         (sqlite:bind-parameter prepared-sql i param))
    (let ((res (loop while (sqlite:step-statement prepared-sql) collect
                    (loop
                       for i from 0
                       for column-name  in columns-name
                       append
                         (list column-name (sqlite:statement-column-value prepared-sql i))))))
      (sqlite:finalize-statement prepared-sql)
      res)))

(defun fetch-all (executed-query)
  executed-query)

(defun fetch (executed-query)
  (first executed-query))

(defun query-low-level (sql &optional (parameters nil))
  (execute-query (prepare-query sql) parameters))

(defun query (q)
  (multiple-value-bind (sql params)
      (sxql:yield q)
    (query-low-level sql params)))

(defun query->sql (q)
  (sxql:yield q))

(defmacro with-db-transaction (&body body)
  `(sqlite:with-transaction *connection*
     ,@body))

(defmacro do-rows ((row res) table &body body)
  `(let ((,res ,table))
     (loop for ,row from 0 below (length ,res) do ,@body)
     ,res))

(defun prepare-for-sql-like (s)
  (if (not (text-utils:string-empty-p s))
      (format nil "%~a%" s)
      "%"))

(defmacro object-exists-in-db-p (table clause)
  `(fetch (query (select :*
                   (from ,table)
                   (where ,clause)))))

(defmacro object-count-in-db (table clause)
  `(second (fetch (query (select ((:count :*))
                           (from ,table)
                           (where ,clause))))))

(defmacro if-db-nil-else (expr else)
  `(if (not (eq ,expr :nil))
       ,expr
       ,else))

(defun db-nil-p (a)
  (or (null a)
      (eq a :nil)))

(defun db-nil->lisp (a)
  (if (db-nil-p a)
      nil
      a))

(defun count-all (table)
  (getf (first (fetch-all (query (select ((:as (:count :*) :ct))
                                   (from table)))))
        :ct))

(defun db-path ()
  (uiop:unix-namestring (concatenate 'string
                                     (fs:user-data-dir)
                                     "/"
                                     +db-file+)))

(defun init-connection ()
  (when (not (fs:file-exists-p (db-path)))
    (fs:create-file (db-path)))
  (setf *connection* (sqlite:connect (db-path))))

(defmacro with-ready-database ((&key (connect t)) &body body)
  `(let ((sxql:*sql-symbol-conversion* #'db-utils:quote-symbol))
     (when ,connect
       (init-connection))
     (query-low-level +directive-no-journaling+)
     (query-low-level +directive-no-sync-os+)
     (query-low-level +directive-foreign-keys+)
     (db:build-all-tables)
     (progn ,@body)))

(defun local-time-obj-now ()
  (local-time:now))

; db -> application
(defun encode-datetime-string (d &optional (fallback nil))
  (handler-case
      (local-time:parse-timestring d)
     (error () fallback)))

;; application -> db
(defgeneric decode-datetime-string (object))

(defmethod decode-datetime-string ((object (eql nil)))
  "")

(defmethod decode-datetime-string ((object local-time:timestamp))
  (local-time:format-rfc3339-timestring nil object))

(defmethod decode-datetime-string ((object string))
  (decode-datetime-string (encode-datetime-string object)))

(defgeneric decode-date-string (object))

(defmethod decode-date-string ((object (eql nil)))
  "")

(defmethod decode-date-string ((object local-time:timestamp))
  (local-time:format-timestring nil object :format '(:year "-" (:month 2) "-"
                                                     (:day 2))))

(defmethod decode-date-string ((object string))
  (decode-date-string (encode-datetime-string object)))

(defgeneric decode-time-string (object))

(defmethod decode-time-string ((object local-time:timestamp))
  (local-time:format-timestring nil object :format '((:hour 2) ":" (:min 2))))

(defmethod decode-time-string ((object string))
  (decode-time-string (encode-datetime-string object)))

(defun encoded-datetime-year (decoded)
  (misc:extract-year-from-timestamp (encode-datetime-string decoded)))

(defmacro make-insert (table-name names values)
  (assert (= (length names) (length values)))
  `(insert-into ,table-name
     (set= ,@(loop
                for name in names
                for value in values append
                  (list name value)))))

(defmacro with-director-db-id ((director-id) &body body)
  `(let ((,director-id (db:director-role-id)))
     ,@body))

(defun get-max-id (table)
  (or (second (fetch (query-low-level (format nil "select max (id) from \"~a\""
                                                  (symbol-name table)))))

      0))

(define-constant +copy-position-separaror+ "," :test #'string=)

;; application -> db
(defun decode-copy-position (pos)
  (let ((splitted (split +copy-position-separaror+ pos)))
    (values (elt splitted 0)    ; building
            (elt splitted 1)    ; room
            (elt splitted 2)    ; storage
            (elt splitted 3)))) ; shelf

(defun encode-copy-position (building room storage shelf)
  (join-with-strings* +copy-position-separaror+ building room storage shelf))

(defun decode-blob (blob)
  (and blob (nodgui.base64:encode blob)))

(defun rows->tsv (rows)
  (with-output-to-string (stream)
    (labels ((%escape (s)
               (regex-replace-all "\"" s "\"\""))
             (%fmt (tpl &rest args)
             (apply #'format
                    stream
                    (strcat tpl (coerce '(#\return #\linefeed) 'string))
                    args))
             (%join (s)
               (join-with-strings s (string #\tab)))
             (%wrap (s)
               (wrap-with (%escape (to-s s)) "\""))
             (%filter-print (filter-fn row)
               (%join (mapcar #'%wrap
                              (remove-if-not filter-fn row))))
             (%filter-header (a)
               (and (symbolp a)
                    (not (eq :nil a))))
             (%filter-data (a)
               (cond
                 ((null a)
                  t)
                 ((and (symbolp a)
                       (not (eq :nil a)))
                   nil)
                 (t t))))
      (%fmt (%filter-print #'%filter-header (first-elt rows)))
      (loop for row in rows do
           (%fmt (%filter-print #'%filter-data row)
                 :stream stream)))))

;; application -> db
(defun decode-vote (v)
  (* +maximum-vote+ v))

;; db -> application
(defun encode-vote (v)
  (if (or (null v)
          (not (numberp v)))
      0.0
      (alexandria:clamp (/ v +maximum-vote+) 0.0 1.0)))

(defun table-exists-p (table-name)
  (fetch (query (select :*
                  (from +sqlite3-db-scheme-table+)
                  (where (:and (:= +sqlite3-db-scheme-table-name+ (quote-symbol table-name))
                               (:= +sqlite3-db-scheme-type+ +sqlite3-db-scheme-table-type+)))))))
