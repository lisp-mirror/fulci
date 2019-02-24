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

(in-package :wikipedia)

(define-constant +api-entry-point+         "w/api.php?action=query" :test #'string=)

(define-constant +rest-api-entry-point+    "api/rest_v1"            :test #'string=)

(define-constant +api-scheme+              "https://"               :test #'string=)

(define-constant +api-fields-separator+    "&"                      :test #'string=)

(define-constant +api-field-key-separator+ "="                      :test #'string=)

(define-constant +api-format-key+          "format"                 :test #'string=)

(define-constant +api-format-value+        "json"                   :test #'string=)

(define-constant +api-search-title-tmp+    '(("list"     . "search")
                                             ("srsearch" . placeholder))
  :test #'equalp)

(define-constant +api-search-principal-image+ '(("prop"    . "pageimages")
                                                ("pageids" . placeholder)
                                                ("piprop"  . "original")
                                                ("pilicense" . "any"))
  :test #'equalp)

(define-constant +acceptable-img-mime-types+ (list +mime-type-jpg+
                                                   +mime-type-png+)
  :test #'equalp)

(define-constant +rest-api-mobile-html+    "page/mobile-html"        :test #'string=)

(defparameter *wikipedia-host* "en.wikipedia.org")

(defun trivial-tpl-subst (tpl var)
  (let ((substitution-happended-p nil))
    (loop for i in tpl collect
         (cond
           (substitution-happended-p
            i)
           ((symbolp (cdr i))
            (setf substitution-happended-p t)
            (cons (car i) var))
           (t
            i)))))

(defun tpl-subst* (tpl vars)
  (if (null vars)
      tpl
      (tpl-subst* (trivial-tpl-subst tpl (first vars))
                  (rest vars))))

(defun tpl-subst (tpl &rest vars)
  (tpl-subst* tpl vars))

(defun param-cons-format ()
  (cons +api-format-key+
        +api-format-value+))

(defun param-cons-key (c)
  (car c))

(defun param-cons-value (c)
  (cdr c))

(defun urlize-query (params-cons &key (append-format-p t))
  (let ((actual-params-cons (append  params-cons
                                     (if append-format-p
                                         (list (param-cons-format))
                                         nil))))
    (join-with-strings (loop for param-cons in actual-params-cons collect
                            (join-with-strings* +api-field-key-separator+
                                                (param-cons-key param-cons)
                                                (param-cons-value param-cons)))
                       +api-fields-separator+)))

(defun fetch-page (query)
  (let ((uri (strcat +api-scheme+
                     *wikipedia-host* "/"
                     (join-with-strings* +api-fields-separator+
                                         +api-entry-point+
                                         query))))
    (multiple-value-bind (body status-code)
        (drakma:http-request uri
                             :want-stream    nil
                             :decode-content nil
                             :verify         :required
                             :url-encoder    (lambda (a b) (declare (ignore b)) a))
      (if (= +http-code-ok+ status-code)
          (if (vectorp body)
              (babel:octets-to-string body)
              (error 'text-error "response from wikipedia was not a vector!"))
          (error 'text-error (format nil "Response code not ok but ~a" status-code))))))

(defun url-encode (s)
  (net-utils:percent-encode s drakma:*drakma-default-external-format*))

(defmacro with-wiki-query ((serialized-response query-template query) &body body)
  (with-gensyms (unserialized)
    `(json:with-decoder-simple-clos-semantics
       (let ((json:*json-symbols-package* :wikipedia))
         (when-let* ((,unserialized       (fetch-page
                                           (urlize-query (tpl-subst ,query-template
                                                                    (url-encode ,query)))))
                     (,serialized-response (json:decode-json-from-string ,unserialized)))
           ,@body)))))

(defun search-wiki-pages (query)
  (with-wiki-query (serialized-response +api-search-title-tmp+ query)
    (when-let* ((res-query   (slot-value serialized-response 'query))
                (res-search  (slot-value res-query  'search)))
      (map 'list #'(lambda (a)
                     (cons (slot-value a 'pageid)
                           (slot-value a 'title)))
           res-search))))

(defun search-wiki-image (page-id)
  (with-wiki-query (serialized-response +api-search-principal-image+ (to-s page-id))
    (when-let* ((page-id-symbol (alexandria:format-symbol :wikipedia "~a" (to-s page-id)))
                (query-res      (slot-value serialized-response 'query))
                (pages          (slot-value query-res           'pages))
                (page           (slot-value pages               page-id-symbol))
                (img-info       (slot-value page                'original))
                (img-url        (slot-value img-info            'source)))
      (image-from-url img-url))))

(defstruct movie-entry
  (title)
  (director)
  (year)
  (runtime))

(defun valid-lquery-res-p (lquery-res)
  (if (vector-empty-p lquery-res)
      nil
      (string-trim '(#\Space #\Tab #\newline) (first-elt lquery-res))))

(defun valid-or-nil (lquery-res)
  (valid-lquery-res-p lquery-res))

(define-constant +infobox-selector+       "table[class^=infobox]" :test #'string=)

(define-constant +infobox-title-selector+    "th[class^=summary]" :test #'string=)

(define-constant +infobox-director-re+             "(?i)directed" :test #'string=)

(define-constant +infobox-runtime-re+         "(?i)running +time" :test #'string=)

(define-constant +infobox-release-re+         "(?i)release +date" :test #'string=)

(defun filter-infobox (node key)
  (let* ((all-th     (lquery:$ node "th"))
         (good-child (remove-if-not #'(lambda (a)
                                        (let ((res (lquery:$ a (text))))
                                          (scan key
                                                (valid-or-nil res))))
                                    all-th)))
    (valid-or-nil (lquery:$ good-child
                            (parent)
                            "td"
                            (text)))))

(defun find-director (node)
  (filter-infobox node +infobox-director-re+))

(defun find-runtime (node)
  (filter-infobox node +infobox-runtime-re+))

(defun find-release-year (node)
  (let ((raw (filter-infobox node +infobox-release-re+)))
    (if raw
        (when-let* ((words      (split "\\s" raw))
                    (maybe-year (remove-if-not (lambda (a)
                                                 (and (safe-parse-integer a)
                                                      (>= (safe-parse-integer a)
                                                          1895)))
                                               words))
                    (year (safe-parse-integer (first-elt maybe-year))))
          (year->timestamp year))
        nil)))

(defun split-directors-names (names)
  (split "[\\n,\\r]+" names))

(defun get-movie-info (page-title)
  (let* ((uri (strcat +api-scheme+
                      *wikipedia-host*    "/"
                      (join-with-strings* "/"
                                          +rest-api-entry-point+
                                          +rest-api-mobile-html+
                                          (url-encode page-title)))))
    (multiple-value-bind (html status-code headers)
        (drakma:http-request uri :verify :required)
      (with-success-request (status-code headers)
        (when-let* ((doc           (lquery:load-page html))
                    (res           (make-movie-entry))
                    (title         (valid-lquery-res-p (lquery:$ doc
                                                                 +infobox-selector+
                                                                 +infobox-title-selector+
                                                                 (text))))
                    (raw-directors (find-director (lquery:$ doc
                                                            +infobox-selector+
                                                            "tr")))
                    (runtime       (find-runtime (lquery:$ doc
                                                           +infobox-selector+
                                                           "tr")))
                    (release-year  (find-release-year (lquery:$ doc
                                                                +infobox-selector+
                                                                "tr"))))
          (setf (movie-entry-title    res) title)
          (setf (movie-entry-director res) (split-directors-names raw-directors))
          (setf (movie-entry-runtime  res) (safe-parse-integer runtime))
          (setf (movie-entry-year     res) release-year)
          res)))))
