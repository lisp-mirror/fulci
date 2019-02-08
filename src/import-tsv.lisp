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

(in-package :import-tsv)

(define-constant +genre-separator+       "," :test #'string=)

(define-constant +subcolumn-separator+   "," :test #'string=)

(define-constant +field-separator+     #\Tab :test #'string=)

(define-constant +field-n/a-value+     "\\N" :test #'string=)

(defparameter *line-ct* 0)

(defun n/a-field-p (f)
  (string= f +field-n/a-value+))

(defun import-file (file fn)
  (read-csv (fs:namestring->pathname file)
            :separator   +field-separator+
            :skip-first-p t
            :row-fn       fn
            :quote        nil))

(defun split-genres (a)
  (split +genre-separator+ a))

#+debug-csv
(defun print-count (max)
  (when (= (rem *line-ct* 1000) 0)
    (misc:dbg "ct: ~a ~f" *line-ct* (/ *line-ct* max))))

(defmacro with-line-count ((line-total-count file) &body body)
  `(let ((*line-ct*  0)
         (,line-total-count (fs:count-lines-in-file ,file)))
     ,@body))

(defmacro lambda-row ((total-lines progress-fn args) &body body)
  `(lambda ,args
     (incf *line-ct*)
     #+debug-csv (print-count ,total-lines)
     (funcall ,progress-fn (coerce (/ *line-ct* ,total-lines) 'single-float))
     ,@body))

(defun import-genres (file &key (progress-fn (lambda (a) (declare (ignore a)))))
  (with-line-count (total-lines file)
    (let ((all-genres '()))
      (with-db-transaction
        (import-file file
                     (lambda-row (total-lines progress-fn (row))
                       ;; F1 F2 Fn ... genre1,genre2,genre2
                       (let ((genres-field (last-elt row)))
                         (loop
                            for genre in (split-genres genres-field)
                            when (not (n/a-field-p genre))
                            do
                              (pushnew genre all-genres :test #'string=)))))
        (flet ((do-update (genre)
                 (when (not (object-exists-in-db-p db:+table-genre+ (:= :description genre)))
                   (query (insert-into db:+table-genre+
                            (set= :description genre))))))
          (loop for genre in all-genres do
               (do-update genre)))))))

(defun decode-year (year)
  (if (n/a-field-p year)
      ""
      (format nil "~a" (year->timestamp (parse-integer year)))))

(defun filter-whitelist (column-value whitelist)
  (loop for re in whitelist do
       (loop for sample in (split +subcolumn-separator+ column-value) do
            (when (scan re sample)
              (return-from filter-whitelist t))))
  nil)

(defun homonym-name (a)
  (elt a 0))

(defun homonym-year (a)
  (elt a 1))

(defun homonym-count (a)
  (elt a 2))

(defun set-count (a v)
  (setf (elt a 2) v))

(defsetf homonym-count set-count)

(defun cons-homonym (name year &optional (count 1))
  (list name year count))

(defun person-row-name (row)
  (elt row 1))

(defun person-row-year (row)
  (elt row 2))

(defun person-row-professions (row)
  (elt row 4))

(defun homonym-p (name birthday)
  (lambda (p)
    (and (string-equal (homonym-name p) name)
         (string-equal (homonym-year p) birthday))))

(defun homonym-tree-equal-fn (a b)
  (and (string-equal (homonym-name a)
                     (homonym-name b))
       (string-equal (homonym-year a)
                     (homonym-year b))))

(defun homonym-tree< (a b)
  (cond
    ((string-equal (homonym-name a)
                   (homonym-name b))
     (< (safe-parse-integer (homonym-year a) :fix-fn (lambda (a) (declare (ignore a)) 0))
        (safe-parse-integer (homonym-year b) :fix-fn (lambda (a) (declare (ignore a)) 0))))
    (t
     (string-lessp (homonym-name a)
                   (homonym-name b)))))

(defun homonym-key (a)
  (identity a))

(defun homonym-search (tree name birthday)
  (bs-tree:search tree
                  (cons-homonym name birthday)
                  :equal     #'homonym-tree-equal-fn
                  :compare   #'homonym-tree<
                  :key-datum #'homonym-key
                  :key       #'homonym-key))

(defun homonym-insert (tree name birthday count)
  (rb-tree:insert tree
                  (cons-homonym name birthday count)
                  :equal     #'homonym-tree-equal-fn
                  :compare   #'homonym-tree<
                  :key-datum #'homonym-key
                  :key       #'homonym-key))

(defun homonyms-persons (file allowed-professions progress-fn)
  (let ((tree (rb-tree:make-root-rb-node nil rb-tree:+rb-red+)))
    (with-line-count (total-lines file)
      (import-file file
                   (lambda-row (total-lines progress-fn (row))
                     (when (filter-whitelist (person-row-professions row) allowed-professions)
                       (let* ((name     (person-row-name row))
                              (birthday (person-row-year row))
                              (found    (homonym-search tree name birthday)))
                         (cond
                           (found
                            (incf (homonym-count (rb-tree:data found))))
                           (t
                            (setf tree (homonym-insert tree name birthday 1))))))))
      tree)))

(defun checking-other-id (table id)
  (object-exists-in-db-p table
                         (:= :other-id id)))

(defun import-persons (file &key
                              (allowed-professions '("(?i)director"))
                              (progress-fn          (lambda (a) (declare (ignore a)))))
  (let ((all-names (homonyms-persons file allowed-professions progress-fn)))
    (with-line-count (total-lines file)
      (with-db-transaction
        (import-file file
                     (lambda-row (total-lines progress-fn (row))
                       ;; id name year ...
                       (let* ((id          (elt row 0))
                              (name        (person-row-name row))
                              (year        (person-row-year row))
                              (birthday    (decode-year year))
                              (professions (person-row-professions row)))
                         (labels ((resolve-name (count)
                                    (db:normalize-name count name year))
                                  (do-update ()
                                    (when (not (checking-other-id db:+table-person+
                                                                  id))
                                      (let* ((homonym  (rb-tree:data (homonym-search all-names
                                                                                     name
                                                                                     year)))
                                             (new-name (resolve-name (homonym-count homonym))))
                                        (when (not (object-exists-in-db-p db:+table-person+
                                                                          (:= :primary-name
                                                                              new-name)))
                                          (decf (homonym-count homonym))
                                          (query (make-insert db:+table-person+
                                                              (:other-id
                                                               :primary-name
                                                               :birthday)
                                                              (id
                                                               new-name
                                                               birthday))))))))
                           (when (filter-whitelist professions allowed-professions)
                             (do-update))))))))))

(defun import-titles (file &key
                             (min-year 1900)
                             (max-year (local-time:timestamp-year (db-utils:local-time-obj-now)))
                             (allowed-type  '("(?i)movie")) ; maybe "(?i)tvSeries" too?
                             (min-duration  90)             ; in minutes
                             (progress-fn (lambda (a) (declare (ignore a)))))
  (with-line-count (total-lines file)
    (with-db-transaction
      (import-file file
                   (lambda-row (total-lines progress-fn (row))
                     ;; id name year ...
                     (let* ((id             (elt row 0))
                            (type           (elt row 1))
                            (primary-title  (elt row 2))
                            (original-title (elt row 3))
                            (raw-year       (elt row 5))
                            (year-db        (decode-year raw-year))
                            (year-encoded   (encode-datetime-string year-db))
                            (runtime      (if (n/a-field-p (elt row 7))
                                              ""
                                              (parse-integer (elt row 7)))))
                       (flet ((do-update ()
                                (when (not (checking-other-id db:+table-title+
                                                              id))
                                  (query (make-insert db:+table-title+
                                                      (:other-id
                                                       :primary-title
                                                       :original-title
                                                       :year
                                                       :runtime)
                                                      (id
                                                       primary-title
                                                       original-title
                                                       year-db
                                                       runtime))))))
                         (when (and year-encoded
                                    (<= min-year
                                        (local-time:timestamp-year year-encoded)
                                        max-year)
                                    (not (stringp runtime))
                                    (>= runtime min-duration)
                                    (filter-whitelist type allowed-type))
                           (do-update)))))))))

(defun link-titles-genres (file &key (progress-fn (lambda (a) (declare (ignore a)))))
  (with-line-count (total-lines file)
    (with-db-transaction
      (import-file file
                   (lambda-row (total-lines progress-fn (row))
                     ;; id name year ...
                     (let* ((raw-genres (last-elt row))
                            (genres     (if (n/a-field-p raw-genres)
                                            nil
                                            (split-genres raw-genres)))
                            (other-id  (first-elt row))
                            (genres-db (db:all-genres)))
                       (labels ((link-genre-exists-p (title-id genre-id)
                                  (object-exists-in-db-p db:+table-title-genre+
                                                         (:and (:= :title title-id)
                                                               (:= :genre genre-id))))
                                (do-update ()
                                  (when (checking-other-id db:+table-title+
                                                           other-id)
                                    (let* ((title    (db:fetch-title other-id))
                                           (title-id (getf title :id)))
                                      (loop for genre in genres-db do
                                           (let ((genre-id          (getf genre :id))
                                                 (genre-description (getf genre :description)))
                                             (when (and (find genre-description genres
                                                              :test #'string=)
                                                        (not (link-genre-exists-p title-id
                                                                                  genre-id)))
                                               (query (make-insert db:+table-title-genre+
                                                                   (:title
                                                                    :genre)
                                                                   (title-id
                                                                    genre-id))))))))))
                         (do-update))))))))

(defun link-crew (file &key (progress-fn (lambda (a) (declare (ignore a)))))
  (with-line-count (total-lines file)
    (let ((director-id (db:director-role-id)))
      (with-db-transaction
        (import-file file
                     (lambda-row (total-lines progress-fn (row))
                       ;; id name year ...
                       (let* ((title-other-id    (elt row 0))
                              (director-other-id (elt row 1)))
                         (labels ((link-crew-exists-p (title-id person-id)
                                    (object-exists-in-db-p db:+table-crew+
                                                           (:and (:= :title  title-id)
                                                                 (:= :person person-id)
                                                                 (:= :role   director-id))))
                                  (do-update ()
                                    (when (checking-other-id db:+table-title+
                                                             title-other-id)
                                      (when-let* ((title       (db:fetch-title title-other-id))
                                                  (title-id    (getf title :id))
                                                  (person      (db:fetch-person director-other-id))
                                                  (person-id   (getf person :id)))
                                        (when (not (link-crew-exists-p title-id
                                                                       person-id))
                                          (query (make-insert db:+table-crew+
                                                              (:title
                                                               :person
                                                               :role)
                                                              (title-id
                                                               person-id
                                                               director-id))))))))
                           (do-update)))))))))

(defun import-country ()
  (with-db-transaction
      (import-file +country-csv-file+
                   (lambda (row)
                     (let* ((name   (elt row 1))
                            (abbrev (elt row 2)))
                       (query (make-insert db:+table-country+
                                           (:description
                                            :abbreviation)
                                           (name
                                            abbrev))))))))
