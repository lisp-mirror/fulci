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

(in-package :db)

(define-constant +make-id+           " id INTEGER PRIMARY KEY AUTOINCREMENT, "
  :test #'string=)

(define-constant +other-id+          :other-id                                          :test #'eq)

(define-constant +make-other-id+     (strcat " "
                                             (quote-symbol +other-id+)
                                             " INTEGER UNIQUE ON CONFLICT FAIL, ")
  :test #'string=)

(define-constant +make-unique-description+          " description TEXT NOT NULL UNIQUE"
  :test #'string=)

(define-constant +make-open+                                                " ( "
  :test #'string=)

(define-constant +make-close+                                              " )  "
  :test #'string=)

(define-constant +restrict+                                          " RESTRICT "
  :test #'string=)

(define-constant +cascade+                                            " CASCADE "
  :test #'string=)

(define-constant +col-sep+                                                  " , "
  :test #'string=)

(define-constant +table-country+                                           :country     :test #'eq)

(define-constant +table-genre+                                             :genre       :test #'eq)

(define-constant +table-movie-storage-format+               :movie-storage-format       :test #'eq)

(define-constant +table-title+                                             :title       :test #'eq)

(define-constant +table-role+                                               :role       :test #'eq)

(define-constant +table-person+                                           :person       :test #'eq)

(define-constant +table-movie-copy+                                   :movie-copy       :test #'eq)

(define-constant +table-crew+                                               :crew       :test #'eq)

(define-constant +table-title-genre+                                  :title-genre      :test #'eq)

(define-constant +table-title-country+                                :title-country    :test #'eq)

(define-constant +view-titles-genres-directors+                       :titles-ge-dir    :test #'eq)

(define-constant +view-copies-genres-directors+                       :copies-ge-dir    :test #'eq)

(define-constant +value-director+                                       "Director"
  :test #'string=)

(define-constant +value-genre-none+                                            "-"
  :test #'string=)

(define-constant +value-countries-none+                                  "unknown"
  :test #'string=)

(define-constant +search-expr-director-col+                           :director         :test #'eq)

(define-constant +search-expr-primary-title-col+                      :pt               :test #'eq)

(define-constant +search-expr-original-title-col+                     :ot               :test #'eq)

(define-constant +search-expr-year-col+                               :year             :test #'eq)

(define-constant +search-expr-notes-col+                              :notes            :test #'eq)

(define-constant +search-expr-genres-col+                             :genres           :test #'eq)

;; currently not used in expression
(define-constant +search-expr-format-col+                             :format           :test #'eq)

(define-constant +search-expr-tags-col+                               :tags             :test #'eq)

(define-constant +search-expr-building-col+                           :building         :test #'eq)

(define-constant +search-expr-room-col+                               :room             :test #'eq)

(define-constant +search-expr-storage-col+                            :storage          :test #'eq)

(define-constant +search-expr-shelf-col+                              :shelf            :test #'eq)

(define-constant +search-expr-country-col+              :country-description          :test #'eq)

(defun create-table-index (table-name &optional (columns '(:id)))
  (labels ((%replace (s chars)
           (if (null chars)
               s
               (%replace (regex-replace-all (string (first chars)) s "_")
                         (rest chars)))))
    (let ((actual-table-name (if (symbolp table-name)
                                 (symbol-name table-name)
                                 (string-downcase table-name))))
      (query-low-level
       (format nil
               "CREATE UNIQUE INDEX IF NOT EXISTS index_~a on ~a (~{~a~^, ~})"
               (%replace actual-table-name +characters-trouble-name+)
               (quote-symbol table-name)
               (mapcar #'quote-symbol columns))))))

(defun delete-table (table-name)
  (query-low-level (format nil "DROP TABLE IF EXISTS ~a" (quote-symbol table-name))))

(defun delete-view (view-name)
  (query-low-level (format nil "DROP VIEW ~a" (quote-symbol view-name))))

(defun prepare-table (name)
  (strcat "create table "
          (quote-symbol name)
          +make-open+
          +make-id+))

(defun make-table-descriptive (name)
  (query-low-level (strcat (prepare-table name)
                           +make-unique-description+
                           +make-close+)))

(defun prepare-table-multiple-id (name)
  (strcat (prepare-table name)
          +make-other-id+))

(defun make-movie-genre ()
  (make-table-descriptive +table-genre+))

(defun make-country ()
  (query-low-level (strcat (prepare-table +table-country+)
                           +make-unique-description+ +col-sep+
                           " abbreviation  TEXT UNIQUE ON CONFLICT FAIL "
                           +make-close+)))

(defun make-movie-storage-format ()
  (make-table-descriptive +table-movie-storage-format+))

(defun make-foreign (table column on-delete on-update &optional (add-comma nil))
  (format nil
          " REFERENCES ~a (~a) ON DELETE ~a ON UPDATE ~a ~:[ ~;,~]"
          (quote-symbol table) column on-delete on-update add-comma))

(defun make-role ()
  (query-low-level (strcat (prepare-table-multiple-id +table-role+)
                           +make-unique-description+
                           +make-close+)))

(defun make-person ()
  (query-low-level (strcat (prepare-table-multiple-id +table-person+)
                           " \"primary-name\"  TEXT UNIQUE ON CONFLICT FAIL NOT NULL,"
                           " \"birthday\"      TEXT          " ; timestamp :(
                           +make-close+)))

(defun make-title ()
  (query-low-level (strcat (prepare-table-multiple-id +table-title+)
                           " \"primary-title\"  TEXT   NOT NULL,"
                           " \"original-title\" TEXT   NOT NULL,"
                           " year               TEXT,           " ; timestamp
                           " runtime            INTEGER,        " ; minutes
                           " image              TEXT,           "
                           " tags               TEXT,           "
                           " vote               REAL,           "
                           " notes              TEXT            "
                           +make-close+)))

(defun make-title-country ()
  (query-low-level (strcat (prepare-table +table-title-country+)
                           "title             INTEGER "
                           (make-foreign +table-title+  "id" +cascade+ +cascade+) +col-sep+
                           "country           INTEGER "
                           (make-foreign +table-country+  "id" +cascade+ +cascade+)
                           +make-close+)))

(defun make-movie-copy ()
  (query-low-level (strcat (prepare-table +table-movie-copy+)
                           "title              INTEGER "
                           (make-foreign +table-title+ "id" +restrict+ +cascade+) +col-sep+
                           "format             INTEGER "
                           (make-foreign +table-movie-storage-format+ "id" +restrict+ +cascade+)
                           +col-sep+
                           "barcode            TEXT, "
                           "building           TEXT, "
                           "room               TEXT, "
                           "storage            TEXT, "
                           "shelf              TEXT, "
                           "notes              TEXT  "
                           +make-close+)))

(defun make-crew ()
  (query-low-level (strcat (prepare-table +table-crew+)
                           "title              INTEGER "
                           (make-foreign +table-title+  "id" +cascade+ +cascade+) +col-sep+
                           "person             INTEGER "
                           (make-foreign +table-person+ "id" +restrict+ +cascade+) +col-sep+
                           "role               INTEGER "
                           (make-foreign +table-role+   "id" +cascade+ +cascade+)
                           +make-close+)))

(defun make-title-genre ()
  (query-low-level (strcat (prepare-table +table-title-genre+)
                           "title              INTEGER "
                           (make-foreign +table-title+  "id" +cascade+ +cascade+) +col-sep+
                           "genre              INTEGER "
                           (make-foreign +table-genre+  "id" +restrict+ +cascade+)
                           +make-close+)))

(defun populate-role ()
  (query (make-insert db:+table-role+
                      (:description)
                      (+value-director+))))

(defun populate-genres ()
  (query (make-insert db:+table-genre+
                      (:description)
                      (+value-genre-none+))))

(defun populate-countries ()
  (query (make-insert +table-country+
                      (:description)
                      (+value-countries-none+))))

(defun build-all-indices ()
  (create-table-index +table-country+              '(:description))
  (create-table-index +table-genre+                '(:description))
  (create-table-index +table-movie-storage-format+ '(:description))
  (create-table-index +table-title+                '(:other-id))
  (create-table-index +table-role+                 '(:other-id :description))
  (create-table-index +table-person+               '(:other-id :primary-name))
  (create-table-index +table-movie-copy+))

(defmacro gen-delete (suffix &rest names)
  `(progn
     ,@(loop for name in names collect
            `(,(misc:format-fn-symbol t "delete-~a" suffix) ,name))))

(defun delete-all-tables ()
  (gen-delete table
              +table-movie-copy+
              +table-title-country+
              +table-title-genre+
              +table-title+
              +table-movie-storage-format+
              +table-person+
              +table-country+
              +table-genre+
              +table-role+
              +table-crew+))

(defun build-views ()
  (create-view +view-titles-genres-directors+ (view-title-main-frame))
  (create-view +view-copies-genres-directors+ (view-copy-main-frame)))

(defun delete-all-views ()
  (delete-view  +view-copies-genres-directors+)
  (delete-view  +view-titles-genres-directors+))

(defun delete-database ()
  (with-disabled-foreign
    (delete-all-views)
    (delete-all-tables)))

(defun build-all-tables ()
  (when (= (fs:file-size (db-path))
           0)
    (make-country)
    (make-movie-genre)
    (make-movie-storage-format)
    (make-role)
    (make-person)
    (make-title)
    (make-movie-copy)
    (make-crew)
    (make-title-genre)
    (make-title-country)
    (build-views)
    (populate-role)
    (populate-genres)
    (populate-countries)
    (import-tsv:import-country)
    (build-all-indices)))

;; specific utils

(defun add-description (table desc)
  (query (insert-into table
           (set= :description desc))))

(defun update-description (table old-desc new-desc)
  (query (update table
           (set= :description new-desc)
           (where (:like :description (prepare-for-sql-like old-desc))))))

(defun delete-description (table desc)
  (query (delete-from table
           (where (:like :description
                         (prepare-for-sql-like desc))))))

(defun description-exists-p (table description-looking-for)
  "Note: uses like"
  (object-exists-in-db-p table
                         (:like :description
                                (prepare-for-sql-like description-looking-for))))

(defun table->alist (table col)
  (let ((all (fetch-all (query (select (:id col)
                                 (from table)
                                 (order-by col))))))
    (loop for i in all collect
         (cons (getf i :id)
               (getf i col)))))

(defun persons->alist (filter)
  (let ((all (fetch-all (query (select (:id :primary-name)
                                 (from +table-person+)
                                 (where (:like :primary-name filter))
                                 (order-by :primary-name))))))
    (loop for i in all collect
         (cons (getf i :id)
               (getf i :primary-name)))))

(defun add-person (table name birthday)
  (query (insert-into table
           (set= :primary-name name
                 :birthday     (decode-datetime-string (misc:year->timestamp birthday))))))

(defun add-person-unknown-birthday (table name)
  (query (insert-into table
           (set= :primary-name name
                 :birthday     ""))))

(defun normalize-birthday (a)
  (misc:safe-parse-integer a :fix-fn (lambda (a)
                                       (declare (ignore a))
                                       (_ "unkn."))))

(defun normalize-name (count name year)
  (let ((year-str (normalize-birthday year)))
    (if (> count 1)
        (format nil "~a (~a) ~@r" name year-str count)
        (format nil "~a (~a)"     name year-str))))

(defun update-person (table old-name new-name new-year)
  (query (update table
           (set= :primary-name new-name
                 :birthday     (decode-datetime-string (misc:year->timestamp new-year)))
           (where (:= :primary-name old-name)))))

(defun delete-person (table name)
  (query (delete-from table
           (where (:= :primary-name name)))))

(defun find-person (name)
  (object-exists-in-db-p +table-person+
                         (:like :primary-name name)))

(defun all-ordered-by-description (table)
  (fetch-all (query (select :*
                      (from table)
                      (order-by :description)))))

(defun all-genres ()
  (all-ordered-by-description +table-genre+))

(defun all-genres-description ()
  (loop for row in (all-genres) collect
       (getf row :description)))

(defun all-countries ()
  (all-ordered-by-description +table-country+))

(defun all-countries-description ()
  (loop for row in (all-countries) collect
       (getf row :description)))

(defun all-formats ()
  (all-ordered-by-description +table-movie-storage-format+))

(defun all-formats-description ()
  (loop for row in (all-formats) collect
       (getf row :description)))

(defun unique-description->id (table description)
  (getf (fetch-single (select :id
                        (from table)
                        (where (:= :description
                                   description))))
        :id))

(defun format-description->id (description)
  (unique-description->id +table-movie-storage-format+ description))

(defun id-desc-alist-from-table (table)
  (let ((all (fetch-all (query (select (:id :description)
                                 (from table)
                                 (order-by :description))))))
    (loop for i in all collect
         (cons (getf i :id)
               (getf i :description)))))

(defgeneric fetch-single (sql))

(defmethod fetch-single (sql)
  (fetch (query sql)))

(defmethod fetch-single ((sql string))
  (fetch (query-low-level sql nil)))

(defgeneric fetch-all-rows (query))

(defmethod fetch-all-rows (sql)
  (fetch-all (query sql)))

(defmethod fetch-all-rows ((sql string))
  (fetch-all (query-low-level sql nil)))

(defun fetch-from-any-id (table id)
  (fetch-single (select :*
                  (from  table)
                  (where (:or (:= :id       id)
                              (:= :other-id id))))))
(defun fetch-title (id)
  (fetch-from-any-id +table-title+ id))

(defun fetch-person (id)
  (fetch-from-any-id +table-person+ id))

(defun director-role-id ()
 (getf (fetch-single (select :*
                       (from +table-role+)
                       (where (:= :description +value-director+))))
       :id))

(defun create-view (name select-query)
  (query-low-level (format nil "create view ~a as ~a"
                           (quote-symbol name)
                           (query->sql select-query))))

(defun view-title-main-frame ()
  (select (:*
           (:as (:group_concat :inner-genres)
                +search-expr-genres-col+))
    (from (select ((:as :title.id            :title-id)
                   (:as :title.primary-title +search-expr-primary-title-col+)
                   (:as :original-title      +search-expr-original-title-col+)
                   (:as (:group_concat :person.primary-name)
                        +search-expr-director-col+)
                   (:as :genre.description   :inner-genres)
                   (:as :title.notes         +search-expr-notes-col+)
                   (:as :title.tags          +search-expr-tags-col+)
                   (:as :title.vote          :vote)
                   (:as :crew.role           :crew-role)
                   :year)
            (from :title)
            (left-join :crew   :on      (:= :crew.title :title.id))
            (left-join :person :on      (:= :person.id :crew.person))
            (left-join :title-genre :on (:= :title-genre.title :title-id))
            (left-join :genre  :on      (:= :genre.id  :title-genre.genre))
            (group-by :title-id :genre.id)))
    (group-by :title-id)))

(defun search-titles-main-frame (key &key
                                       (order-dir :desc)
                                       (order-columns :title-id))
  (with-director-db-id (director-id)
    (let* ((res (select (:*
                         (:as (:group_concat :description)
                              +search-expr-country-col+))
                  (from +view-titles-genres-directors+)
                  (where (:and (:or (:like :pt       (prepare-for-sql-like key))
                                    (:like :ot       (prepare-for-sql-like key))
                                    (:like :director (prepare-for-sql-like key))
                                    (:like :year     (format nil "~a%" key)))
                               (:or (:= :crew-role director-id)
                                    (:is-null :crew-role))))
                  (left-join :title-country :on (:= :title-id :title-country.title))
                  (left-join :country       :on (:= :country.id :title-country.country))
                  (order-by `(,order-dir ,order-columns))
                  (group-by :title-id))))
     (fetch-all-rows res))))

(defun all-genres-by-title (title-id)
  (let* ((res (select ((:as :genre.id          :id)
                       (:as :genre.description :desc))
                (from :title)
                (inner-join :title-genre :on (:= :title-genre.title :title.id))
                (inner-join :genre       :on (:= :genre.id          :title-genre.genre))
                (where (:= :title.id title-id))
                (order-by (:asc :description)))))
     (fetch-all-rows res)))

(defun all-directors-by-title (title-id)
  (with-director-db-id (director-id)
    (let* ((res (select ((:as :person.id           :id)
                         (:as :person.primary-name :desc))
                  (from :title)
                  (inner-join :crew        :on (:= :crew.title :title.id))
                  (inner-join :person      :on (:= :person.id  :crew.person))
                  (where (:and (:= :title.id  title-id)
                               (:= :crew.role director-id)))
                  (order-by (:asc :desc)))))
     (fetch-all-rows res))))

(defun all-title-countries-ids (title-id)
  (let* ((res (select ((:as :country.id :cid))
                (from :title)
                (inner-join :title-country :on (:= :title-country.title :title.id))
                (inner-join :country       :on (:= :cid                 :title-country.country))
                (where (:= :title.id title-id))
                (order-by (:asc :description)))))
     (remove-if #'(lambda (a) (typep a 'symbol)) (flatten (fetch-all-rows res)))))

(defun all-countries-by-title (title-id)
  (let* ((res (select ((:as :country.id          :id)
                       (:as :country.description :desc))
                (from :title)
                (inner-join :title-country :on (:= :title-country.title :title.id))
                (inner-join :country       :on (:= :country.id          :title-country.country))
                (where (:= :title.id title-id))
                (order-by (:asc :description)))))
     (fetch-all-rows res)))

(defun delete-by-id (table id)
  (query (delete-from table (where (:= :id id)))))

(defun view-search-titles ()
    (select (:*
           (:as (:group_concat :inner-genres)
                +search-expr-genres-col+))
    (from (select ((:as :title.id            :title-id)
                   (:as :title.primary-title +search-expr-primary-title-col+)
                   (:as :original-title      +search-expr-original-title-col+)
                   (:as (:group_concat :person.primary-name)
                        +search-expr-director-col+)
                   (:as :genre.description   :inner-genres)
                   (:as :title.notes         +search-expr-notes-col+)
                   (:as :title.tags          +search-expr-tags-col+)
                   (:as :crew.role           :crew-role)
                   :year)
            (from :title)
            (left-join :crew   :on      (:= :crew.title :title.id))
            (left-join :person :on      (:= :person.id :crew.person))
            (left-join :title-genre :on (:= :title-genre.title :title-id))
            (left-join :genre  :on      (:= :genre.id  :title-genre.genre))
            (group-by :title-id :genre.id)))
    (group-by :title-id)))

(defmacro title-general-query (&optional (where-clause nil) (add-group-by t))
  (let ((query `(select (:*
                        (:as (:group_concat :description)
                             ,+search-expr-country-col+))
                 (from ,+view-titles-genres-directors+)
                 (left-join :title-country :on (:= :title-id :title-country.title))
                 (left-join :country       :on (:= :country.id :title-country.country)))))
    (if where-clause
        (append query
                (list where-clause)
                (if add-group-by
                    `((group-by :title-id))
                    nil))
        (append query
                (if add-group-by
                    `((group-by :title-id))
                    nil)))))

(defun filter-directors (key)
  (let ((query (query (select (:person.id :person.primary-name)
                        (from :person)
                        (where (:and (:like :primary-name (prepare-for-sql-like key))))
                        (order-by :primary-name)))))
  (fetch-all query)))

(defun link-title-to-countries (title-id countries)
  (loop for country in countries do
       (let ((country-row (fetch-single (select (:id)
                                          (from :country)
                                          (where (:= :description country))))))
         (when country-row
           (query (make-insert :title-country
                               (:title
                                :country)
                               (title-id
                                (getf country-row :id))))))))

(defun link-title-to-directors (title-id directors)
  (with-director-db-id (director-id)
    (loop for director in directors do
         (let ((person-row (fetch-single (select (:id)
                                          (from :person)
                                          (where (:= :primary-name director))))))
           (when person-row
             (query (make-insert :crew
                                 (:title
                                  :person
                                  :role)
                                 (title-id
                                  (getf person-row :id)
                                  director-id))))))))

(defun link-title-to-genres (title-id genres)
  (loop for genre in genres do
       (let ((genre-row (fetch-single (select (:id)
                                       (from :genre)
                                       (where (:= :description genre))))))
         (when genre-row
           (query (make-insert :title-genre
                               (:title
                                :genre)
                               (title-id
                                (getf genre-row :id))))))))

(defun add-new-movie (image        new-pr-title
                      new-or-title new-directors
                      new-year     new-runtime
                      new-genres   new-tags
                      new-notes    new-countries
                      new-vote)
  (with-db-transaction
    (query (make-insert :title
                        (:primary-title
                         :original-title
                         :year
                         :runtime
                         :tags
                         :notes
                         :image
                         :vote)
                        (new-pr-title
                         new-or-title
                         (decode-datetime-string new-year)
                         new-runtime
                         new-tags
                         new-notes
                         (decode-blob image)
                         new-vote)))
    (let ((last-id (get-max-id :title))
          (actual-new-genres (or new-genres (list +value-genre-none+)))
          (actual-new-countries (or new-countries (list +value-countries-none+))))
      (link-title-to-directors last-id new-directors)
      (link-title-to-genres    last-id actual-new-genres)
      (link-title-to-countries last-id actual-new-countries)
      last-id)))

(defun update-movie (title-id      image
                     new-pr-title  new-or-title
                     new-directors new-year
                     new-runtime   new-genres
                     new-tags      new-notes
                     new-countries new-vote)
  (with-db-transaction
    (query (update :title
             (set= :primary-title  new-pr-title
                   :original-title new-or-title
                   :year           (decode-datetime-string new-year)
                   :runtime        new-runtime
                   :tags           new-tags
                   :notes          new-notes
                   :image          (decode-blob image)
                   :vote           new-vote)
             (where (:= :id title-id))))
    (let ((actual-new-genres (or new-genres (list +value-genre-none+))))
      (query (delete-from :title-genre (where (:= :title title-id))))
      (link-title-to-genres title-id actual-new-genres))
    (when new-directors
      (query (delete-from :crew (where (:= :title title-id))))
      (link-title-to-directors title-id new-directors))
    (when new-countries
      (query (delete-from :title-country (where (:= :title title-id))))
      (link-title-to-countries title-id new-countries))))

(defun remove-link-title-director (title-id director-name)
  (with-db-transaction
    (with-director-db-id (director-id)
      (let* ((rows (query (select ((:as :c.id :cid))
                            (from (:as :person :p))
                            (inner-join (:as :crew  :c) :on (:= :p.id :c.person))
                            (inner-join (:as :title :t) :on (:= :t.id :c.title))
                            (where (:and (:= :c.role              director-id)
                                         (:= :t.id                title-id)
                                         (:= :p.primary-name      director-name)))))))
        (loop for row in rows do
             (when-let ((remove-crew-id (getf row :cid)))
               (delete-by-id :crew remove-crew-id)))))))

(defun add-new-copy (title-id new-barcode new-position new-notes new-format)
  (with-db-transaction
    (multiple-value-bind (building room storage shelf)
        (decode-copy-position new-position)
      (query (make-insert +table-movie-copy+
                          (:title
                           :barcode
                           :notes
                           :building
                           :room
                           :storage
                           :shelf
                           :format)
                          (title-id
                           new-barcode
                           new-notes
                           building
                           room
                           storage
                           shelf
                           (format-description->id new-format))))
      (get-max-id +table-movie-copy+))))

(defun update-copy (copy-id new-barcode new-position new-notes new-format)
  (with-db-transaction
    (multiple-value-bind (building room storage shelf)
        (decode-copy-position new-position)
      (query (update +table-movie-copy+
               (set= :barcode  new-barcode
                     :notes    new-notes
                     :building building
                     :room     room
                     :storage  storage
                     :shelf    shelf
                     :format   (format-description->id new-format))
               (where (:= :id copy-id)))))))

(defun copy-id->titles (id)
  (let ((query (select ((:as :title.original-title
                             +search-expr-original-title-col+)
                        (:as :title.primary-title
                             +search-expr-primary-title-col+))
                 (from (:as +table-movie-copy+ :copy))
                 (inner-join :title :on (:= :title.id  :copy.title))
                 (where (:= :copy.id id)))))
  (fetch-single query)))

(defun view-copy-main-frame ()
  (select (:*
           (:as (:group_concat :inner-genres)
                +search-expr-genres-col+))
    (from (select ((:as :title.id            :title-id)
                   (:as :copy.id             :copy-id)
                   (:as :title.primary-title +search-expr-primary-title-col+)
                   (:as :original-title      +search-expr-original-title-col+)
                   (:as (:group_concat :person.primary-name)
                        +search-expr-director-col+)
                   (:as :genre.description   :inner-genres)
                   (:as :copy.notes         +search-expr-notes-col+)
                   (:as :title.tags          +search-expr-tags-col+)
                   (:as :crew.role           :crew-role)
                   (:as :copy.barcode  :barcode)
                   (:as :copy.building :building)
                   (:as :copy.room     :room)
                   (:as :copy.storage  :storage)
                   (:as :copy.shelf    :shelf)
                   (:as :format.description  :format)
                   :year)
            (from :title)
            (left-join (:as :movie-copy :copy) :on  (:= :copy.title :title.id))
            (left-join :crew   :on      (:= :crew.title :title.id))
            (left-join :person :on      (:= :person.id :crew.person))
            (left-join :title-genre :on (:= :title-genre.title :title-id))
            (left-join :genre  :on      (:= :genre.id  :title-genre.genre))
            (inner-join (:as +table-movie-storage-format+ :format)
                        :on (:= :format.id         :copy.format))
            (group-by :copy-id :genre.id)))
    (group-by :copy-id)))

(defun search-copies-main-frame (key &key
                                       (order-dir :asc)
                                       (order-columns :title-id))
  (let* ((res (select (:*
                       (:as (:group_concat :description)
                            +search-expr-country-col+))
                (from +view-copies-genres-directors+)
                (left-join :title-country :on (:= :title-id :title-country.title))
                (left-join :country       :on (:= :country.id :title-country.country))
                (where (:and (:or (:like :barcode  (prepare-for-sql-like key))
                                  (:like :copy-id  (prepare-for-sql-like key))
                                  (:like :pt       (prepare-for-sql-like key))
                                  (:like :ot       (prepare-for-sql-like key))
                                  (:like :director (prepare-for-sql-like key))
                                  (:like :building (prepare-for-sql-like key))
                                  (:like :room     (prepare-for-sql-like key))
                                  (:like :storage  (prepare-for-sql-like key))
                                  (:like :shelf    (prepare-for-sql-like key))
                                  (:like :year     (format nil "~a%" key)))))
                (order-by `(,order-dir ,order-columns))
                (group-by :copy-id))))
    (fetch-all-rows res)))

(defun last-n-copies-id (offset)
    (let* ((res (select :*
                  (from +view-copies-genres-directors+)
                  (order-by (:asc :copy-id))
                  (limit offset))))
      (fetch-all-rows res)))

(defun sql-order-by (column direction)
  (yield (order-by `(,direction ,column))))

(defmacro sql-group-by (&rest columns)
  `(yield (group-by ,@columns)))

(defun sql-search-titles ()
  (query->sql (title-general-query nil nil)))

(defun search-movie-expr (expr order-columns order-direction)
  (db-utils:with-ready-database (:connect nil)
    (let ((where-clause    (search-title-expr:parse (subseq expr 1)))
          (group-clause    (db:sql-group-by  :title-id))
          (order-by-clause (db:sql-order-by order-columns order-direction)))
      (if (null where-clause)
          (values nil t)
          (let* ((select-cmd (sql-search-titles))
                 (res        (join-with-strings* " "
                                                 select-cmd where-clause
                                                 group-clause
                                                 order-by-clause)))
            (values (fetch-all-rows res) nil))))))

(defun search-movie-simple (key order-columns order-direction)
  (db-utils:with-ready-database (:connect nil)
    (search-titles-main-frame key
                              :order-dir     order-direction
                              :order-columns order-columns)))

(defun search-movies (key order-columns order-direction)
  (db-utils:with-ready-database (:connect nil)
    (cond
      ((char= +char-start-search-expr+ (first-elt key))
       (search-movie-expr key order-columns order-direction))
      (t
       (search-movie-simple key order-columns order-direction)))))

(defun search-copies (key order-columns order-direction)
  (db-utils:with-ready-database (:connect nil)
    (let* ((res (cond
                  ((cl-ppcre:scan +search-latest-id-re+ key)
                   (last-n-copies-id (misc:safe-parse-number (subseq key 1)
                                                             :fix-fn
                                                             (lambda (e)
                                                               (declare (ignore e))
                                                               0))))
                  ((char= +char-start-search-expr+ (first-elt key))
                   (search-copies-expr key order-columns order-direction))
                  (t
                   (search-copies-main-frame key
                                             :order-dir     order-direction
                                             :order-columns order-columns)))))
      res)))

(defmacro copies-general-query (&optional (where-clause nil) (add-group-by t))
  (let ((query `(select (:*
                        (:as (:group_concat :description)
                             ,+search-expr-country-col+))
                 (from ,+view-copies-genres-directors+)
                 (left-join :title-country :on (:= :title-id :title-country.title))
                 (left-join :country       :on (:= :country.id :title-country.country)))))
    (if where-clause
        (append query
                (list where-clause)
                (if add-group-by
                    `((group-by :title-id))
                    nil))
        (append query
                (if add-group-by
                    `((group-by :title-id))
                    nil)))))

(defun sql-search-copies ()
  (query->sql (copies-general-query nil nil)))

(defun search-copies-expr (expr order-columns order-direction)
  (db-utils:with-ready-database (:connect nil)
    (let ((where-clause    (search-copy-expr:parse (subseq expr 1)))
          (group-clause    (db:sql-group-by  :copy-id))
          (order-by-clause (db:sql-order-by order-columns order-direction)))
      (if (null where-clause)
          (values nil t)
          (let* ((select-cmd (sql-search-copies))
                 (res        (join-with-strings* " "
                                                 select-cmd where-clause
                                                 group-clause
                                                 order-by-clause)))
            (values (fetch-all-rows res) nil))))))

(defun title-details (title-id)
  (let ((query (title-general-query
                 (where (:= :title-id title-id)))))
    (fetch-single query)))

(defun copy-row->format-row (copy-row)
  (db:fetch-from-any-id +table-movie-storage-format+
                        (getf copy-row :format)))

(defun copy-row->format-description (copy-row)
  (let* ((format (copy-row->format-row copy-row)))
    (getf format :description)))
