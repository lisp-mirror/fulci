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

(in-package :manage-movie)

(define-constant +autocomplete-director-min-key-length+   3 :test #'=)

(define-constant +vote-bar-width+                       120 :test #'=)

(cl-syntax:use-syntax nodgui.event-parser:nodgui-event-syntax)

(defclass add-movie-frame (frame)
  ((image-button
    :initform nil
    :accessor image-button
    :type     button
    :documentation "displayed image")
   (vote-bar
    :initform nil
    :accessor vote-bar)
   (vote-scale
    :initform nil
    :accessor vote-scale)
   (primary-title-text-label
    :initform nil
    :accessor primary-title-text-label)
   (primary-title-text-entry
    :initform nil
    :accessor primary-title-text-entry)
   (original-title-text-label
    :initform nil
    :accessor original-title-text-label
    :type     label)
   (original-title-text-entry
    :initform nil
    :accessor original-title-text-entry
    :type     entry)
   (added-director-text-label
    :initform nil
    :accessor added-director-text-label
    :type     label)
   (added-director-listbox
    :initform nil
    :accessor added-director-listbox
    :type     scrolled-listbox)
   (director-text-label
    :initform nil
    :accessor director-text-label
    :type     label)
   (director-autocomplete
    :initform nil
    :accessor director-autocomplete
    :type     autocomplete-listbox)
   (year-text-label
    :initform nil
    :accessor year-text-label
    :type     label)
   (year-text-entry
    :initform nil
    :accessor year-text-entry
    :type     entry)
   (runtime-text-label
    :initform nil
    :accessor runtime-text-label
    :type     label)
   (runtime-text-entry
    :initform nil
    :accessor runtime-text-entry
    :type     entry)
   (genres-label
    :initform nil
    :accessor genres-label
    :type     label)
   (genres-searchbox
    :initform nil
    :accessor genres-searchbox
    :type     searchable-listbox)
   (countries-label
    :initform nil
    :accessor countries-label
    :type     label)
   (countries-searchbox
    :initform nil
    :accessor countries-searchbox
    :type     searchable-listbox)
   (tags-text-label
    :initform nil
    :accessor tags-text-label
    :type     label)
   (tags-text-entry
    :initform nil
    :accessor tags-text-entry
    :type     entry)
   (notes-text-label
    :initform nil
    :accessor notes-text-label
    :type     label)
   (notes-text-entry
    :initform nil
    :accessor notes-text-entry
    :type     entry)
   (apply-button
    :initform nil
    :accessor apply-button
    :type     button)
   (close-button
    :initform nil
    :accessor close-button
    :type     button)
   (fetch-wiki-data-button
    :initform nil
    :accessor fetch-wiki-data-button
    :type     button)
   (fetch-wiki-image-button
    :initform nil
    :accessor fetch-wiki-image-button
    :type     button)
   (image
    :initform nil
    :accessor image
    :type     vector
    :documentation "Raw image data")
   (container-win
    :initform nil
    :initarg  :container-win
    :accessor container-win
    :type     widget)
   (title-id
    :initform nil
    :initarg  :title-id
    :accessor title-id
    :type     integer)))

(defmacro with-all-accessors ((object) &body body)
  "Very anaphoric :)"
  `(with-accessors ((image-button              image-button)
                    (vote-bar                  vote-bar)
                    (vote-scale                vote-scale)
                    (fetch-wiki-data-button    fetch-wiki-data-button)
                    (fetch-wiki-image-button   fetch-wiki-image-button)
                    (primary-title-text-label  primary-title-text-label)
                    (primary-title-text-entry  primary-title-text-entry)
                    (original-title-text-label original-title-text-label)
                    (original-title-text-entry original-title-text-entry)
                    (added-director-text-label added-director-text-label)
                    (added-director-listbox    added-director-listbox)
                    (director-text-label       director-text-label)
                    (director-autocomplete     director-autocomplete)
                    (year-text-label           year-text-label)
                    (year-text-entry           year-text-entry)
                    (runtime-text-label        runtime-text-label)
                    (runtime-text-entry        runtime-text-entry)
                    (genres-label              genres-label)
                    (genres-searchbox          genres-searchbox)
                    (countries-label           countries-label)
                    (countries-searchbox       countries-searchbox)
                    (tags-text-label           tags-text-label)
                    (tags-text-entry           tags-text-entry)
                    (notes-text-label          notes-text-label)
                    (notes-text-entry          notes-text-entry)
                    (apply-button              apply-button)
                    (close-button              close-button)
                    (image                     image)
                    (container-win             container-win)
                    (title-id                  title-id))                  ,object
     ,@body))

(defun build-image (frame bitmap)
  (with-accessors ((image-button image-button)
                   (image        image))       frame
      (let* ((tga-data (image->tga-memory bitmap))
             (tga      (make-instance 'nodgui.pixmap:tga)))
        (load-from-vector tga tga-data)
        (let* ((w     (nodgui.pixmap:width  tga))
               (h     (nodgui.pixmap:height tga))
               (scale (if (> w h)
                          (/ +image-max-w+ w)
                          (/ +image-max-w+ h))))
          (setf tga (nodgui.pixmap:scale-nearest tga scale scale))
          (nodgui.pixmap:sync-data-to-bits tga)
          (setf image (tga-as-vector tga))
          (setf (image image-button) (make-image tga))))))

(defun add-image-clsr (frame)
  (lambda ()
    (with-accessors ((image-button image-button)
                     (image        image))       frame
      (let ((file     (get-open-file :file-types '(("JPEG" "*.jpg"))
                                     :parent     frame)))
        (when (string-not-empty-p file)
          (with-busy* (*tk*)
            (let* ((bitmap (fs:slurp-file file :convert-to-string nil)))
              (build-image frame bitmap))))))))

(defun dialog-wiki-choose-title (frame matched-titles)
  (listbox-dialog frame
                  (_ "Choose item")
                  (_ "These are the titles returned from wikipedia, select one to accept or close window to discard.")
                  matched-titles
                  :key #'cdr))

(defun allowed-to-fetch-from-wiki-p (title)
  (and (string-not-empty-p title)
       (> (length title) 5)))

(defun sort-wiki-page-results (entries)
  (let ((important-word (pref:preferences-wiki-important-string)))
    (flet ((sort-predicate (a b)
             (let ((title-a (cdr a))
                   (title-b (cdr b)))
               (cond
                 ((scan important-word title-a)
                  t)
                 ((scan important-word title-b)
                  nil)
               (t
                (string-greaterp title-a title-b))))))
      (sort entries #'sort-predicate))))

(defun director-not-exists-in-db-p (key)
  (and (string-not-empty-p key)
       (> (length key) +autocomplete-director-min-key-length+)
       (not (db:filter-directors key))))

(defun first-director (movie-entry-info)
  (and (movie-entry-director movie-entry-info)
       (first (movie-entry-director movie-entry-info))))

(defun try-add-new-directors (frame directors)
  (let ((new-directors-msg '()))
    (loop for director in directors when (director-not-exists-in-db-p director) do
         (push director new-directors-msg)
         (let ((actual-name (db:normalize-name 0 director nil)))
           (db:add-person-unknown-birthday db:+table-person+ actual-name)))
    (when new-directors-msg
      (info-dialog frame
                   (strcat (format nil
                                   (n_ "The following director has been added to database:~2%~a~2%"
                                       "The following directors have been added to database:~2%~a2~%"
                                       (length new-directors-msg))
                                   (join-with-strings new-directors-msg ", "))
                           (n_ "You can add them to this movie using the \"Director\" listbox."
                               "You can add them to this movie using the \"Director\" listbox."
                               (length new-directors-msg)))))))

(defun add-data-from-wiki-clsr (frame)
  (lambda ()
    (with-busy* (frame)
      (with-all-accessors (frame)
        (let ((pt (text primary-title-text-entry)))
          (when (allowed-to-fetch-from-wiki-p pt)
            (when-let* ((*wikipedia-host* (preferences:preferences-wiki-host))
                        (matched-titles   (sort-wiki-page-results (search-wiki-pages pt)))
                        (selected         (dialog-wiki-choose-title frame matched-titles))
                        (info             (get-movie-info (first-elt selected)))
                        (year             (or (movie-entry-year info)
                                              (local-time-obj-now)))
                        (directors       (movie-entry-director info)))
              (try-add-new-directors frame directors)
              ;; these should be extracted
              (setf (text primary-title-text-entry)      (movie-entry-title           info))
              (setf (text original-title-text-entry)     (movie-entry-title           info))
              (setf (text (entry director-autocomplete)) (first-director info))
              (setf (text year-text-entry)               (extract-year-from-timestamp year))
              (setf (text runtime-text-entry)
                    (movie-entry-runtime         info))
              (when (string-not-empty-p (text (entry director-autocomplete)))
                (launch-autocompletion director-autocomplete)))))))))

(defun add-image-from-wiki-clsr (frame)
  (lambda ()
    (with-busy* (frame)
      (with-all-accessors (frame)
        (let ((pt (text primary-title-text-entry)))
          (when (allowed-to-fetch-from-wiki-p pt)
            (when-let* ((*wikipedia-host* (preferences:preferences-wiki-host))
                        (matched-titles   (sort-wiki-page-results (search-wiki-pages pt)))
                        (selected         (dialog-wiki-choose-title frame matched-titles))
                        (page-id          (car (find-if (lambda (a)
                                                          (string= (cdr a) (first selected)))
                                                        matched-titles)))
                        (jpg-data         (search-wiki-image page-id)))
              (build-image frame jpg-data))))))))

(defun insert-new-title (frame         image
                         new-pr-title  new-or-title
                         new-directors new-year
                         new-runtime   new-genres
                         new-tags      new-notes
                         new-countries new-vote)
  (with-accessors ((added-director-listbox added-director-listbox)
                   (title-id               title-id))              frame
    (let ((new-title-id (db:add-new-movie image         new-pr-title
                                          new-or-title  new-directors
                                          new-year      new-runtime
                                          new-genres    new-tags
                                          new-notes     new-countries
                                          new-vote)))
      (listbox-delete added-director-listbox)
      (dolist (new-director new-directors)
        (listbox-append added-director-listbox new-director))
      (setf title-id new-title-id)
      (info-operation-completed frame))))

(defun update-title (frame         image
                     new-pr-title  new-or-title
                     new-directors new-year
                     new-runtime   new-genres
                     new-tags      new-notes
                     new-countries new-vote)
  (with-accessors ((added-director-listbox added-director-listbox)
                   (title-id               title-id))              frame
    (db:update-movie title-id      image
                     new-pr-title  new-or-title
                     new-directors new-year
                     new-runtime   new-genres
                     new-tags      new-notes
                     new-countries new-vote)
    (listbox-delete added-director-listbox)
    (dolist (new-director new-directors)
      (listbox-append added-director-listbox new-director))
    (info-operation-completed frame)))

(defun get-searchbox-selection (searchbox)
  (if (= (length (listbox-all-values searchbox))
         1)
      (listbox-all-values searchbox)
      (listbox-get-selection-value searchbox)))

(defun add-movie-clsr (frame)
  (lambda ()
    (with-all-accessors (frame)
      (declare (ignore apply-button close-button))
      (with-entry-text-validate
          (frame (primary-title-text-entry  +free-text-re+
                                            (_ "Primary title can not be empty"))
                 (year-text-entry           +pos-integer-re+
                                            (_ "Year must be a positive number"))
                 (runtime-text-entry        +pos-integer-re+
                                            (_ "Runtime must be a positive number")))
        (let ((new-pr-title  (text primary-title-text-entry))
              (new-or-title  (if (string-empty-p (text original-title-text-entry))
                                 (text primary-title-text-entry)
                                 (text original-title-text-entry)))
              (new-directors (union (listbox-get-selection-value director-autocomplete)
                                    (listbox-all-values          added-director-listbox)
                                    :test #'string-equal))
              (new-year      (year->timestamp             (text year-text-entry)))
              (new-runtime   (parse-integer               (text runtime-text-entry)))
              (new-genres    (get-searchbox-selection genres-searchbox))
              (new-countries (get-searchbox-selection countries-searchbox))
              (new-tags      (text tags-text-entry))
              (new-notes     (text notes-text-entry))
              (new-vote      (decode-vote (nodgui.mw:value vote-bar))))
          ;; resync original title
          (setf (text original-title-text-entry) new-or-title)
          (if (null new-directors)
              (error-dialog frame (_ "No director choosen"))
              (if (update-mode-p frame)
                  (update-title     frame         image
                                    new-pr-title  new-or-title
                                    new-directors new-year
                                    new-runtime   new-genres
                                    new-tags      new-notes
                                    new-countries new-vote)
                  (insert-new-title frame         image
                                    new-pr-title  new-or-title
                                    new-directors new-year
                                    new-runtime   new-genres
                                    new-tags      new-notes
                                    new-countries new-vote))))))))

(defun autocomplete-directors-fn (key)
  (when (and (not (string-empty-p key))
             (> (length key) +autocomplete-director-min-key-length+))
    (let ((all (db:filter-directors key)))
      (mapcar #'(lambda (a) (getf a :primary-name))
              all))))

(defun update-mode-p (add-movie-frame)
  (title-id add-movie-frame))

(defun delete-director-clsr (frame)
  (lambda ()
    (with-accessors ((added-director-listbox added-director-listbox)
                     (title-id               title-id))              frame
      (when (update-mode-p frame)
        (let ((selection-indices (listbox-get-selection-index added-director-listbox))
              (selection-values  (listbox-get-selection-value added-director-listbox)))
          (loop
             for index         in selection-indices
             for director-name in selection-values  do
               (listbox-delete added-director-listbox index index)
               (db:remove-link-title-director title-id director-name)))))))

(define-constant +max-scale-vote+ 100 :test #'=)

(defmethod initialize-instance :after ((object add-movie-frame) &key &allow-other-keys)
  (with-all-accessors (object)
    (setf image-button              (make-instance 'button
                                                   :text    (_ "Press to add an image from file")
                                                   :command (add-image-clsr object)
                                                   :master  object))
    (setf vote-bar                  (make-instance 'nodgui.mw:progress-bar-star
                                                   :star-num   +maximum-vote+
                                                   :width      +vote-bar-width+
                                                   :height     (floor (/ +vote-bar-width+ 5))
                                                   :master     object))
    (setf vote-scale                (make-instance 'scale
                                                   :length  +vote-bar-width+
                                                   :master  object
                                                   :from      0
                                                   :to      +max-scale-vote+
                                                   :command (lambda (a)
                                                              (declare (ignore a))
                                                              (let ((v (/ (value vote-scale)
                                                                          +max-scale-vote+)))
                                                                (setf (nodgui.mw:value vote-bar)
                                                                      v)))))
    (setf primary-title-text-label  (make-instance 'label
                                                   :text   (_ "Primary title:")
                                                   :master object))
    (setf primary-title-text-entry  (make-instance 'entry
                                                   :master object))
    (setf original-title-text-label (make-instance 'label
                                                   :text   (_ "Original title:")
                                                   :master object))
    (setf original-title-text-entry (make-instance 'entry
                                                   :master object))
    (setf added-director-text-label (make-instance 'label
                                                   :text   (_ "Added directors:")
                                                   :master object))
    (setf added-director-listbox    (make-instance 'scrolled-listbox
                                                   :select-mode                :extended
                                                   :master object))
    (setf director-text-label       (make-instance 'label
                                                   :text   (_ "Director:")
                                                   :master object))
    (setf director-autocomplete     (make-instance 'autocomplete-listbox
                                                   :entry-label                (_ "Search:")
                                                   :export-selection           nil
                                                   :select-mode                :extended
                                                   :autocomplete-function-hook
                                                   #'autocomplete-directors-fn
                                                   :master       object))
    (setf year-text-label           (make-instance 'label
                                                   :text   (_ "Year:")
                                                   :master object))
    (setf year-text-entry           (make-instance 'entry
                                                   :master object))
    (setf runtime-text-label        (make-instance 'label
                                                   :text   (_ "Runtime (minutes):")
                                                   :master object))
    (setf runtime-text-entry        (make-instance 'entry
                                                   :master object))
    (setf genres-label              (make-instance 'label
                                                   :text   (_ "Genres:")
                                                   :master object))
    (setf genres-searchbox          (make-instance 'searchable-listbox
                                                   :export-selection      nil
                                                   :select-mode :extended
                                                   :data
                                                   (db:all-genres-description)
                                                   :entry-label           (_ "Filter:")
                                                   :master                object
                                                   :remove-non-matching-p t
                                                   :matching-fn
                                                   (lambda (a b)
                                                     (scan (strcat "(?i)" a) b))))
    (setf countries-label              (make-instance 'label
                                                   :text   (_ "Countries:")
                                                   :master object))
    (setf countries-searchbox          (make-instance 'searchable-listbox
                                                   :export-selection      nil
                                                   :select-mode :extended
                                                   :data
                                                   (db:all-countries-description)
                                                   :entry-label           (_ "Filter:")
                                                   :master                object
                                                   :remove-non-matching-p t
                                                   :matching-fn
                                                   (lambda (a b)
                                                     (scan (strcat "(?i)" a) b))))
    (setf tags-text-label           (make-instance 'label
                                                   :text   (_ "Tags (comma separated values):")
                                                   :master object))
    (setf tags-text-entry           (make-instance 'entry
                                                   :master object))
    (setf notes-text-label          (make-instance 'label
                                                   :text   (_ "Notes:")
                                                   :master object))
    (setf notes-text-entry          (make-instance 'entry
                                                   :master object))
    (setf apply-button              (make-instance 'button
                                                   :text    (_ "Apply")
                                                   :command (add-movie-clsr object)
                                                   :master  object))
    (setf close-button              (make-instance 'button
                                                   :text    (_ "Close")
                                                   :command (lambda ()
                                                              (setf *break-mainloop* t))
                                                   :master  object))
    (let* ((delete-director-button  (make-instance 'button
                                                   :master  object
                                                   :image   *icon-delete-small*
                                                   :command (delete-director-clsr object)))
           (wiki-frame              (make-instance 'frame
                                                   :master object)))

      (setf fetch-wiki-data-button    (make-instance 'button
                                                     :image   *icon-wiki-fetch-data*
                                                     :command (add-data-from-wiki-clsr object)
                                                     :master  wiki-frame))
      (setf fetch-wiki-image-button   (make-instance 'button
                                                     :image   *icon-wiki-fetch-image*
                                                     :command (add-image-from-wiki-clsr object)
                                                     :master  wiki-frame))
      (attach-tooltips (fetch-wiki-data-button  (_ "get data from wikipedia"))
                       (fetch-wiki-image-button (_ "get movie's image from wikipedia")))
      (grid image-button              0 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+
            :rowspan 6)
      (grid vote-bar                  7 0 :sticky :ns   :padx +min-padding+ :pady +min-padding+)
      (grid vote-scale                8 0 :sticky :ns   :padx +min-padding+ :pady +min-padding+)
      (grid wiki-frame               12 0 :sticky :news :padx +min-padding+ :pady +min-padding+)
      (grid fetch-wiki-data-button    0 1 :sticky :sw   :padx +min-padding+ :pady +min-padding+)
      (grid fetch-wiki-image-button   0 2 :sticky :sw   :padx +min-padding+ :pady +min-padding+)
      (grid primary-title-text-label  0 1 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid primary-title-text-entry  0 2 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid original-title-text-label 0 3 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid original-title-text-entry 0 4 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid director-text-label       1 1 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid director-autocomplete     1 2 :sticky :we   :padx +min-padding+ :pady +min-padding+
            :rowspan 2)
      (grid added-director-text-label 1 3 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid added-director-listbox    1 4 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid delete-director-button    2 4 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid genres-label              4 1 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid genres-searchbox          4 2 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid countries-label           4 3 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid countries-searchbox       4 4 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid year-text-label           6 1 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid year-text-entry           6 2 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid runtime-text-label        6 3 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid runtime-text-entry        6 4 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid tags-text-label          10 1 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid tags-text-entry          10 2 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid notes-text-label         10 3 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid notes-text-entry         10 4 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid apply-button             12 1 :sticky :s    :padx +min-padding+ :pady +min-padding+)
      (grid close-button             12 2 :sticky :s    :padx +min-padding+ :pady +min-padding+)
      (gui-resize-grid-all object)
      (when (update-mode-p object)
        (sync-title-frame object)))))

(defun load-image-in-button (title-id button)
  (let ((title-info     (db:fetch-from-any-id db:+table-title+ title-id)))
    (when (not (db-nil-p (getf title-info :image)))
      (let* ((tga-data (nodgui.base64:decode (getf title-info :image)))
             (tga      (make-instance 'nodgui.pixmap:tga)))
        (load-from-vector tga tga-data)
        (setf (image button) (make-image tga))
        tga-data))))

(defun sync-title-frame (frame)
  (with-all-accessors (frame)
    (let* ((title-info     (db:fetch-from-any-id db:+table-title+ title-id))
           (genre-info     (mapcar (lambda (a) (getf a :desc))
                                   (db:all-genres-by-title title-id)))
           (all-genres     (listbox-all-values genres-searchbox))
           (country-info   (mapcar (lambda (a) (getf a :desc))
                                   (db:all-countries-by-title title-id)))
           (all-countries  (listbox-all-values countries-searchbox))
           (directors-info (db:all-directors-by-title title-id))
           (vote           (encode-vote (getf title-info :vote))))
      (loop for i in directors-info do
           (listbox-append added-director-listbox (getf i :desc)))
      (loop for genre-desc in genre-info do
           (let ((idx (position genre-desc all-genres :test #'string-equal)))
             (listbox-select genres-searchbox idx)))
      (loop for country-desc in country-info do
           (let ((idx (position country-desc all-countries :test #'string-equal)))
             (listbox-select countries-searchbox idx)))
      (setf (text primary-title-text-entry)  (getf title-info :primary-title))
      (setf (text original-title-text-entry) (getf title-info :original-title))
      (setf (text year-text-entry)           (encoded-datetime-year (getf title-info
                                                                          :year)))
      (setf (text runtime-text-entry)        (getf title-info :runtime))
      (setf (text tags-text-entry)           (getf title-info :tags))
      (setf (text notes-text-entry)          (getf title-info :notes))
      (setf (nodgui.mw:value vote-bar)       vote)
      (configure vote-scale :value  (nodgui.utils:->f (* +max-scale-vote+ vote)))
      (when (not (db-nil-p (getf title-info :image)))
        (let ((tga-data (load-image-in-button title-id image-button)))
          (setf image tga-data))))))

(defun make-add-movie-window (&optional (title-id nil))
  (with-modal-toplevel (toplevel :title (_ "Add a new movie"))
    (let ((frame (make-instance 'add-movie-frame
                                :container-win toplevel
                                :title-id      title-id
                                :master        toplevel)))
      (grid frame 0 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (gui-resize-grid-all toplevel))))
