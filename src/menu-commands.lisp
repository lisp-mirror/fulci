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

(in-package :menu-commands)

(define-constant +sql-dump-file-type+ '(("SQL" "*.sql")) :test #'equalp)

(defun help-about ()
  (with-modal-toplevel (toplevel :master nil :title (_ "About"))
    (let* ((editor  (make-text toplevel)))
      (setf (text editor)
	    (format nil +help-about-message-template+ +program-name+))
      (pack editor))))

(defun quit ()
  (db-utils:close-db)
  (exit-nodgui))

(defun sql-dump-default-filename ()
  (format nil "~a-~a.sql"  +program-name+ (decode-date-string (local-time-obj-now))))

(defun copy-db-fn (parent)
  (lambda ()
    (let ((destination (get-save-file :parent parent
                                      :title  (_ "Choose file"))))
      (when (not (string-empty-p destination))
        (with-busy* (*tk*)
          (fs:copy-a-file (db-utils:db-path) destination :overwrite t))
        (info-operation-completed *tk*)))))

(defun dump-db-fn (parent)
  (lambda ()
    (let* ((destination        (get-save-file :initial-file (sql-dump-default-filename)
                                              :file-types   +sql-dump-file-type+
                                              :parent       parent
                                              :title        (_ "Choose file")))
           (command-error-code 0)
           (shell-command      (format nil "~a ~a .dump > ~a" +sqlite-bin+
                                       (db-utils:db-path)
                                       destination)))
    (when (not (string-empty-p destination))
        (with-busy* (*tk*)
          (setf command-error-code (launch-command shell-command t nil)))
        (when (command-terminated-no-error-p command-error-code)
          (nodgui:do-msg (_ "The database has been serialized to filesystem")
            :title (_ "Dump completed")))))))

(define-constant +tsv-allowed-extension+ '(("Tab separated value" "*.tsv")
                                           ("Tab separated value" "*.csv")
                                           ("Tab separated value" "*.data"))
  :test #'equalp)

(defun tsv-choose-file-fn (parent text-entry)
  #'(lambda ()
      (let ((file (get-open-file :file-types +tsv-allowed-extension+
                                 :title      (_ "Choose file")
                                 :parent     parent)))
        (setf (text text-entry) file))))

(defun actual-import (parent-widget
                      progress-label
                      progress-bar
                      genres
                      persons
                      persons-professions
                      titles
                      min-year
                      max-year
                      min-runtime
                      crew
                      link-genre-p)
  (db-utils:with-ready-database (:connect nil)
    (labels ((set-progress (v)
               (let ((nodgui::*max-line-length* nil))
                 (sleep 0.0001) ; give GUI a break
                 (with-atomic
                     (setf (value progress-bar) v))))
             (progress-bar-fn (frac)
               (set-progress (* 100.0 frac)))
             (update-progress-label (new-data)
               (setf (text progress-label)
                     (strcat (text progress-label) " " new-data))))
      (with-busy* (parent-widget)
        (set-progress 0.0)
        (when (not (string-empty-p genres))
          (import-tsv:import-genres genres :progress-fn #'progress-bar-fn)
          (update-progress-label (_ "Done: import genres")))
        (set-progress 0.0)
        (when (not (string-empty-p persons))
          (let* ((cleaned (regex-replace-all "[^\\p{Letter}\\,]" persons-professions ""))
                 (allowed (mapcar (lambda (a) (strcat "(?i)" a))
                                  (remove-if #'string-empty-p
                                             (split "," cleaned)))))
            (import-tsv:import-persons persons
                                       :allowed-professions allowed
                                       :progress-fn #'progress-bar-fn)
            (update-progress-label (_ "Done: import persons"))))
        (set-progress 0.0)
        (when (not (string-empty-p titles))
          (import-tsv:import-titles titles
                                    :min-year     min-year
                                    :max-year     max-year
                                    :min-duration min-runtime
                                    :progress-fn  #'progress-bar-fn)
          (update-progress-label (_ "Done: import titles")))
        (set-progress 0.0)
        (when link-genre-p
          (import-tsv:link-titles-genres titles
                                         :progress-fn #'progress-bar-fn)
          (update-progress-label (_ "Done: link titles to genres")))
        (set-progress 0.0)
        (when (not (string-empty-p crew))
          (import-tsv:link-crew crew
                                :progress-fn #'progress-bar-fn)
          (update-progress-label (_ "Done: import crew")))
        (do-msg (_ "Import operation succefully!") :title (_ "Success") :parent parent-widget)))))

(defun start-import (parent
                     l-progress
                     progress
                     e-genres
                     e-persons
                     e-persons-professions
                     e-titles
                     sp-min-year
                     sp-max-year
                     e-min-runtime
                     e-link-title-persons
                     ck-link-title-genre)

  #'(lambda ()
      (flet ((2nl ()
               (format nil "~2%")))
        (let* ((genres              (text e-genres))
               (persons             (text e-persons))
               (persons-professions (text e-persons-professions))
               (titles              (text e-titles))
               (min-year            (safe-parse-integer (text sp-min-year)))
               (max-year            (safe-parse-integer (text sp-max-year)))
               (min-runtime         (safe-parse-integer (text e-min-runtime)
                                                        :fix-fn (lambda (a)
                                                                  (declare (ignore a)) 90)))
               (link-crew           (text  e-link-title-persons))
               (link-genre-p        (value ck-link-title-genre))
               (operation (_ "WARNING: this operation could takes a lot of time to complete, with a magnitude going from HOURS to DAYS in the worst case and could render your system instable "))
               (actual-min-year (or min-year (current-year)))
               (actual-max-year (or max-year (current-year))))
          (if (< actual-max-year
                 actual-min-year)
              (swap actual-max-year
                    actual-min-year))
          (when (not (string-empty-p genres))
            (setf operation (strcat operation
                                    (2nl)
                                    (_ "A file will be imported to refresh all movie genres"))))
          (when (not (string-empty-p persons))
            (setf operation (strcat operation
                                    (2nl)
                                    (_ "A file will be imported to refresh all persons."))))
          (when (not (string-empty-p titles))
            (setf operation (strcat operation
                                    (2nl)
                                    (_ "A file will be imported to refresh all movies, ")
                                    (format nil
                                            (_ "from year ~a to year ~a.")
                                            actual-min-year
                                            actual-max-year))))
          (when link-genre-p
            (setf operation
                  (strcat operation
                          (2nl)
                          (_ "A file will be scanned to attribute genres to each movie"))))
          (when (not (string-empty-p link-crew))
            (setf operation
                  (strcat operation
                          (2nl)
                          (_ "A file will be scanned to attribute directors to movies"))))
          (setf operation
                (strcat operation
                        (2nl)
                        (_ "Really start this operation?")))
          (when (ask-yesno operation
                           :parent parent
                           :title  (_ "Confirm operation")
                           :parent parent)
            (actual-import parent
                           l-progress
                           progress
                           genres
                           persons
                           persons-professions
                           titles
                           min-year
                           max-year
                           min-runtime
                           link-crew
                           link-genre-p))))))

(defun import-tsv-window ()
  (with-modal-toplevel (tl :title (_ "Import Tab Separated Values file"))
    (let* ((l-genres                   (make-instance 'label
                                                      :master tl
                                                      :text (_ "Genres file:")))
           (e-genres                   (make-instance 'entry  :master tl))
           (b-genres                   (make-instance 'button
                                                      :master tl
                                                      :text (_ "Choose file")
                                                      :command (tsv-choose-file-fn tl
                                                                                   e-genres)))
           (l-persons                  (make-instance 'label
                                                      :master tl
                                                      :text (_ "Persons file:")))
           (e-persons                  (make-instance 'entry  :master tl))
           (b-persons                  (make-instance 'button
                                                      :master tl
                                                      :text (_ "Choose file")
                                                      :command (tsv-choose-file-fn tl
                                                                                   e-persons)))
           (l-persons-professions-text (_ "Allowed professions (comma separated values):"))
           (l-persons-professions      (make-instance 'label
                                                      :master tl
                                                      :text  l-persons-professions-text))
           (e-persons-professions      (make-instance 'entry
                                                      :text   "Director"
                                                      :master tl))
           (l-titles                   (make-instance 'label
                                                      :master tl
                                                      :text (_ "Titles file:")))
           (e-titles                   (make-instance 'entry :master tl))
           (b-titles                   (make-instance 'button
                                                      :master tl
                                                      :text (_ "Choose file")
                                                      :command (tsv-choose-file-fn tl
                                                                                   e-titles)))
           (l-min-year                 (make-instance 'label
                                                      :master tl
                                                      :text   (_ "Minimum year to import from:")))
           (sp-min-year                (make-instance 'spinbox
                                                      :text   (misc:current-year)
                                                      :master tl
                                                      :from   1900
                                                      :to     (misc:current-year)))
           (l-max-year                 (make-instance 'label
                                                      :master tl
                                                      :text   (_ "Maximum year to import from:")))
           (sp-max-year                (make-instance 'spinbox
                                                      :text   (misc:current-year)
                                                      :master tl
                                                      :from   1900
                                                      :to     (misc:current-year)))
           (l-link-title-persons       (make-instance 'label
                                                      :text   (_ "Crew file:")
                                                      :master tl))
           (e-link-title-persons       (make-instance 'entry
                                                      :master tl))
           (b-link-title-persons       (make-instance 'button
                                                      :master  tl
                                                      :text    (_ "Choose file")
                                                      :command
                                                      (tsv-choose-file-fn tl
                                                                          e-link-title-persons)))
           (l-min-runtime              (make-instance 'label
                                                      :text   (_ "Minimum runtime in minutes:")
                                                      :master tl))
           (e-min-runtime              (make-instance 'entry
                                                      :text "90"
                                                      :master tl))
           (ck-link-title-genre        (make-instance 'check-button
                                                      :text
                                                      (_ "Associate title with genre")
                                                      :master tl
                                                      :initial-value  nil))
           (l-progress                 (make-instance 'label
                                                      :master tl
                                                      :text (_ "Progress")))
           (progress                   (make-instance 'progressbar
                                                      :master tl
                                                      :initial-value 0.0))
           (bottom-frame               (make-instance 'frame
                                                      :master tl))
           (b-start                    (make-instance 'button
                                                      :master  bottom-frame
                                                      :text    (_ "Start import")))
           (b-cancel                   (make-instance 'button
                                                      :master  bottom-frame
                                                      :text    (_ "Cancel")
                                                      :command (lambda ()
                                                                 (nodgui:exit-from-modal-toplevel tl)))))
      (grid l-genres              0 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid e-genres              0 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid b-genres              0 2 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-persons             1 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid e-persons             1 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid b-persons             1 2 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-persons-professions 2 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid e-persons-professions 2 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-titles              3 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid e-titles              3 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid b-titles              3 2 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-min-year            4 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid sp-min-year           4 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-max-year            5 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid sp-max-year           5 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-min-runtime         6 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid e-min-runtime         6 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-link-title-persons  7 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid e-link-title-persons  7 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid b-link-title-persons  7 2 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid ck-link-title-genre   8 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid l-progress            9 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid progress             10 0
            :sticky     :nswe
            :columnspan 3
            :padx       +min-padding+
            :pady       +min-padding+)
      (grid bottom-frame         11 0
            :sticky     :nswe
            :columnspan 3
            :padx       +min-padding+
            :pady       +min-padding+)
      (grid b-start               0 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (grid b-cancel              0 1 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (nodgui-utils:gui-resize-grid-all tl)
      (setf (command b-start)
            (start-import tl
                          l-progress
                          progress
                          e-genres
                          e-persons
                          e-persons-professions
                          e-titles
                          sp-min-year
                          sp-max-year
                          e-min-runtime
                          e-link-title-persons
                          ck-link-title-genre)))))

(defun import-from-sql-fn (parent)
  (lambda ()
    (when (nodgui:ask-yesno
           (_ "This operation will empty your old database and will replace it with the imported data. Do you want to continue? Please ensure that the backup comes from a trusted source because this operation will read and execute any SQL command in the file.")
           :title  (_ "Warning")
           :parent parent)
      (let ((source (get-open-file :file-types +sql-dump-file-type+
                                   :title      (_ "Choose a file")
                                   :parent     parent)))
        (when (not (string-empty-p source))
          (with-busy* (*tk*)
            (with-open-file (stream source)
              (db:delete-database)
              (loop
                 for i = (read-line stream nil nil)
                 while i do
                   (db-utils:query-low-level i))
              (nodgui:do-msg (_ "The database has been restored.")
                :title (_ "Load completed")))))))))

(defun import-from-db-fn (parent)
  (lambda ()
    (when (nodgui:ask-yesno
           (_ "This operation will empty your old database and will replace it with the imported data, after the program will quit. Do you want to continue?")
           :title  (_ "Warning")
           :parent parent)
      (let ((source (get-open-file :title (_ "Choose a file")
                                   :parent parent)))
        (when (not (string-empty-p source))
          (with-busy* (*tk*)
            (fs:copy-a-file source (db-utils:db-path) :overwrite t)
            (nodgui:do-msg (_ "The database has been restored, restart the program is necessary now.")
              :title (_ "Load completed"))
            (quit)))))))

(defun export-tsv-titles ()
  (let ((key  (text-input-dialog *tk*
                                 (_ "Search")
                                 (_ "Please insert the search criteria")
                                 :button-message (_ "OK")))
        (file (get-save-file :file-types +tsv-allowed-extension+
                             :title      (_ "Save")
                             :parent     *tk*)))
    (when file
      (with-busy* (*tk*)
        (cond
          ((string-empty-p key)
           (nodgui-utils:error-dialog *tk* (_ "Empty search criteria")))
          (t
           (let ((rows (db:search-movies key :title-id :asc)))
             (fs:dump-sequence-to-file (rows->tsv rows) file))))
        (info-operation-completed *tk*)))))

(defun export-tsv-copies ()
  (let ((key  (text-input-dialog *tk*
                                 (_ "Search")
                                 (_ "Please insert the search criteria")
                                 :button-message (_ "OK")))
        (file (get-save-file :file-types +tsv-allowed-extension+
                             :title      (_ "Save")
                             :parent     *tk*)))
    (when file
      (with-busy* (*tk*)
        (cond
          ((string-empty-p key)
           (nodgui-utils:error-dialog *tk* (_ "Empty search criteria")))
          (t
           (let ((rows (db:search-copies key :copy-id :desc)))
             (fs:dump-sequence-to-file (rows->tsv rows) file)))))
      (info-operation-completed *tk*))))

(defstruct copy-problem
  (id)
  (title)
  (problems))

(defun all-copies-with-problems ()
  (flet ((decode-problem (row key error)
           (if (null (getf row key))
               error
               "")))
    (let ((rows (db:copies-with-problems)))
      (loop for row in rows collect
           (make-copy-problem :id       (getf row :title-id)
                              :title    (getf row :pt)
                              :problems
                              (join-with-strings* (format nil "~%")
                                                  (decode-problem row
                                                                  :director
                                                                  (_ "Missing director"))
                                                  (decode-problem row
                                                                  :image
                                                                  (_ "Missing image"))
                                                  (decode-problem row
                                                                  :country
                                                                  (_ "Missing country"))
                                                  (decode-problem row
                                                                  :runtime
                                                                  (_ "Missing runtime"))))))))

(defun import-find-problem-copies-fn (main-window)
  (lambda ()
    (let ((all-copies (all-copies-with-problems)))
      (if (null all-copies)
          (info-dialog main-window (_ "No copies with problems found"))
          (with-modal-toplevel (toplevel :title (_ "Check copies"))
            (with-busy* (main-window)
              (let* ((main-frame   (make-instance 'scrolled-frame
                                                  :master toplevel))
                     (actual-frame (interior main-frame)))
                (grid main-frame 0 0 :sticky :news)
                (gui-resize-grid-all main-frame)
                (loop
                   for row from 0
                   for copy in all-copies do
                     (let* ((frame  (make-instance 'labelframe
                                                   :master actual-frame
                                                   :text   (copy-problem-title copy)))
                            (info   (make-instance 'label
                                                   :text   (copy-problem-problems copy)
                                                   :master frame))
                            (id     (copy-problem-id copy))
                            (button (make-instance 'button
                                                   :text   (_ "Edit")
                                                   :master frame
                                                   :command
                                                   (lambda ()
                                                     (manage-movie:make-add-movie-window id)))))
                       (grid frame  row 0 :sticky :news :pady +min-padding+)
                       (grid info   0   0 :sticky :nw   :padx (* 10 +min-padding+))
                       (grid button 0   1 :sticky :e))))))))))
