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

(in-package :search-frame)

(cl-syntax:use-syntax 'nodgui.event-parser:nodgui-event-syntax)

(define-constant +columns-movie-search-id+                   "id" :test #'string=)

(define-constant +columns-movie-search-international-title+  "it" :test #'string=)

(define-constant +columns-movie-search-original-title+       "ot" :test #'string=)

(define-constant +columns-movie-search-director+             "di" :test #'string=)

(define-constant +columns-movie-search-year+                 "ye" :test #'string=)

(define-constant +columns-movie-search-tags+                 "ta" :test #'string=)

(define-constant +columns-movie-search-genre+                "ge" :test #'string=)

(define-constant +columns-movie-search-notes+                "no" :test #'string=)

(define-constant +max-char-column-length+                      10 :test #'=)

(define-constant +max-char-column-notes+                      100 :test #'=)

(define-constant +columns-width+       '(100 400 400 200 150 200 200 200)
                                       ;; id it  ot  dir ye  tag ge  note
  :test #'equalp)

(defparameter *search-order* :asc)

(defclass search-title-frame (frame)
  ((main-frame
    :initform nil
    :initarg  :main-frame
    :accessor main-frame)
   (search-text-entry
    :initform nil
    :accessor search-text-entry)
   (search-results
    :initform nil
    :accessor search-results
    :type     scrolled-treeview)
   (search-button
    :initform nil
    :accessor search-button
    :type     button)
   (delete-selected-button
    :initform nil
    :accessor delete-selected-button
    :type     button)
   (add-item-button
    :initform nil
    :accessor add-item-button
    :type     button)
   (add-copy-button
    :initform nil
    :accessor add-copy-button
    :type     button)
   (goto-copy-button
    :initform nil
    :accessor goto-copy-button
    :type     button)
   (edit-item-button
    :initform nil
    :accessor edit-item-button
    :type     button)
   (details-item-button
    :initform nil
    :accessor details-item-button
    :type     button)))

(defun flip-search-order ()
  (if (eq *search-order* :desc)
      (setf *search-order* :asc)
      (setf *search-order* :desc)))

(defun search-movie-column-values (row)
  (let* ((year   (if (db-nil-p (getf row :year))
                     (_ "unknown")
                     (encoded-datetime-year (getf row :year)))))
    (flet ((escape (row key)
             (db-utils:db-nil->lisp (getf row key)))
           (escape-notes (row)
             (ellipsize (db-nil->lisp (getf row
                                            db:+search-expr-notes-col+))
                        :len +max-char-column-length+)))
      (list (escape row db:+search-expr-primary-title-col+)
            (escape row db:+search-expr-original-title-col+)
            (escape row db:+search-expr-director-col+)
            year
            (escape row db:+search-expr-genres-col+)
            (escape row db:+search-expr-tags-col+)
            (escape-notes row)))))

(defun insert-item-in-search-results (widget results-rows row-id build-values-columns-fn)
  (loop for row in results-rows do
       (let ((movie-id (getf row row-id))
             (column-values (funcall build-values-columns-fn row)))
         (treeview-insert-item-new widget
                                   :test          (lambda (a b)
                                                    (= (id a) (id b)))
                                   :text          movie-id
                                   :column-values column-values
                                   :id            movie-id))))

(defun search-movie-command (entry search-results-widget &optional (order-columns :title-id))
  (lambda ()
    (with-busy* (*tk*)
      (with-inner-treeview (results-list search-results-widget)
        (treeview-delete-all results-list)
        (let* ((key (text entry))
               (res (db:search-movies key order-columns *search-order*)))
          (insert-item-in-search-results search-results-widget
                                         res
                                         :title-id
                                         #'search-movie-column-values))))))

(defun search-results-order-movie-clsr (search-text-entry search-res-widget order-column)
  (lambda ()
    (flip-search-order)
    (funcall (search-movie-command search-text-entry
                                   search-res-widget
                                   order-column))))

(defun setup-search-res-movie-headers (search-text-entry search-res-widget)
  (treeview-heading search-res-widget
                    +treeview-first-column-id+
                    :text     (_ "ID"))
  (treeview-heading search-res-widget
                    +columns-movie-search-international-title+
                    :text    (_ "International title")
                    :command (search-results-order-movie-clsr search-text-entry
                                                              search-res-widget
                                                              :pt))
  (treeview-heading search-res-widget
                    +columns-movie-search-original-title+
                    :text    (_ "Original title")
                    :command (search-results-order-movie-clsr search-text-entry
                                                              search-res-widget
                                                              :ot))
  (treeview-heading search-res-widget
                    +columns-movie-search-director+
                    :text    (_ "Director")
                    :command (search-results-order-movie-clsr search-text-entry
                                                              search-res-widget
                                                              :director))
  (treeview-heading search-res-widget
                    +columns-movie-search-year+
                    :text    (_ "Production year")
                    :command (search-results-order-movie-clsr search-text-entry
                                                              search-res-widget
                                                              :year))
  (treeview-heading search-res-widget
                    +columns-movie-search-genre+
                    :text    (_ "Genre")
                    :command (search-results-order-movie-clsr search-text-entry
                                                              search-res-widget
                                                              db:+search-expr-genres-col+))
  (treeview-heading search-res-widget
                    +columns-movie-search-tags+
                    :text    (_ "Tags")
                    :command (search-results-order-movie-clsr search-text-entry
                                                              search-res-widget
                                                              db:+search-expr-tags-col+))
  (treeview-heading search-res-widget
                    +columns-movie-search-notes+
                    :text    (_ "Notes")
                    :command (search-results-order-movie-clsr search-text-entry
                                                              search-res-widget
                                                              db:+search-expr-notes-col+)))

(defun delete-selected-entries (search-res-widget table)
  (let ((selected (mapcar #'nodgui:id (treeview-get-selection search-res-widget))))
    (when (and selected
               (confirm-deletion *tk* (length selected)))
      (with-busy* (*tk*)
        (treeview-delete search-res-widget selected)
        (loop for i in selected do (db:delete-by-id table i)))
      (info-operation-completed *tk*))))

(defun delete-selected-movies-fn (search-res-widget)
  (lambda ()
    (delete-selected-entries search-res-widget db:+table-title+)))

(defun edit-movie-clsr (search-res-widget)
  (lambda ()
    (let ((selected (first (mapcar #'nodgui:id (treeview-get-selection search-res-widget)))))
      (manage-movie:make-add-movie-window selected))))

(defun add-movie-cb ()
  (manage-movie:make-add-movie-window))

(defun add-copy-clsr (search-res-widget)
  (lambda ()
    (let ((selected (first (mapcar #'nodgui:id (treeview-get-selection search-res-widget)))))
      (when selected
        (manage-copies:make-add-copy-window selected)))))

(defun search-movie-cb (search-text-entry search-results)
  (funcall (search-movie-command search-text-entry
                                 search-results)))

(defun dump-search-history (entry)
  (let ((all-lines (fs:file->list-lines (history-path)))
        (text      (text entry)))
    (when (not (find text all-lines :test #'string-equal))
      (if (< (length all-lines)
             +max-history-items-count+)
          (fs:append-line->file text (history-path))
          (fs:list-lines->file (append all-lines (list text))
                               (history-path)))
      (revive-history entry))))

(defun revive-history (entry)
  (setf (nodgui.mw:history entry)
        (reverse (fs:file->list-lines (history-path)))))

(defun search-movie-entry-cb (search-text-entry search-results)
  (lambda (a)
    (declare (ignore a))
    (add-history search-text-entry (text search-text-entry))
    (dump-search-history search-text-entry)
    (let ((*search-order* :asc))
      (search-movie-cb search-text-entry search-results))))

(defun search-movie-button-cb (search-text-entry search-results)
  (lambda ()
    (search-movie-cb search-text-entry search-results)))

(defun search-entry-on-enter-cb (entry)
  (lambda (a)
    (declare (ignore a))
    (focus entry)))

(defun search-entry-on-keypress-cb (entry)
  (let ((first-time-p t))
    (lambda (e)
      (when first-time-p
        (focus entry)
        (setf  first-time-p nil)
        (if (nodgui.event-symbols:keysym-printable-p (nodgui:event-char-code e))
            (setf (text entry) (safe-subseq (text entry) 0 1))
            (setf (text entry) ""))))))

(defun make-details-movie-window (title-id)
  (nodgui-utils:with-title-details-toplevel (title title-id (_ "Title details") notes-entry)))

(defun treeitem-international-title (item)
  (first-elt (column-values item)))

(defun details-titles-clsr (search-res-widget)
  (lambda ()
    (let ((selected (mapcar #'nodgui:id (treeview-get-selection search-res-widget))))
      (with-busy* (*tk*)
        (loop for id in selected do
             (make-details-movie-window id))))))

(defun goto-copy-clsr (frame main-frame search-results-widget)
  (lambda ()
    (when-let* ((search-copy-frame (main-frame:search-copies-tab main-frame))
                (selected          (first (treeview-get-selection search-results-widget)))
                (title             (treeitem-international-title selected)))
      (notebook-select (master frame) search-copy-frame)
      (setf (text (search-text-entry search-copy-frame)) title)
      (funcall (search-copies-command (search-text-entry search-copy-frame)
                                      (search-results    search-copy-frame))))))

(defun make-search-results-widget (master &optional (column-width-fn #'identity))
  (make-instance 'scrolled-treeview
                 :columns (list +columns-movie-search-international-title+
                                +columns-movie-search-original-title+
                                +columns-movie-search-director+
                                +columns-movie-search-year+
                                +columns-movie-search-genre+
                                +columns-movie-search-tags+
                                +columns-movie-search-notes+)
                 :columns-width (mapcar column-width-fn +columns-width+)
                 :master        master))

(defun make-search-titles-entry (master
                                 &key (initial-text (_ "search title, director, year...")))
  (make-instance 'history-entry
                 :compare-history-candidate-predicate #'string-equal
                 :text                                initial-text
                 :master                              master))

(defmethod initialize-instance :after ((object search-title-frame) &key &allow-other-keys)
  (with-accessors ((main-frame             main-frame)
                   (search-text-entry      search-text-entry)
                   (search-results         search-results)
                   (search-button          search-button)
                   (delete-selected-button delete-selected-button)
                   (add-item-button        add-item-button)
                   (edit-item-button       edit-item-button)
                   (add-copy-button        add-copy-button)
                   (goto-copy-button       goto-copy-button)
                   (details-item-button    details-item-button)) object
    (setf search-text-entry (make-search-titles-entry object))
    (setf search-results (make-search-results-widget object))
    (setup-search-res-movie-headers search-text-entry search-results)
    (focus search-text-entry)
    (bind search-text-entry #$<Enter>$ (search-entry-on-enter-cb       search-text-entry))
    (bind search-text-entry #$<Key>$   (search-entry-on-keypress-cb    search-text-entry)
          :exclusive nil :append t)
    (bind search-text-entry #$<Return>$ (search-movie-entry-cb         search-text-entry
                                                                       search-results))
    (revive-history search-text-entry)
    (setf search-button  (make-instance 'button
                                        :image   *icon-search*
                                        :command (search-movie-command search-text-entry
                                                                       search-results)
                                        :master object))
    (let ((toolbar-frame (make-instance 'frame
                                        :master object)))
      (setf delete-selected-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-delete-movie*
                           :command (delete-selected-movies-fn search-results)))
      (setf add-item-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-add-movie*
                           :command #'add-movie-cb))
      (setf edit-item-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-edit-movie*
                           :command (edit-movie-clsr search-results)))
      (setf add-copy-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-add-dvd*
                           :command (add-copy-clsr search-results)))
      (setf goto-copy-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-goto-copy*
                           :command (goto-copy-clsr object main-frame search-results)))
      (setf details-item-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-details*
                           :command (details-titles-clsr search-results)))
      (attach-tooltips (delete-selected-button (_ "delete selected"))
                       (add-item-button        (_ "add title"))
                       (edit-item-button       (_ "edit title"))
                       (add-copy-button        (_ "add a copy"))
                       (goto-copy-button       (_ "lookup copy"))
                       (details-item-button    (_ "title details")))
      (grid add-item-button        0 0)
      (grid delete-selected-button 0 1)
      (grid edit-item-button       0 2)
      (grid details-item-button    0 4)
      (grid add-copy-button        0 5)
      (grid goto-copy-button       0 6)
      (grid search-text-entry
            0 0
            :sticky     :nswe
            :columnspan 7
            :padx       +min-padding+
            :pady       +min-padding+)
      (grid search-button
            0 7
            :sticky :nswe
            :padx   +min-padding+
            :pady   +min-padding+)
      (grid toolbar-frame
            1 0
            :sticky     :nswe
            :columnspan 8
            :padx       +min-padding+
            :pady       +min-padding+)
      (grid search-results
            2 0
            :sticky     :nswe
            :columnspan 8
            :padx       +min-padding+
            :pady       +min-padding+)
      (grid-rowconfigure      object 2 :weight 1)
      (grid-columnconfigure   object 1 :weight 1))))
