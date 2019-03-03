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

(define-constant +columns-copies-search-format+   "ft" :test #'string=)

(define-constant +columns-copies-search-position+ "po" :test #'string=)

(define-constant +columns-copies-width+       '(50  350 350 150 80 100 60  140 400)
                                              ;; id it  ot  dir ye  ge  ft  pos note
  :test #'equalp)

(defclass search-copies-frame (search-frame)
  ((barcodes-button
    :initform nil
    :accessor barcode-button)))

(defun delete-selected-dvd-clsr (search-res-widget)
  (lambda ()
    (delete-selected-entries search-res-widget db:+table-movie-copy+)))

(defun edit-copy-clsr (search-res-widget)
  (lambda ()
    (when-let ((selected (first (mapcar #'nodgui:id (treeview-get-selection search-res-widget)))))
      (manage-copies:make-add-copy-window nil selected))))

(defun edit-movie-from-copy-clsr (search-res-widget)
  (lambda ()
    (when-let ((selected (first (mapcar #'nodgui:id (treeview-get-selection search-res-widget)))))
      (let* ((copy  (db:fetch-from-any-id db:+table-movie-copy+ selected))
             (movie (db:fetch-from-any-id db:+table-title+ (getf copy :title))))
        (manage-movie:make-add-movie-window (getf movie :id))))))

(defun build-barcodes-clsr (search-res-widget)
  (lambda ()
    (let* ((selected    (mapcar #'nodgui:id (treeview-get-selection search-res-widget)))
           (labels-rows (mapcar (lambda (a) (db:copy-id->titles a)) selected))
           (all-labels  (mapcar (lambda (a) (format nil
                                                    "~a/~a"
                                                    (ellipsize (getf a :pt) :len 15)
                                                    (ellipsize (getf a :ot) :len 15)))
                                labels-rows)))
      (when all-labels
        (let ((destination (get-save-file :parent     *tk*
                                          :file-types '(("PostScript Files" "*.ps"))
                                          :title      (_ "Choose file"))))
          (when destination
            (with-busy* (*tk*)
              (let ((res (ps-utils:render-barcode-table selected all-labels
                                                        (pref:preferences-page-width)
                                                        (pref:preferences-page-height)
                                                        (pref:preferences-barcode-width)
                                                        (pref:preferences-barcode-height))))
                (fs:dump-sequence-to-file res destination)
                (info-operation-completed *tk*)
                (when (string-not-empty-p (pref:preferences-gv-bin))
                  (launch-command (format nil "cat \"~a\" | ~a -"
                                          destination (pref:preferences-gv-bin))))))))))))

(defun search-copies-column-values (row)
  (let* ((year   (if (db-nil-p (getf row :year))
                     (_ "unknown")
                     (encoded-datetime-year (getf row :year)))))
    (flet ((escape (row key)
             (escape-tilde (db-utils:db-nil->lisp (getf row key))))
           (escape-notes (row)
             (ellipsize (escape-tilde (db-nil->lisp (getf row
                                                          db:+search-expr-notes-col+)))
                        :len +max-char-column-notes+)))
      (list (escape row db:+search-expr-primary-title-col+)
            (escape row db:+search-expr-original-title-col+)
            (escape row db:+search-expr-director-col+)
            year
            (escape row db:+search-expr-genres-col+)
            (escape row db:+search-expr-format-col+)
            (join-with-strings* "/"
                                (escape row db:+search-expr-building-col+)
                                (escape row db:+search-expr-room-col+)
                                (escape row db:+search-expr-storage-col+)
                                (escape row db:+search-expr-shelf-col+))
            (escape-notes row)))))

(defun search-copies-clsr (entry search-results-widget order-columns)
  (lambda (e)
    (declare (ignore e))
    (dump-search-history entry)
    (let ((*search-order* :asc))
      (funcall (search-copies-command entry search-results-widget order-columns)))))

(defun search-copies-command (entry search-results-widget &optional (order-columns :copy-id))
  (lambda ()
    (with-busy* (*tk*)
      (with-inner-treeview (results-list search-results-widget)
        (treeview-delete-all results-list)
        (let* ((key (text entry))
               (res (db:search-copies key order-columns *search-order*)))
          (insert-item-in-search-results search-results-widget
                                         res
                                         :copy-id
                                         #'search-copies-column-values))))))

(defun search-results-order-copies-clsr (search-text-entry search-res-widget order-column)
  (lambda ()
    (flip-search-order)
    (funcall (search-copies-command search-text-entry
                                    search-res-widget
                                    order-column))))

(defun setup-search-res-copies-headers (search-text-entry search-res-widget)
  (treeview-heading search-res-widget
                    +treeview-first-column-id+
                    :text     (_ "ID"))
  (treeview-heading search-res-widget
                    +columns-movie-search-international-title+
                    :text    (_ "International title")
                    :command (search-results-order-copies-clsr search-text-entry
                                                               search-res-widget
                                                               :pt))
  (treeview-heading search-res-widget
                    +columns-movie-search-original-title+
                    :text    (_ "Original title")
                    :command (search-results-order-copies-clsr search-text-entry
                                                               search-res-widget
                                                               :ot))
  (treeview-heading search-res-widget
                    +columns-movie-search-director+
                    :text    (_ "Director")
                    :command (search-results-order-copies-clsr search-text-entry
                                                               search-res-widget
                                                               :director))
  (treeview-heading search-res-widget
                    +columns-movie-search-year+
                    :text    (_ "Production year")
                    :command (search-results-order-copies-clsr search-text-entry
                                                               search-res-widget
                                                               :year))
  (treeview-heading search-res-widget
                    +columns-movie-search-genre+
                    :text    (_ "Genre")
                    :command (search-results-order-copies-clsr search-text-entry
                                                               search-res-widget
                                                               db:+search-expr-genres-col+))
  (treeview-heading search-res-widget
                    +columns-copies-search-position+
                    :text    (_ "Position"))
  (treeview-heading search-res-widget
                    +columns-copies-search-format+
                    :text    (_ "Format")
                    :command (search-results-order-copies-clsr search-text-entry
                                                               search-res-widget
                                                               db:+search-expr-format-col+))
  (treeview-heading search-res-widget
                    +columns-movie-search-notes+
                    :text    (_ "Notes")
                    :command (search-results-order-copies-clsr search-text-entry
                                                               search-res-widget
                                                               db:+search-expr-notes-col+)))

(defun make-details-copy-window (copy-id)
  (let* ((copy-info (db:fetch-from-any-id db:+table-movie-copy+ copy-id))
         (format    (db:copy-row->format-description copy-info)))
    (nodgui-utils:with-title-details-toplevel (title (getf copy-info :title) ; title id
                                                     (_ "Copy details")
                                                     notes-entry)
      (let* ((formats-desc   (make-instance 'label
                                            :font   +font-h3+
                                            :text   format
                                            :master nil))
             (formats-label (make-instance 'label
                                           :text   (_ "Format")
                                           :font   +font-h2+
                                           :master nil)))
        (setf (text notes-entry) (getf copy-info :notes))
        (grid formats-label (+ (max-row) 1) (max-col)
              :sticky :we :padx +min-padding+ :pady +min-padding+)
        (grid formats-desc (+ (max-row) 2) (max-col)
              :sticky :we :padx +min-padding+ :pady +min-padding+)))))

(defun details-copies-clsr (search-res-widget)
  (lambda ()
    (let ((selected (mapcar #'nodgui:id (treeview-get-selection search-res-widget))))
      (with-busy* (*tk*)
        (loop for id in selected do
             (make-details-copy-window id))))))

(defmethod initialize-instance :after ((object search-copies-frame) &key &allow-other-keys)
  (with-accessors ((search-text-entry      search-text-entry)
                   (search-results         search-results)
                   (search-button          search-button)
                   (delete-selected-button delete-selected-button)
                   (add-item-button        add-item-button)
                   (edit-item-button       edit-item-button)
                   (barcode-button         barcode-button)
                   (details-item-button    details-item-button)) object
    (setf search-text-entry (make-instance 'history-entry
                                           :compare-history-candidate-predicate #'string-equal
                                           :text (_ "search title, director, year...")
                                           :master object))
    (setf search-results (make-instance 'scrolled-treeview
                                        :columns (list +columns-movie-search-international-title+
                                                       +columns-movie-search-original-title+
                                                       +columns-movie-search-director+
                                                       +columns-movie-search-year+
                                                       +columns-movie-search-genre+
                                                       +columns-copies-search-format+
                                                       +columns-copies-search-position+
                                                       +columns-movie-search-notes+)
                                        :columns-width +columns-copies-width+
                                        :master        object))
    (setup-search-res-copies-headers search-text-entry search-results)
    (bind search-text-entry #$<Enter>$  (search-entry-on-enter-cb    search-text-entry))
    (bind search-text-entry #$<Key>$    (search-entry-on-keypress-cb search-text-entry))
    (bind search-text-entry #$<Return>$ (search-copies-clsr search-text-entry
                                                            search-results
                                                            :copy-id))
    (revive-history search-text-entry)
    (setf search-button  (make-instance 'button
                                        :image   *icon-search*
                                        :command (search-copies-command search-text-entry
                                                                        search-results)
                                        :master object))
    (let* ((toolbar-frame     (make-instance 'frame
                                            :master object))
           (edit-movie-button (make-instance 'button
                                             :master  toolbar-frame
                                             :image   *icon-edit-movie*
                                             :command (edit-movie-from-copy-clsr search-results))))
      (setf delete-selected-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-delete-dvd*
                           :command (delete-selected-dvd-clsr search-results)))
      (setf edit-item-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-edit-dvd*
                           :command (edit-copy-clsr search-results)))
      (setf barcode-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-barcode*
                           :command (build-barcodes-clsr search-results)))
      (setf details-item-button
            (make-instance 'button
                           :master  toolbar-frame
                           :image   *icon-details*
                           :command (details-copies-clsr search-results)))
      (attach-tooltips (delete-selected-button (_ "delete selected"))
                       (edit-movie-button      (_ "edit title"))
                       (edit-item-button       (_ "edit copy"))
                       (barcode-button         (_ "generate copies barcode"))
                       (details-item-button    (_ "movie copy details")))
      (grid delete-selected-button 0 1)
      (grid edit-item-button       0 2)
      (grid edit-movie-button      0 3)
      (grid details-item-button    0 4)
      (grid barcode-button         0 5)
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
