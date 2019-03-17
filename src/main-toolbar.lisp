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

(in-package :main-toolbar)

(define-constant +max-ct-name-homonyms+ 1000 :test #'=)

(defun anc-table-selected-element (listbox)
  (when (listbox-get-selection-value listbox)
    (first-elt (listbox-get-selection-value listbox))))

(defun manage-ancillary-tables (table-name title
                                &key
                                  (to-alist-fn     #'db:id-desc-alist-from-table)
                                  (add-value-fn    #'db:add-description)
                                  (delete-value-fn #'db:delete-description)
                                  (update-value-fn #'db:update-description))
  (lambda ()
    (with-ready-database (:connect nil)
      (let ((all-data (funcall to-alist-fn table-name))
            (listbox  nil))
        (with-modal-toplevel (toplevel :title title)
          (labels ((refresh-data ()
                     (setf all-data (funcall to-alist-fn table-name))
                     (listbox-delete listbox)
                     (listbox-append listbox (mapcar #'cdr all-data)))
                   (find-data (needle)
                     (find needle all-data
                           :test #'string-equal
                           :key  #'cdr))
                   (add-item ()
                     (when (not (string-empty-p (search-text listbox)))
                       (if (find-data (search-text listbox))
                           (error-dialog toplevel (_ "This item already exists"))
                           (progn
                             (funcall add-value-fn table-name (search-text listbox))
                             (refresh-data)))))
                   (delete-item ()
                     (if (not (anc-table-selected-element listbox))
                         (error-dialog toplevel (_ "No item was selected"))
                         (progn
                           (funcall delete-value-fn
                                    table-name
                                    (anc-table-selected-element listbox))
                           (refresh-data))))
                   (update-item ()
                     (when-let* ((old (anc-table-selected-element listbox))
                                 (new (text-input-dialog toplevel
                                                         (_ "Edit item")
                                                         (_ "Please insert the new description")
                                                         :text           old
                                                         :button-message (_ "OK"))))
                       (when (every #'(lambda (a) (not (string-empty-p a)))
                                    (list old new))
                         (funcall update-value-fn table-name old new)
                         (refresh-data)))))
            (setf listbox (make-instance 'searchable-listbox
                                         :entry-label           (_ "Filter:")
                                         :master                toplevel
                                         :remove-non-matching-p t
                                         :matching-fn           (lambda (a b)
                                                                  (scan (strcat "(?i)" a) b))))
            (let* ((bottom-bar    (make-instance 'frame
                                                 :master toplevel))
                   (add-button    (make-instance 'button
                                                 :master  bottom-bar
                                                 :image   *icon-add-small*
                                                 :command #'add-item))
                   (delete-button (make-instance 'button
                                                 :master  bottom-bar
                                                 :image   *icon-delete-small*
                                                 :command #'delete-item))
                   (edit-button   (make-instance 'button
                                                 :master  bottom-bar
                                                 :image   *icon-edit-small*
                                                 :command #'update-item)))
              (refresh-data)
              (grid listbox       0 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
              (grid add-button    0 0 :sticky :ns)
              (grid delete-button 0 1 :sticky :ns)
              (grid edit-button   0 2 :sticky :ns)
              (grid bottom-bar    1 0 :sticky :n)
              (gui-resize-grid-all toplevel))))))))

(defun manage-persons-table (title to-alist-fn)
  (lambda ()
    (with-ready-database (:connect nil)
      (let ((all-data       (with-busy* (*tk*) (funcall to-alist-fn db:+table-person+)))
            (listbox        nil)
            (birthday-entry nil))
        (with-modal-toplevel (toplevel :title title)
          (labels ((refresh-data ()
                     (setf all-data (funcall to-alist-fn db:+table-person+))
                     (listbox-delete listbox)
                     (listbox-append listbox (mapcar #'cdr all-data)))
                   (find-data (name)
                     (or (find name all-data
                               :test #'string-equal
                               :key  #'cdr)
                         (db:find-person name)))
                   (birthday-not-empty-p (person-row)
                     (string-not-empty-p (getf person-row :birthday)))
                   (add-item ()
                     (nodgui-utils:with-entry-text-validate
                         (toplevel (birthday-entry validation:+pos-integer-re+
                                                (_ "Year must be a positive number")))
                       (when (not (string-empty-p (search-text listbox)))
                         (if (find-data (search-text listbox))
                             (error-dialog toplevel (_ "This item already exists"))
                             (loop named names for count from 1 below +max-ct-name-homonyms+ do
                                  (let ((actual-name (db:normalize-name count
                                                                        (search-text listbox)
                                                                        (text birthday-entry))))
                                    (when (not (db:find-person actual-name))
                                      (db:add-person db:+table-person+
                                                     actual-name
                                                     (text birthday-entry))
                                      (refresh-data)
                                      (return-from names t))))))))
                   (delete-item ()
                     (if (not (anc-table-selected-element listbox))
                         (error-dialog toplevel (_ "No item was selected"))
                         (progn
                           (db:delete-person db:+table-person+ (anc-table-selected-element listbox))
                           (refresh-data))))
                   (update-item ()
                     (when-let* ((old-name   (anc-table-selected-element listbox))
                                 (old-person (db:find-person old-name))
                                 (birthday   (getf old-person :birthday))
                                 (new-name
                                  (text-input-dialog toplevel
                                                     (_ "Edit item")
                                                     (_ "Please insert the new description")
                                                     :text           old-name
                                                     :button-message (_ "OK")))
                                 (new-year
                                  (text-input-dialog toplevel
                                                     (_ "Edit item")
                                                     (_ "Please insert the new year of birth")
                                                     :text (and (birthday-not-empty-p old-person)
                                                                (encoded-datetime-year birthday))
                                                     :button-message (_ "OK"))))
                       (cond
                         ((not (scan validation:+pos-integer-re+ new-year))
                          (error-dialog toplevel (_ "Year must be a positive number")))
                         ((not (scan validation:+free-text-re+ new-name))
                          (error-dialog toplevel (_ "Description empty")))
                         ((db:find-person new-name)
                          (error-dialog toplevel (_ "This person already exists in the database")))
                         (t
                          (db:update-person db:+table-person+ old-name new-name new-year)
                          (refresh-data))))))
            (setf listbox (make-instance 'searchable-listbox
                                         :entry-label           (_ "Filter:")
                                         :master                toplevel
                                         :remove-non-matching-p t
                                         :matching-fn           (lambda (a b)
                                                                  (scan (strcat "(?i)" a) b))))
            (setf birthday-entry (make-instance 'entry
                                                :master toplevel))
            (let* ((birthday-label (make-instance 'label
                                                  :text   (_ "Birth year")
                                                  :master toplevel))
                   (bottom-bar     (make-instance 'frame
                                                  :master toplevel))
                   (add-button     (make-instance 'button
                                                  :master  bottom-bar
                                                  :image   *icon-add-small*
                                                  :command #'add-item))
                   (delete-button  (make-instance 'button
                                                  :master  bottom-bar
                                                  :image   *icon-delete-small*
                                                  :command #'delete-item))
                   (edit-button    (make-instance 'button
                                                  :master  bottom-bar
                                                  :image   *icon-edit-small*
                                                  :command #'update-item)))
              (refresh-data)
              (grid listbox        0 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
              (grid birthday-label 1 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
              (grid birthday-entry 2 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
              (grid add-button     0 0 :sticky :ns)
              (grid delete-button  0 1 :sticky :ns)
              (grid edit-button    0 2 :sticky :ns)
              (grid bottom-bar     3 0 :sticky :n)
              (gui-resize-grid-all toplevel))))))))

(defclass main-toolbar (frame) ())

(defmethod initialize-instance :after ((object main-toolbar) &key &allow-other-keys)
  (flet ((person-button-cb ()
           (lambda ()
             (let ((filter (text-input-dialog *tk*
                                              (_ "Filter items")
                                              (_ "Please insert a prefix name filter (minimum length 3 characters)")
                                              :button-message (_ "OK"))))
               (if (or (string-empty-p filter)
                       (< (length filter) 3))
                   (error-dialog *tk* (_ "Please provide a prefix of three characters minimum"))
                   (let ((fn (funcall #'manage-persons-table
                                      (_ "Manage persons")
                                      (lambda (table)
                                        (declare (ignore table))
                                        (db:persons->alist (strcat filter "%"))))))
                     (funcall fn)))))))
    (let ((manage-persons   (make-instance 'button
                                           :image   *icon-persons*
                                           :command (person-button-cb)
                                           :master  object))
          (manage-support   (make-instance 'button
                                           :image   *icon-dvd-case*
                                           :command (manage-ancillary-tables
                                                     db:+table-movie-storage-format+
                                                     (_ "Manage physical supports"))
                                           :master  object))
          (manage-genres    (make-instance 'button
                                           :image   *icon-genre*
                                           :command (manage-ancillary-tables db:+table-genre+
                                                                             (_ "Manage genres"))
                                           :master object))
          (manage-countries (make-instance 'button
                                           :image   *icon-country*
                                           :command (manage-ancillary-tables db:+table-country+
                                                                             (_ "Manage countries"))
                                           :master object)))
      (attach-tooltips (manage-persons   (_ "manage people"))
                       (manage-support   (_ "manage storage format"))
                       (manage-genres    (_ "manage genres"))
                       (manage-countries (_ "manage countries")))
      (configure object :relief :raised)
      (grid manage-persons   0 0 :pady (* 2 +min-padding+))
      (grid manage-support   0 1 :pady (* 2 +min-padding+))
      (grid manage-genres    0 2 :pady (* 2 +min-padding+))
      (grid manage-countries 0 3 :pady (* 2 +min-padding+)))))
