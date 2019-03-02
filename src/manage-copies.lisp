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

(in-package :manage-copies)

(cl-syntax:use-syntax nodgui.event-parser:nodgui-event-syntax)

(defclass add-copy-frame (frame)
  ((primary-title-label
    :initform nil
    :accessor primary-title-label)
   (original-title-label
    :initform nil
    :accessor original-title-label
    :type     label)
   (directors-label
    :initform nil
    :accessor directors-label
    :type     label)
   (year-label
    :initform nil
    :accessor year-label
    :type     label)
   (barcode-text-label
    :initform nil
    :accessor barcode-text-label
    :type     label)
   (barcode-text-entry
    :initform nil
    :accessor barcode-text-entry
    :type     entry)
   (position-text-label
    :initform nil
    :accessor position-text-label
    :type     label)
   (position-text-entry
    :initform nil
    :accessor position-text-entry
    :type     entry)
   (notes-text-label
    :initform nil
    :accessor notes-text-label
    :type     label)
   (notes-text
    :initform nil
    :accessor notes-text
    :type     entry)
   (format-label
    :initform nil
    :accessor format-label
    :type     label)
   (formats-listbox
    :initform nil
    :accessor formats-listbox
    :type     searchable-listbox)
   (apply-button
    :initform nil
    :accessor apply-button
    :type     button)
   (close-button
    :initform nil
    :accessor close-button
    :type     button)
   (container-win
    :initform nil
    :initarg  :container-win
    :accessor container-win
    :type     toplevel)
   (title-id
    :initform nil
    :initarg :title-id
    :accessor title-id
    :type     integer)
   (copy-id
    :initform nil
    :initarg  :copy-id
    :accessor copy-id
    :type     integer)))

(defun update-mode-p (add-movie-frame)
  (copy-id add-movie-frame))

(defmacro with-all-accessors ((object) &body body)
  "Very anaphoric :)"
  `(with-accessors ((primary-title-label  primary-title-label)
                    (original-title-label original-title-label)
                    (directors-label      directors-label)
                    (year-label           year-label)
                    (barcode-text-label   barcode-text-label)
                    (barcode-text-entry   barcode-text-entry)
                    (position-text-label  position-text-label)
                    (position-text-entry  position-text-entry)
                    (notes-text-label     notes-text-label)
                    (notes-text           notes-text)
                    (format-label         format-label)
                    (formats-listbox      formats-listbox)
                    (apply-button         apply-button)
                    (close-button         close-button)
                    (title-id             title-id)
                    (copy-id              copy-id))             ,object
     ,@body))

(defun insert-copy (frame new-barcode new-position new-notes new-format)
  (with-all-accessors (frame)
    (let ((new-copy-id (db:add-new-copy title-id
                                        new-barcode
                                        new-position
                                        new-notes
                                        new-format)))
      (setf copy-id new-copy-id)
      (let ((msg (format nil (_ "Added new copy with id: ~a") new-copy-id)))
        (if (preferences:preferences-use-insert-mode)
            (nodgui.mw:message-with-timeout (container-win frame) msg 10 (_ "OK")
                                            :font +font-h2+)
            (info-dialog frame msg))))))

(defun update-copy (frame new-barcode new-position new-notes new-format)
  (with-all-accessors (frame)
    (db:update-copy copy-id new-barcode new-position new-notes new-format)
    (info-operation-completed frame)))

(defun add-copy-clsr (frame)
  (lambda ()
    (with-all-accessors (frame)
      (declare (ignore apply-button close-button))
      (with-entry-text-validate
          (frame (barcode-text-entry  +free-text-re+
                                      (_ "Barcode can not be empty"))
                 (position-text-entry +copy-position-re+
                                      (_ "Position must be four comma separated fields")))
        (let ((new-barcode   (text barcode-text-entry))
              (new-position  (text position-text-entry))
              (new-notes     (text notes-text))
              (new-format    (first (listbox-get-selection-value formats-listbox))))
          (if (null new-format)
              (error-dialog frame (_ "No format choosen"))
              (if (update-mode-p frame)
                  (update-copy frame new-barcode new-position new-notes new-format)
                  (insert-copy frame new-barcode new-position new-notes new-format))))))))

(defun select-listbox-format (formats-listbox format-description)
  (let* ((all-formats-in-listbox (listbox-all-values formats-listbox))
         (idx                    (position format-description
                                           all-formats-in-listbox
                                           :test #'string-equal)))
    (when idx
      (listbox-clear  formats-listbox)
      (listbox-select formats-listbox idx))))

(defmethod initialize-instance :after ((object add-copy-frame) &key &allow-other-keys)
  (with-all-accessors (object)
    (let* ((bottom-frame         (make-instance 'frame :master object))
           (top-frame            (make-instance 'labelframe
                                                :text (_ "Movie information")
                                                :master object))
           (primary-title-desc   (make-instance 'label
                                                :font "bold"
                                                :text (_ "Primary Title")
                                                :master top-frame))
           (original-title-desc   (make-instance 'label
                                                 :font "bold"
                                                 :text (_ "Original Title")
                                                 :master top-frame))
           (directors-desc        (make-instance 'label
                                                 :font "bold"
                                                 :text (_ "Direction")
                                                 :master top-frame))
           (year-desc             (make-instance 'label
                                                 :font "bold"
                                                 :text (_ "Production Year")
                                                 :master top-frame)))
      (setf primary-title-label  (make-instance 'label
                                                :master top-frame))
      (setf original-title-label (make-instance 'label
                                                :master top-frame))
      (setf directors-label      (make-instance 'label
                                                :master  top-frame))
      (setf year-label           (make-instance 'label
                                                :master  top-frame))
      (setf barcode-text-label   (make-instance 'label
                                                :text   (_ "Product barcode:")
                                                :master object))
      (setf barcode-text-entry   (make-instance 'entry
                                                :master object))
      (setf position-text-label  (make-instance 'label
                                                :text
                                                (_ "Position (building,room,storage,shelf):")
                                                :master object))
      (setf position-text-entry  (make-instance 'entry
                                                :master object))
      (setf notes-text-label     (make-instance 'label
                                                :text
                                                (_ "Notes:")
                                                :master object))
      (setf notes-text           (make-instance 'text
                                                :height 6
                                                :master object))
      (setf format-label         (make-instance 'label
                                                :text
                                                (_ "Format:")
                                                :master object))
      (setf formats-listbox      (make-instance 'searchable-listbox
                                                :entry-label       (_ "Search:")
                                                :export-selection  nil
                                                :select-mode      :browse
                                                :data             (db:all-formats-description)
                                                :master           object))
      (setf apply-button         (make-instance 'button
                                                :text    (_ "Apply")
                                                :command (add-copy-clsr object)
                                                :master  bottom-frame))
      (setf close-button         (make-instance 'button
                                                    :text    (_ "Close")
                                                    :command (lambda () (break-mainloop))
                                                    :master  bottom-frame))
      (setf (text position-text-entry) (pref:preferences-place))
      (select-listbox-format formats-listbox (pref:preferences-copy-format))
      (grid top-frame             0 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid primary-title-desc    0 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid primary-title-label   1 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid original-title-desc   2 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid original-title-label  3 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid year-desc             4 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid year-label            5 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid directors-desc        6 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid directors-label       7 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid barcode-text-label    1 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid barcode-text-entry    2 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid position-text-label   3 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid position-text-entry   4 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid format-label          5 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid formats-listbox       6 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid notes-text-label      7 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid notes-text      8 0 :sticky :we   :padx +min-padding+ :pady +min-padding+)
      (grid bottom-frame          9 0 :sticky :ns   :padx +min-padding+ :pady +min-padding+)
      (grid apply-button          0 0 :sticky :ns   :padx +min-padding+ :pady +min-padding+)
      (grid close-button          0 1 :sticky :ns   :padx +min-padding+ :pady +min-padding+)
      (gui-resize-grid-all object)
      (focus barcode-text-entry)
      (when (preferences:preferences-use-insert-mode)
        (bind barcode-text-entry #$<Return>$
              (lambda (e)
                (declare (ignore e))
                (funcall (add-copy-clsr object))
                (break-mainloop))))
      (sync-copy-frame object))))

(defun sync-copy-frame (frame)
  (with-all-accessors (frame)
    (let* ((actual-title-id (or title-id
                                (getf (db:fetch-from-any-id db:+table-movie-copy+ copy-id)
                                      :title)))
           (title-info      (db:fetch-from-any-id db:+table-title+ actual-title-id))
           (directors-info  (if (db:all-directors-by-title actual-title-id)
                                (join-with-strings (mapcar (lambda (a) (getf a :desc))
                                                           (db:all-directors-by-title actual-title-id))
                                                   ",")
                                ""))
           (copy-info       (and copy-id
                                 (db:fetch-from-any-id db:+table-movie-copy+ copy-id))))
      (setf (text primary-title-label)  (getf title-info :primary-title))
      (setf (text original-title-label) (getf title-info :original-title))
      (setf (text directors-label)      directors-info)
      (setf (text year-label)
            (extract-year-from-timestamp (encode-datetime-string (getf title-info :year))))
      (when copy-info
        (setf (text barcode-text-entry)   (getf copy-info :barcode))
        (setf (text position-text-entry)  (encode-copy-position (getf copy-info :building)
                                                                (getf copy-info :room)
                                                                (getf copy-info :storage)
                                                                (getf copy-info :shelf)))
        (setf (text notes-text) (getf copy-info :notes))
        (let* ((format-desc (db:copy-row->format-description copy-info)))
          (select-listbox-format formats-listbox format-desc))))))

(defun make-add-copy-window (&optional (title-id nil) (copy-id nil))
  (assert (or title-id copy-id))
  (with-modal-toplevel (toplevel :title (_ "Add a new movie copy"))
    (let ((frame (make-instance 'add-copy-frame
                                :container-win toplevel
                                :title-id      title-id
                                :copy-id       copy-id
                                :master        toplevel)))
      (grid frame 0 0 :sticky :nswe :padx +min-padding+ :pady +min-padding+)
      (gui-resize-grid-all toplevel))))
