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

(in-package :preferences)

(define-constant +preferences-file+ (strcat +home-data-dir+ "preference.sexp") :test #'string=)

(defun preferences-file-path ()
  (uiop:unix-namestring (strcat (fs:user-config-dir) "/" +preferences-file+)))

(defclass preferences ()
  ((copy-format
    :initform "dvd"
    :initarg  :copy-format
    :accessor copy-format)
   (page-width
    :initform (ps:width ps:+a4-page-size+)
    :initarg  :page-width
    :accessor page-width)
   (page-height
    :initform (ps:height ps:+a4-page-size+)
    :initarg  :page-height
    :accessor page-height)
   (barcode-width
    :initform 52.0
    :initarg  :barcode-width
    :accessor barcode-width)
   (barcode-height
    :initform 36.0
    :initarg  :barcode-height
    :accessor barcode-height)
   (place
    :initform ""
    :initarg  :place
    :accessor place)
   (wiki-host
    :initform "en.wikipedia.org"
    :initarg  :wiki-host
    :accessor wiki-host)
   (wiki-important-string
    :initform "film"
    :initarg  :wiki-important-string
    :accessor wiki-important-string)
   (gv-bin
    :initform +gv-bin+
    :initarg  :gv-bin
    :accessor gv-bin)
   (gimp-bin
    :initform +gimp-bin+
    :initarg  :gimp-bin
    :accessor gimp-bin)))

(defmethod marshal:class-persistant-slots ((object preferences))
  '(copy-format
    page-width
    page-height
    barcode-width
    barcode-height
    place
    wiki-host
    wiki-important-string
    gv-bin
    gimp-bin))

(defun make-default-preferences ()
  (make-instance 'preferences))

(defparameter *preferences* (make-default-preferences))

(defun dump (&optional (object-preferences *preferences*))
  (let* ((preferences-file (preferences-file-path)))
    (fs:create-file-if-not-exists preferences-file)
    (fs:dump-sequence-to-file (serialize object-preferences) preferences-file)))

(defun init ()
  (let* ((size (fs:file-length-if-exists (preferences-file-path))))
    (if (or (null size)
            (= size 0))
        (dump (make-default-preferences))
        (setf *preferences* (deserialize 'preferences (preferences-file-path))))))

(defmacro gen-acc-fn (name)
  `(progn
     (defun ,(misc:format-fn-symbol t "preferences-~a" name) ()
       (,(misc:format-fn-symbol t "~a" name) *preferences*))
     (defun ,(misc:format-fn-symbol t "set-~a" name) (val)
       ,(with-gensyms (old)
          `(let ((,old (,(misc:format-fn-symbol t "preferences-~a" name))))
             (setf (,(misc:format-fn-symbol t "~a" name) *preferences*) val)
             ,old)))))

(gen-acc-fn copy-format)

(gen-acc-fn page-width)

(gen-acc-fn page-height)

(gen-acc-fn barcode-width)

(gen-acc-fn barcode-height)

(gen-acc-fn place)

(gen-acc-fn wiki-host)

(gen-acc-fn wiki-important-string)

(gen-acc-fn gv-bin)

(gen-acc-fn gimp-bin)

(defun parse-number-default (e)
  (declare (ignore e))
  20.0)

(defun sync-preferences-to-gui (copy-format-entry
                                position-entry
                                page-width-entry
                                page-height-entry
                                barcode-width-entry
                                barcode-height-entry
                                wiki-host-entry
                                wiki-important-string-entry
                                gv-bin-entry
                                gimp-bin-entry)
  (setf (nodgui:text copy-format-entry)     (preferences-copy-format))
  (setf (nodgui:text position-entry)        (preferences-place))
  (setf (nodgui:text page-width-entry)      (to-s (preferences-page-width)))
  (setf (nodgui:text page-height-entry)     (to-s (preferences-page-height)))
  (setf (nodgui:text barcode-width-entry)   (to-s (preferences-barcode-width)))
  (setf (nodgui:text barcode-height-entry)  (to-s (preferences-barcode-height)))
  (setf (nodgui:text wiki-host-entry)       (to-s (preferences-wiki-host)))
  (setf (nodgui:text wiki-important-string-entry)
        (to-s (preferences-wiki-important-string)))
  (setf (nodgui:text gv-bin-entry)          (to-s (preferences-gv-bin)))
  (setf (nodgui:text gimp-bin-entry)        (to-s (preferences-gimp-bin))))

(defun sync-gui-to-preferences (copy-format-entry
                                position-entry
                                page-width-entry
                                page-height-entry
                                barcode-width-entry
                                barcode-height-entry
                                wiki-host-entry
                                wiki-important-string-entry
                                gv-bin-entry
                                gimp-bin-entry)
  (set-copy-format    (nodgui:text copy-format-entry))
  (set-place          (nodgui:text position-entry))
  (set-page-width     (misc:safe-parse-number (nodgui:text page-width-entry)
                                              :fix-fn #'parse-number-default))
  (set-page-height    (misc:safe-parse-number (nodgui:text page-height-entry)
                                              :fix-fn #'parse-number-default))
  (set-barcode-width  (misc:safe-parse-number (nodgui:text barcode-width-entry)
                                              :fix-fn #'parse-number-default))
  (set-barcode-height (misc:safe-parse-number (nodgui:text barcode-height-entry)
                                              :fix-fn #'parse-number-default))
  (set-wiki-host                 (nodgui:text wiki-host-entry))
  (set-wiki-important-string     (nodgui:text wiki-important-string-entry))
  (set-gv-bin                    (nodgui:text gv-bin-entry))
  (set-gimp-bin                  (nodgui:text gimp-bin-entry)))

(defun make-preferences-window ()
  (init)
  (nodgui:with-modal-toplevel (toplevel :title (_ "Preferences"))
    (let* ((tabs-container    (make-instance 'nodgui:notebook :master toplevel))
           (general           (make-instance 'nodgui:frame    :master tabs-container))
           (print             (make-instance 'nodgui:frame    :master tabs-container))
           (net               (make-instance 'nodgui:frame    :master tabs-container))
           (ext-programs      (make-instance 'nodgui:frame    :master tabs-container))
           (bottom-frame      (make-instance 'nodgui:frame    :master toplevel))
           (copy-format-label (make-instance 'nodgui:label
                                             :master general
                                             :text (_ "Default physical movie copy format")))
           (copy-format-entry (make-instance 'nodgui:entry
                                             :master general))
           (position-label    (make-instance 'nodgui:label
                                             :master general
                                             :text (_ "Default physical position of a movie copy")))
           (position-entry    (make-instance 'nodgui:entry
                                             :master general))
           (page-width-label  (make-instance 'nodgui:label
                                             :master print
                                             :text (_ "Page width in millimiters")))
           (page-width-entry  (make-instance 'nodgui:entry
                                             :master print))
           (page-height-label (make-instance 'nodgui:label
                                             :master print
                                             :text (_ "Page height in millimiters")))
           (page-height-entry (make-instance 'nodgui:entry
                                             :master print))
           (barcode-height-label (make-instance 'nodgui:label
                                                :master print
                                                :text (_ "Barcode height in millimiters")))
           (barcode-height-entry (make-instance 'nodgui:entry
                                                :master print))
           (barcode-width-label  (make-instance 'nodgui:label
                                                :master print
                                                :text (_ "Barcode width in millimiters")))
           (barcode-width-entry  (make-instance 'nodgui:entry
                                                :master print))
           (wiki-host-label      (make-instance 'nodgui:label
                                                :master net
                                                :text (_ "Wikipedia hostname")))
           (wiki-host-entry      (make-instance 'nodgui:entry
                                                :master net))
           (wiki-important-string-label  (make-instance 'nodgui:label
                                                        :master net
                                                        :text (_ "When searching in Wikipedia promote on top entries that contains the following word")))
           (wiki-important-string-entry  (make-instance 'nodgui:entry
                                                        :master net))
           (gv-bin-label         (make-instance 'nodgui:label
                                                :master ext-programs
                                                :text (_ "PostScript viewer program path")))
           (gv-bin-entry         (make-instance 'nodgui:entry
                                                :master ext-programs))
           (gimp-bin-label       (make-instance 'nodgui:label
                                                :master ext-programs
                                                :text (_ "GIMP program path")))
           (gimp-bin-entry       (make-instance 'nodgui:entry
                                                :master ext-programs))
           (ok-button-cb         (lambda ()
                                   (sync-gui-to-preferences copy-format-entry
                                                            position-entry
                                                            page-width-entry
                                                            page-height-entry
                                                            barcode-width-entry
                                                            barcode-height-entry
                                                            wiki-host-entry
                                                            wiki-important-string-entry
                                                            gv-bin-entry
                                                            gimp-bin-entry)
                                   (dump)
                                   (nodgui:break-mainloop)))
           (ok-button            (make-instance 'nodgui:button
                                                :master  bottom-frame
                                                :text    (_ "OK")
                                                :command ok-button-cb))
           (cancel-button     (make-instance 'nodgui:button
                                             :master  bottom-frame
                                             :text    (_ "Cancel")
                                             :command (lambda ()
                                                        (nodgui:break-mainloop)))))
      (nodgui:grid copy-format-label    0  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid copy-format-entry    1  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid position-label       2  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid position-entry       3  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid page-width-label     0  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid page-width-entry     1  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid page-height-label    2  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid page-height-entry    3  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid barcode-width-label  4  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid barcode-width-entry  5  0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid barcode-height-label 6 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid barcode-height-entry 7 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid wiki-host-label      0 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid wiki-host-entry      1 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid wiki-important-string-label 2 0
                   :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid wiki-important-string-entry 3 0
                   :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid gv-bin-label         0 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid gv-bin-entry         1 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid gimp-bin-label       2 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid gimp-bin-entry       3 0 :sticky :we :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid bottom-frame         1 0 :sticky :ns :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid ok-button            0 0 :sticky :ns :padx +min-padding+ :pady +min-padding+)
      (nodgui:grid cancel-button        0 1 :sticky :ns :padx +min-padding+ :pady +min-padding+)
      (sync-preferences-to-gui copy-format-entry
                               position-entry
                               page-width-entry
                               page-height-entry
                               barcode-width-entry
                               barcode-height-entry
                               wiki-host-entry
                               wiki-important-string-entry
                               gv-bin-entry
                               gimp-bin-entry)
      (nodgui:notebook-add tabs-container
                           general
                           :text (_ "General preferences"))
      (nodgui:notebook-add tabs-container
                           print
                           :text (_ "Printing"))
      (nodgui:notebook-add tabs-container
                           net
                           :text (_ "Networking"))
      (nodgui:notebook-add tabs-container
                           ext-programs
                           :text (_ "External programs"))
      (nodgui:grid tabs-container 0 0 :sticky :nwes)
      (nodgui-utils:gui-resize-grid-all toplevel))))
