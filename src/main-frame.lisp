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

(in-package :main-frame)

(defun temporary-disabled ()
  (nodgui-utils:info-dialog *tk* "Functionality temporarly disabled"))

(defun initialize-menu (parent)
  (with-accessors ((main-window main-window)) parent
    (let* ((bar        (make-menubar))
           (db         (make-menu bar    (_ "Database")))
           (sep1       (add-separator    db))
           (db-import  (make-menu db     (_ "Import")))
           (sep2       (add-separator    db))
           (export     (make-menu db     (_ "Export")))
           (export-tsv (make-menu export (_ "As TSV")))
           (sep3       (add-separator    db))
           (edit       (make-menu bar    (_ "Edit")))
           (help       (make-menu bar    (_ "Help"))))
      (declare (ignore sep1 sep2 sep3))
      (make-menubutton db-import (_ "From TSV")    #'temporary-disabled :underline 0)
      (make-menubutton export    (_ "Copy")        (menu:copy-db-fn main-window)
                       :underline 0)
      (make-menubutton export     (_ "Dump SQL")    (menu:dump-db-fn main-window)
                       :underline 0)
      (make-menubutton export-tsv (_ "Titles")      #'menu::export-tsv-titles
                       :underline 0)
      (make-menubutton export-tsv (_ "Copies")      #'menu::export-tsv-copies
                       :underline 0)
      (make-menubutton db-import (_ "From SQL")    (menu:import-from-sql-fn main-window)
                       :underline 0)
      (make-menubutton db-import (_ "From backup") (menu:import-from-db-fn main-window)
                       :underline 0)
      (make-menubutton db        (_ "Quit")        #'menu:quit              :underline 0)
      (make-menubutton edit      (_ "Preferences")
                       (lambda () (preferences:make-preferences-window))
                       :underline 0)
      (make-menubutton help      (_ "About")       #'menu:help-about        :underline 0))))

(defclass main-frame (frame)
  ((main-window
    :initform nil
    :initarg :main-window
    :accessor main-window)
   (search-title-tab
    :initform nil
    :initarg  :search-title-tab
    :accessor search-title-tab)
   (search-copies-tab
    :initform nil
    :initarg  :search-copies-tab
    :accessor search-copies-tab)))

(defmethod initialize-instance :after ((object main-frame) &key &allow-other-keys)
  (with-accessors ((search-title-tab  search-title-tab)
                   (search-copies-tab search-copies-tab)) object
    (let* ((tabs-container (make-instance 'notebook
                                          :master object)))

      (setf search-title-tab  (make-instance 'search-frame:search-frame
                                             :main-frame object
                                             :master tabs-container))
      (setf search-copies-tab (make-instance 'search-frame:search-copies-frame
                                             :main-frame object
                                             :master     tabs-container))
      (notebook-add tabs-container
                    search-title-tab
                    :text "Movies"
                    :image *icon-movie*)
      (notebook-add tabs-container
                    search-copies-tab
                    :text "Copies"
                    :image *icon-dvd*)
      (grid tabs-container 0 0 :sticky :nwes)
      (nodgui-utils:gui-resize-grid-all object))))
