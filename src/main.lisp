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

(in-package :main)

(defun init-i18n ()
  (handler-bind ((error
                  #'(lambda(e)
                      (declare (ignore e))
                      (invoke-restart 'cl-i18n:return-empty-translation-table))))
    (setf cl-i18n:*translation-file-root* +catalog-dir+)
    (cl-i18n:load-language +text-domain+ :locale (cl-i18n:find-locale))))

(defun init-history ()
  (when (not (fs:file-exists-p (misc:history-path)))
    (fs:create-file (misc:history-path))))

(defun main ()
  (let ((*debug-tk* #+debug-tk t
                    #-debug-tk nil))
    (db-utils:with-ready-database (:connect t)
      (setf sxql:*sql-symbol-conversion* #'db-utils:quote-symbol)
      (init-history)
      (preferences:init)
      (init-i18n)
      (with-nodgui (:title                            +program-name+
                    :main-loop-thread-special-bindings
                    (acons 'sxql.operator:*sql-symbol-conversion* #'db-utils:quote-symbol
                           bt:*default-special-bindings*))
        (icons:load-icons)
        (nodgui:icon-photo *tk* icons:*icon-fulci*)
        (let ((main-toolbar (make-instance 'main-toolbar:main-toolbar))
              (main-frame   (make-instance 'main-frame:main-frame
                                           :main-window *tk*)))
          (main-frame:initialize-menu main-frame)
          (grid main-toolbar 0 0 :sticky :ew)
          (grid main-frame   1 0 :sticky :nswe)
          (grid-columnconfigure *tk* 0 :weight 1)
          (grid-rowconfigure    *tk* 0 :weight 0)
          (grid-rowconfigure    *tk* 1 :weight 1))))))
