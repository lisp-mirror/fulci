;; fulci: a program to organize your movies collection
;; Copyright (C) 2018  cage

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

(defsystem :fulci
  :encoding    :utf-8
  :name        "fulci"
  :version     "0.9.1"
  :author      "cage"
  :maintainer  "cage"
  :bug-tracker "https://notabug.org/cage/fulci/issues"
  :licence     "GPLv3"
  :description "a program to organize your movies collection"
  :pathname    "src"
  :serial      t
  :depends-on  (:alexandria
                :cl-ppcre
                :sqlite
                :dbi
                :sxql
                :yacc
                :parse-number
                :cl-syntax
                :lquery
                :local-time
                :osicat
                :cl-colors2
                :nodgui
                :drakma
                :log4cl
                :clunit2
                :cl-i18n
                :cl-jpeg
                :cl-json
                :osicat
                :babel
                :nodgui
                :marshal
                :cl-pslib
                :cl-pslib-barcode
                :cl-csv)
  :components  ((:file "package")
                (:file "config")
                (:file "constants")
                (:file "conditions")
                (:file "misc-utils")
                (:file "text-utils")
                (:file "net-utils")
                (:file "filesystem-utils")
                (:file "image-utils")
                (:file "db-utils")
                (:file "bs-tree")
                (:file "rb-tree")
                (:file "json-rpc2")
                (:file "validation")
                (:file "db")
                (:file "preferences")
                (:file "search-title-expr")
                (:file "search-copy-expr")
                (:file "import-tsv")
                (:file "wikipedia")
                (:file "nodgui-utils")
                (:file "icons")
                (:file "menu-commands")
                (:file "manage-movie")
                (:file "ps-utils")
                (:file "manage-copies")
                (:file "search-frame")
                (:file "search-copies-frame")
                (:file "main-toolbar")
                (:file "main-frame")
                (:file "main")
                (:module tests
                         :components ((:file "package")
                                      (:file "all-tests")
                                      (:file "test-jsonrpc")
                                      (:file "test-search-expr")))))

;(push :debug-tk *features*)
;(push :print-sql *features*)
;(push :debug-csv *features*)
