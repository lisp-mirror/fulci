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

(defpackage :config
  (:use
   :cl
   :alexandria)
  (:export
   :+sqlite-bin+
   :+gimp-bin+
   :+gv-bin+
   :+sys-data-dir+
   :+catalog-dir+
   :+text-domain+
   :+program-name+
   :+home-data-dir+
   :_
   :n_))

(defpackage :constants
  (:use :cl
        :alexandria
        :config)
  (:export
   :+help-about-message-template+
   :+db-file+
   :+search-history-file+
   :+tmp-dir+
   :+country-csv-file+
   :+http-code-ok+
   :+mime-type-html+
   :+mime-type-jpg+
   :+mime-type-png+
   :+min-padding+
   :+char-start-search-expr+
   :+search-latest-id-re+
   :+image-max-w+
   :+image-max-h+
   :+max-history-items-count+
   :+maximum-vote+))

(defpackage :conditions
  (:use :cl)
  (:export
   :text-error
   :http-error
   :text
   :not-implemented-error
   :null-reference
   :out-of-bounds
   :length-error
   :different-length-error
   :with-default-on-error))

(defpackage :misc-utils
  (:use :cl
        :config
        :constants)
  (:nicknames :misc)
  (:export
   :when-debug
   :debug-log
   :dbg
   :code->char
   :char->code
   :swap
   :dump-hash-table
   :with-messages-start-end
   :safe-random
   :split-into-sublist
   :delete@
   :safe-delete@
   :remove-compact-remap-sequence
   :remove-if-null
   :remove-if-not-null
   :do-while
   :do-while*
   :not-null-p
   :permutation
   :make-fresh-list
   :seq->list
   :lcat
   :vcat
   :fresh-list-insert@
   :fresh-list-subst@
   :make-array-frame
   :make-fresh-array
   :sequence->list
   :vector-empty-p
   :safe-subseq
   :copy-multiply
   :all-but-last-elt
   :list->array
   :list->simple-array
   :copy-list-into-array
   :2byte->word
   :2word->int
   :byte->int
   :bytes->string
   :int16->bytes
   :int32->bytes
   :define-offset-size
   :define-parse-header-chunk
   :read-list
   :read-array
   :definline
   :+cache-invalid-value+
   :defcached
   :defcached-list
   :fn-delay
   :unsplice
   :defalias
   :defun-inline-function
   :format-fn-symbol
   :check-body-keywords
   :format-keyword
   :a->function
   :gen-type-p
   :define-compiler-macros
   :defmethod-inline-function
   :nest-expressions
   :replace-e!
   :+nil-equiv-bag+
   :build-plist
   :build-assocs-chain
   :recursive-assoc
   :recursive-assoc-just-before
   :n-setf-path-value
   :plist-path-value
   :gen-trivial-plist-predicates
   :gen-trivial-plist-predicate
   :gen-trivial-plist-get
   :gen-trivial-plist-gets
   :gen-vec-comp
   :make-null-pointer
   :with-load-forms-in-var
   :time-unix->universal
   :time-second-of
   :time-minutes-of
   :time-hour-of
   :time-date-of
   :time-month-of
   :time-year-of
   :time-day-of
   :time-daylight-p-of
   :time-zone-of
   :safe-parse-integer
   :year->timestamp
   :current-year
   :extract-year-from-timestamp
   :launch-command
   :serialize
   :serialize-to-stream
   :deserialize
   :serialize-to-stream
   :safe-parse-number
   :history-path))

(defpackage :filesystem-utils
  (:use
   :cl
   :alexandria)
  (:nicknames :fs)
  (:export
   :+preprocess-include+
   :+file-path-regex+
   :*directory-sep-regexp*
   :*directory-sep*
   :copy-a-file
   :file-size
   :count-lines-in-file
   :file->list-lines
   :list-lines->file
   :append-line->file
   :slurp-file
   :dump-sequence-to-file
   :create-file
   :create-file-if-not-exists
   :cat-parent-dir
   :has-extension
   :do-directory
   :search-matching-file
   :split-path-elements
   :path-last-element
   :path-first-element
   :path-to-hidden-file-p
   :parent-dir-path
   :strip-dirs-from-path
   :get-stat-mtime
   :get-stat-ctime
   :get-stat-atime
   :file-outdated-p
   :file-exists-p
   :directory-exists-p
   :file-length-if-exists
   :delete-file-if-exists
   :home-dir
   :user-data-dir
   :user-config-dir
   :user-cache-dir
   :temporary-filename
   :with-anaphoric-temp-file
   :temp-file
   :file-can-write-p
   :cached-directory-files
   :directory-files
   :make-directory
   :package-path
   :file-in-package
   :pathname->namestring
   :namestring->pathname
   :read-single-form
   :eq-filename))

(defpackage :text-utils
  (:use :cl)
  (:import-from :misc :definline)
  (:export
   :+float-regexp+
   :+integer-regexp+
   :uchar-length
   :utf8-encoded-p
   :clean-unprintable-chars
   :to-s
   :strcat
   :join-with-strings
   :join-with-strings*
   :split-words
   :split-lines
   :strip-prefix
   :strip-withespaces
   :string-empty-p
   :string-not-empty-p
   :basename
   :wrap-with
   :right-padding
   :left-padding
   :justify-monospaced-text
   :flush-left-mono-text
   :escape-tilde
   :ellipsize))

(defpackage :net-utils
  (:use
   :cl
   :alexandria
   :cl-ppcre)
  (:export
   :mime-type
   :check-mime-type
   :percent-encode))

(defpackage :image-utils
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :nodgui.pixmap
   :constants
   :config
   :filesystem-utils)
  (:export
   :load-from-vector
   :tga-as-vector
   :image->tga-file
   :image->tga-memory))

(defpackage :db-utils
    (:use
     :cl
     :alexandria
     :cl-ppcre
     :sxql
     :local-time
     :config
     :constants
     :text-utils)
    (:export
     :+characters-trouble-name+
     :*connection*
     :fetch
     :fetch-all
     :close-db
     :connectedp
     :with-db-transaction
     :db-path
     :quote-symbol
     :init-connection
     :with-ready-database
     :with-disabled-foreign
     :do-rows
     :fetch-raw-list
     :prepare-for-sql-like
     :object-exists-in-db-p
     :object-count-in-db
     :query-low-level
     :db-nil-p
     :db-nil->lisp
     :if-db-nil-else
     :count-all
     :get-column-from-id
     :query
     :query->sql
     :local-time-obj-now
     :decode-date-string
     :decode-datetime-string
     :encode-datetime-string
     :encoded-datetime-year
     :make-insert
     :director-role-id
     :with-director-db-id
     :get-max-id
     :decode-copy-position
     :encode-copy-position
     :decode-blob
     :rows->tsv
     :encode-vote
     :decode-vote))

(defpackage :bs-tree
  (:use
   :cl)
  (:shadow :search :map)
  (:export
   :node
   :parent
   :data
   :left
   :right
   :make-node
   :make-leaf
   :make-root-node
   :%key
   :+data+
   :+left+
   :+right+
   :+parent+
   :render-tree
   :node->string
   :search
   :search-opt
   :with-insert-local-function
   :insert
   :leafp
   :all-children-leaf-p
   :map
   :map-node
   :walk
   :bstp
   :node->dot
   :reconstruct-parent
   :find-max-node
   :to-sexp
   :from-sexp))

(defpackage :rb-tree
  (:use
   :cl
   :bs-tree)
  (:shadowing-import-from :bs-tree :search :map)
  (:export
   :+rb-red+
   :+rb-black+
   :+rb-color+
   :rb-node
   :color
   :make-rb-node
   :make-rb-leaf
   :make-root-rb-node
   :data
   :left
   :right
   :node->string
   :search
   :search-opt
   :with-insert-local-function
   :insert
   :remove-node
   :leafp
   :map
   :map-node
   :balancedp
   :walk
   :bstp
   :node->dot
   :reconstruct-parent))

(defpackage :json-rpc2
  (:use :cl
        :alexandria
        :cl-json)
  (:export
   :generate-request-id
   :*function-db*
   :register-function
   :unregister-function
   :make-request
   :make-request*
   :make-notification
   :make-notification*
   :make-batch
   :jsonify
   :json-rpc-error
   :elaborate-request
   :transaction-id
   :code
   :text))

(defpackage :validation
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :config
   :constants
   :db-utils)
  (:export
   :all-not-null-p
   :all-null-p
   :regexp-validate
   :unique-p-validate
   :unique-p-validate*
   :exists-with-different-id-validate
   :id-valid-and-used-p
   :with-id-valid-and-used
   :integer-validate
   :date-validate-p
   :magic-validate-p
   :png-validate-p
   :integer-%-validate
   :integer-positive-validate
   :+integer-re+
   :+pos-integer-re+
   :+email-re+
   :+internet-address-re+
   :+free-text-re+
   :+barcode-id-re+
   :+copy-position-re+))

(defpackage :db
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :sxql
   :config
   :constants
   :db-utils
   :text-utils)
  (:export
   :+other-id+
   :+table-country+
   :+table-genre+
   :+table-movie-storage-format+
   :+table-title+
   :+table-genre+
   :+table-role+
   :+table-person+
   :+table-movie-copy+
   :+table-crew+
   :+table-title-genre+
   :+search-expr-director-col+
   :+search-expr-primary-title-col+
   :+search-expr-original-title-col+
   :+search-expr-year-col+
   :+search-expr-notes-col+
   :+search-expr-tags-col+
   :+search-expr-genres-col+
   :+search-expr-format-col+
   :+search-expr-building-col+
   :+search-expr-room-col+
   :+search-expr-shelf-col+
   :+search-expr-storage-col+
   :+search-expr-country-col+
   :other-id
   :description
   :movie-strorage-format
   :genre
   :role
   :title
   :primary-title
   :year
   :runtime
   :person
   :primary-name
   :birth-year
   :crew
   :movie-copy
   :build-all-tables
   :build-all-indices
   :delete-all-tables
   :delete-all-views
   :delete-database
   :fetch-all-rows
   :add-description
   :delete-description
   :update-description
   :description-exists-p
   :table->alist
   :persons->alist
   :normalize-birthday
   :normalize-name
   :add-person
   :add-person-unknown-birthday
   :update-person
   :delete-person
   :find-person
   :all-genres
   :all-genres-description
   :all-title-genres-ids
   :all-genres-by-title
   :all-countries
   :all-countries-description
   :all-title-countries-ids
   :all-countries-by-title
   :all-formats-description
   :unique-description->id
   :format-description->id
   :all-directors-by-title
   :id-desc-alist-from-table
   :fetch-from-any-id
   :fetch-single
   :fetch-title
   :fetch-person
   :director-role-id
   :search-titles-main-frame
   :search-titles-by-genre
   :all-title-genres
   :sql-search-titles
   :title-details
   :copy-row->format-row
   :copy-row->format-description
   :filter-directors
   :add-new-movie
   :update-movie
   :remove-link-title-director
   :add-new-copy
   :search-copies-main-frame
   :update-copy
   :copy-id->titles
   :last-n-copies-id
   :copies-with-problems
   :search-movie-expr
   :search-movie-simple
   :search-movies
   :search-copies
   :search-copies-expr
   :sql-order-by
   :sql-group-by
   :delete-by-id))

(defpackage :preferences
  (:use :cl
        :alexandria
        :cl-ppcre
        :config
        :constants
        :misc
        :text-utils)
  (:nicknames :pref)
  (:export
   :dump
   :init
   :preferences-copy-format
   :set-copy-format
   :preferences-page-width
   :set-page-width
   :preferences-page-height
   :set-page-height
   :preferences-barcode-width
   :set-barcode-width
   :preferences-barcode-height
   :set-barcode-height
   :preferences-place
   :set-place
   :preferences-wiki-host
   :set-wiki-host
   :preferences-wiki-important-string
   :set-wiki-important-string
   :preferences-gv-bin
   :set-gv-bin
   :preferences-gimp-bin
   :set-gimp-bin
   :make-preferences-window))

(defpackage :search-title-expr
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :cl-lex
   :yacc
   :misc
   :text-utils
   :db-utils
   :db)
  (:export
   :lexer
   :parse))

(defpackage :search-copy-expr
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :cl-lex
   :yacc
   :misc
   :text-utils
   :db-utils
   :db)
  (:export
   :lexer
   :parse))

(defpackage :import-tsv
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :sxql
   :cl-csv
   :config
   :constants
   :misc
   :db-utils
   :text-utils)
  (:export
   :import-genres
   :import-persons
   :import-titles
   :link-titles-genres
   :link-crew
   :import-country))

(defpackage :wikipedia
  (:use :cl
        :alexandria
        :cl-ppcre
        :config
        :constants
        :conditions
        :misc
        :text-utils
        :net-utils)
  (:export
   :*wikipedia-host*
   :search-wiki-pages
   :search-wiki-image
   :movie-entry
   :movie-entry-title
   :movie-entry-director
   :movie-entry-year
   :movie-entry-runtime
   :get-movie-info))

(defpackage :nodgui-utils
  (:use :cl
        :alexandria
        :cl-ppcre
        :config
        :constants
        :misc
        :text-utils
        :nodgui
        :nodgui.shapes)
  (:export
   :+font-h1+
   :+font-h2+
   :+font-h3+
   :gui-resize-grid-all
   :confirm-deletion
   :info-operation-completed
   :info-dialog
   :error-dialog
   :re-validate
   :with-re-validate
   :with-entry-text-validate
   :attach-tooltip
   :attach-tooltips
   :with-busy*
   :with-title-details-toplevel))

(defpackage :icons
  (:use :cl
        :alexandria
        :cl-ppcre
        :nodgui
        :config
        :constants
        :misc
        :text-utils)
  (:export
   :+icon-dir+
   :*icon-search*
   :*icon-movie*
   :*icon-delete-movie*
   :*icon-add-movie*
   :*icon-edit-movie*
   :*icon-lookup-internet-movie*
   :*icon-dvd*
   :*icon-delete-dvd*
   :*icon-add-dvd*
   :*icon-edit-dvd*
   :*icon-persons*
   :*icon-genre*
   :*icon-country*
   :*icon-add*
   :*icon-delete*
   :*icon-add-small*
   :*icon-delete-small*
   :*icon-edit*
   :*icon-edit-small*
   :*icon-goto-wiki*
   :*icon-wiki-fetch-image*
   :*icon-wiki-fetch-data*
   :*icon-dvd-case*
   :*icon-barcode*
   :*icon-details*
   :*icon-fulci*
   :*icon-goto-copy*
   :load-icon
   :load-icons))

(defpackage :menu-commands
  (:use :cl
        :alexandria
        :cl-ppcre
        :nodgui
        :nodgui.mw
        :config
        :constants
        :misc
        :text-utils
        :nodgui-utils
        :db-utils)
  (:nicknames :menu)
  (:export
   :help-about
   :quit
   :copy-db-fn
   :dump-db-fn
   :import-from-sql-fn
   :import-from-db-fn
   :import-tsv-window
   :export-tsv-titles
   :export-tsv-copies
   :import-find-problem-copies-fn))

(defpackage :manage-movie
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :nodgui
   :nodgui.mw
   :sxql
   :config
   :constants
   :misc
   :validation
   :nodgui-utils
   :db-utils
   :icons
   :text-utils
   :image-utils
   :wikipedia)
  (:export
   :load-image-in-button
   :make-add-movie-window))

(defpackage :ps-utils
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :cl-pslib
   :config
   :constants
   :text-utils
   :db-utils
   :db)
  (:shadowing-import-from :ps :rotate)
  (:export
   :render-barcode-table))

(defpackage :manage-copies
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :nodgui
   :nodgui.mw
   :sxql
   :config
   :constants
   :misc
   :validation
   :nodgui-utils
   :db-utils
   :icons
   :text-utils
   :image-utils
   :wikipedia)
  (:export
   :make-add-copy-window))

(defpackage :search-frame
  (:use
   :cl
   :alexandria
   :nodgui
   :nodgui.mw
   :config
   :constants
   :misc
   :nodgui-utils
   :db-utils
   :sxql
   :icons
   :text-utils)
  (:export
   :search-frame
   :search-copies-frame))

(defpackage :main-toolbar
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :nodgui
   :nodgui.mw
   :config
   :constants
   :misc
   :nodgui-utils
   :db-utils
   :sxql
   :text-utils
   :icons)
  (:export
   :main-toolbar))

(defpackage :main-frame
  (:use
   :cl
   :alexandria
   :nodgui
   :config
   :constants
   :misc
   :nodgui-utils
   :db-utils
   :sxql
   :icons)
  (:export
   :initialize-menu
   :main-frame
   :search-title-tab
   :search-copies-tab))

(defpackage :main
  (:use :cl
        :config
        :constants
        :nodgui))
