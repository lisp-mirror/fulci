(in-package :nodgui-utils)

(cl-syntax:use-syntax nodgui.utils:nodgui-color-syntax)

(define-constant +font-h1+       "sans 20 bold" :test #'string=)

(define-constant +font-h2+       "sans 15 bold" :test #'string=)

(define-constant +font-h3+       "bold"         :test #'string=)

(defun gui-resize-grid-all (w)
  (grid-columnconfigure w :all :weight 1)
  (grid-rowconfigure    w :all :weight 1))

(defun confirm-deletion (parent how-many)
  (ask-yesno (format nil (n_ "~a element will be deleted; continue?"
                             "~a elements will be deleted; continue?"
                             how-many)
                     how-many)
             :title  (_ "Confirm operation")
             :parent parent))

(defun info-operation-completed (parent)
  (message-box (_ "Operation completed") (_ "information") :ok "info" :parent parent))

(defun info-dialog (parent message)
  (message-box message (_ "Information") :ok "info" :parent parent))

(defun error-dialog (parent message)
  (message-box message (_ "Error") :ok "error" :parent parent))

(defun re-validate (parent datum regex error-message)
  "data -> '(datum regexp error-message)"
  (let* ((all-errors (join-with-strings* " "
                                         (validation:regexp-validate (list (list datum
                                                                                 regex
                                                                                 error-message)))))

         (error-message (when all-errors
                          (join-with-strings all-errors (format nil "~%")))))
    (if all-errors
        (progn
          (error-dialog parent error-message)
          nil)
        t)))

(defmacro with-re-validate ((parent &rest filters) &body body)
  `(and ,@(loop for filter in filters collect
               `(apply #'re-validate (list ,parent ,@filter)))
        ,@body))

(defmacro with-entry-text-validate ((parent &rest filters) &body body)
  "filtrers -> '(entry-widget regex error-message)"
  `(with-re-validate (,parent ,@(loop for filter in filters collect
                                     `((text ,(elt filter 0))
                                       ,(elt filter 1)
                                       ,(elt filter 2))))
        ,@body))

(defun attach-tooltip (widget text)
  (let ((tooltip (make-instance 'nodgui.mw:tooltip
                                :borderwidth 1
                                :relief      :solid)))
      (nodgui.mw:register-tooltip tooltip widget text)))

(defmacro attach-tooltips (&rest widget-text)
  `(progn
     ,@(loop for i in widget-text collect
            `(attach-tooltip ,(first i) ,(second i)))))

(defmacro with-busy* ((root-widget) &body body)
  `(progn
     (with-busy (,root-widget)
       (with-hourglass ,(list root-widget)
         ,@body))))

(define-constant +vote-canvas-h+ 30 :test #'=)

(defmacro with-title-details-toplevel ((title-row title-id title-window notes-entry) &body body)
  (with-gensyms (title-frame
                 primary-title-desc
                 vote-canvas
                 vote-star
                 vote-value
                 vote-text
                 original-title-label
                 original-title-desc
                 countries-desc
                 countries-label
                 directors-label
                 directors-desc
                 year-desc
                 year-label
                 genres-desc
                 genres-label
                 notes-text-label
                 image-button)
    `(let ((,title-row (db:title-details ,title-id)))
       (labels ((,(format-fn-symbol t "~a" 'max-col) () 1)
                (,(format-fn-symbol t "~a" 'max-row) () 13)
                (format-vote (title-row)
                  (format nil
                          "~,1f"
                          (safe-parse-integer (getf title-row :vote)
                                              :fix-fn (lambda (e)
                                                        (declare (ignore e))
                                                        "0")))))
         (declare (ignorable #',(format-fn-symbol t "~a" 'max-col)
                             #',(format-fn-symbol t "~a" 'max-row)))
         (with-nodgui (:title ,title-window)
           (let* ((,title-frame          (make-instance 'frame
                                                        :height +vote-canvas-h+
                                                        :master nil))
                  (,primary-title-desc   (make-instance 'label
                                                        :justify    :center
                                                        :wraplength (* 20 +vote-canvas-h+)
                                                        :font       +font-h1+
                                                        :text       (getf ,title-row :pt)
                                                        :master     ,title-frame))
                  (,vote-canvas          (make-canvas ,title-frame
                                                      :width  (* 3 +vote-canvas-h+)
                                                      :height +vote-canvas-h+))
                  (,vote-star            (make-star   ,vote-canvas
                                                      (truncate (/ +vote-canvas-h+ 2))
                                                      0.5
                                                      #%yellow%
                                                      #%orange%
                                                      5
                                                      :outline-width 1))
                  (,vote-value           (format-vote ,title-row))
                  (,vote-text            (create-text ,vote-canvas
                                                      0 0
                                                      ,vote-value
                                                      :font +font-h3+))
                  (,original-title-label (make-instance 'label
                                                        :text   (_ "Original title")
                                                        :font   +font-h2+
                                                        :master nil))
                  (,original-title-desc  (make-instance 'label
                                                        :font   +font-h3+
                                                        :text   (getf ,title-row :ot)
                                                        :master nil))
                  (,countries-desc       (make-instance 'label
                                                        :font   +font-h3+
                                                        :text   (getf ,title-row
                                                                      :country-description)
                                                        :master nil))
                  (,countries-label      (make-instance 'label
                                                        :font   +font-h2+
                                                        :text   (_ "Country")
                                                        :master nil))
                  (,directors-label      (make-instance 'label
                                                        :text   (_ "Directors")
                                                        :font   +font-h2+
                                                        :master nil))
                  (,directors-desc       (make-instance 'label
                                                        :font   +font-h3+
                                                        :text   (getf ,title-row :director)
                                                        :master nil))
                  (,year-desc            (make-instance 'label
                                                        :font   +font-h3+
                                                        :text
                                                        (db-utils:encoded-datetime-year
                                                         (getf ,title-row :year))
                                                        :master nil))
                  (,year-label           (make-instance 'label
                                                        :font   +font-h2+
                                                        :text   (_ "Year")
                                                        :master nil))
                  (,genres-desc          (make-instance 'label
                                                        :font   +font-h3+
                                                        :text   (getf ,title-row :genres)
                                                        :master nil))
                  (,genres-label         (make-instance 'label
                                                        :font   +font-h2+
                                                        :text   (_ "Genres")
                                                        :master nil))
                  (,notes-text-label     (make-instance 'label
                                                        :font   +font-h2+
                                                        :text   (_ "Notes")
                                                        :master nil))
                  (,notes-entry          (make-instance 'label
                                                        :font   +font-h3+
                                                        :text   (getf ,title-row :notes)
                                                        :master nil))
                  (,image-button         (make-instance 'button
                                                        :text (_ "Image not available")
                                                        :master nil)))
             (item-move-to ,vote-canvas ,vote-text +vote-canvas-h+ 0)
             (manage-movie:load-image-in-button ,title-id ,image-button)
             (grid ,primary-title-desc   0 0 :sticky :ns :padx +min-padding+ :pady +min-padding+
                   :columnspan 3)
             (grid ,vote-canvas          0 4 :sticky :e  :padx +min-padding+ :pady +min-padding+)
             (shape-move-to ,vote-star  0 0)
             (grid ,title-frame          0 0 :sticky :news :padx +min-padding+ :pady +min-padding+
                   :columnspan 2)
             (grid ,image-button         1 0 :sticky :news :padx +min-padding+ :pady +min-padding+
                   :rowspan 12)
             (grid ,original-title-label 2 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,original-title-desc  3 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,countries-label      4 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,countries-desc       5 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,year-label           6 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,year-desc            7 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,directors-label      8 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,directors-desc       9 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,genres-label        10 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,genres-desc         11 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,notes-text-label    12 1 :sticky :we :padx +min-padding+ :pady +min-padding+)
             (grid ,notes-entry         13 1 :sticky :we :padx +min-padding+
                   :pady +min-padding+)
             ,@body))))))
