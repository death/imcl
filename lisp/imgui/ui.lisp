;;;; +----------------------------------------------------------------+
;;;; | IMGUI-ECL                                                      |
;;;; +----------------------------------------------------------------+

(in-package #:imgui)

(export
 '(eval-in-top-level
   color
   color-noalpha
   window
   tooltip
   group
   with-style
   with-style-color
   with-id
   hsplit
   child
   tab-bar
   tab-item
   text-colored
   list-box
   app
   app-add
   app-remove
   remove-all-apps
   app-activep
   app-tick
   defonce))

;; Evaluate things in the top level process

(defvar *top-level-process*
  (find 'si:top-level (mp:all-processes) :key #'mp:process-name))

(defvar *top-level-eval-mailbox*
  (mp:make-mailbox))

(defmacro eval-in-top-level (&body forms)
  `(progn
     (mp:interrupt-process *top-level-process*
                           (lambda ()
                             (mp:mailbox-send *top-level-eval-mailbox*
                                              (multiple-value-list (progn ,@forms)))))
     (values-list (mp:mailbox-read *top-level-eval-mailbox*))))

;; Colors

(defvar *named-colors* nil)

(defun rgb-to-bgra (value)
  (let ((result #xFF000000))
    (setf (ldb (byte 8 0) result) (ldb (byte 8 16) value))
    (setf (ldb (byte 8 8) result) (ldb (byte 8 8) value))
    (setf (ldb (byte 8 16) result) (ldb (byte 8 0) value))
    result))

(defun named-colors-init ()
  (let ((table (make-hash-table)))
    (loop for (name value) on *named-colors-plist* by #'cddr
          do (setf (gethash name table)
                   (rgb-to-bgra value)))
    table))

(defun color (value &optional alpha)
  (let ((value (color-noalpha value)))
    (when alpha
      (setf (ldb (byte 8 24) value) alpha))
    value))

(defun color-noalpha (value)
  (etypecase value
    (keyword
     (unless *named-colors*
       (setf *named-colors* (named-colors-init)))
     (or (gethash value *named-colors*)
         (error "There is no color named ~S." value)))
    (integer
     value)
    (cons
     (destructuring-bind (r g b &optional (a #xFF)) value
       (dpb r (byte 8 0)
            (dpb g (byte 8 8)
                 (dpb b (byte 8 16)
                      (dpb a (byte 8 24) 0))))))))

;; Convenience operators

(defmacro window (name &body forms)
  (let ((ret (gensym)))
    `(let ((,ret (begin ,name)))
       (unwind-protect (when ,ret ,@forms)
         (end)))))

(defmacro tooltip (&body forms)
  `(progn
     (begin-tooltip)
     (unwind-protect (progn ,@forms)
       (end-tooltip))))

(defmacro group (&body forms)
  `(progn
     (begin-group)
     (unwind-protect (progn ,@forms)
       (end-group))))

(defmacro with-style ((&rest properties) &body forms)
  (cond ((null properties)
         `(progn ,@forms))
        (t
         (let ((varname (pop properties))
               (varval (pop properties)))
           `(progn
              (push-style-var ,varname ,varval)
              (unwind-protect
                   (with-style (,@properties)
                     ,@forms)
                (pop-style-var)))))))

(defmacro with-style-color ((&rest properties) &body forms)
  (cond ((null properties)
         `(progn ,@forms))
        (t
         (let ((varname (pop properties))
               (varvalform (pop properties)))
           (let ((varval (gensym)))
             `(let ((,varval ,varvalform))
                (push-style-color ,varname
                                  (if (keywordp ,varval)
                                      (color ,varval)
                                      ,varval))
                (unwind-protect
                     (with-style-color (,@properties)
                       ,@forms)
                  (pop-style-color))))))))

(defmacro with-id (id &body forms)
  `(progn
     (push-id ,id)
     (unwind-protect (progn ,@forms)
       (pop-id))))

(defmacro hsplit (&body forms)
  (case (length forms)
    (0 `(values))
    (1 (first forms))
    (t `(progn
          ,@(butlast
             (loop for form in forms
                   collect form
                   collect `(same-line)))))))

(defmacro child ((name &rest more-args) &body forms)
  `(progn
     (begin-child ,name ,@more-args)
     (unwind-protect (progn ,@forms)
       (end-child))))

(defmacro tab-bar (&body forms)
  `(when (begin-tab-bar)
     (unwind-protect (progn ,@forms)
       (end-tab-bar))))

(defmacro tab-item (label &body forms)
  `(when (begin-tab-item ,label)
     (unwind-protect (progn ,@forms)
       (end-tab-item))))

(defun text-colored (color text)
  (%text-colored (color color) text))

(defmacro list-box ((name &rest more-args) &body forms)
  `(when (begin-listbox ,name ,@more-args)
     (unwind-protect (progn ,@forms)
       (end-listbox))))

;; Apps

;; For lack of a better name, I call them apps.  Currently they are
;; composed of an entry point, which is a function that runs every
;; tick, and a UI state, which begins as "normal" but can transition
;; into "error" in case an error escapes the app.

(defstruct app
  entry-point
  (ui-state '(:normal)))

(defvar *apps* '())

(defun app-add (entry-point)
  (push (make-app :entry-point entry-point) *apps*)
  t)

(defun app-remove (entry-point)
  (setf *apps*
        (remove-if (lambda (app)
                     (eql (app-entry-point app) entry-point))
                   *apps*
                   :count 1))
  t)

(defun remove-all-apps ()
  (setf *apps* nil))

(defun app-activep (entry-point)
  (find-if (lambda (app)
             (eql (app-entry-point app) entry-point))
           *apps*))

(defvar *current-app* nil)

(defmacro with-error-reporting (&body forms)
  `(handler-case (progn ,@forms)
     (error (e)
       (setf (app-ui-state *current-app*)
             (list :error e)))))

(defun window-error-report (condition)
  (window "Lisp error"
    (with-style-color (:text :red)
      (text (format nil "A Lisp error of type ~S was encountered"
                    (type-of condition))))
    (text (princ-to-string condition))
    (when (button "Retry")
      (setf (app-ui-state *current-app*)
            '(:normal)))))

(defun app-tick ()
  (with-simple-restart (return-from-app-tick "Return from APP-TICK")
    (dolist (*current-app* *apps*)
      (ecase (car (app-ui-state *current-app*))
        (:normal
         (with-error-reporting
           (funcall (app-entry-point *current-app*))))
        (:error
         (window-error-report (cadr (app-ui-state *current-app*))))))))

(defmacro defonce (&body forms)
  `(labels ((app ()
              (unwind-protect
                   (progn ,@forms)
                (app-remove #'app))))
     (app-add #'app)))
