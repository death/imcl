;; Swank server (port 4242)

(require :asdf)
(load "~/quicklisp/setup.lisp")
(setf ql:*local-project-directories*
      (append ql:*local-project-directories*
              (list (ql:qmerge "third-party/"))))
(ql:quickload "swank")
(swank:create-server :port 4242 :dont-close t)

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

;; GL environment

(defun setup-gl-environment ()
  (ql:quickload "cl-opengl")
  (ql:quickload "cl-glfw3")
  (set (find-symbol "*WINDOW*" "GLFW") *glfw-window*)
  t)

;; Colors

(defvar *named-colors* nil)

(defun rgb-to-bgra (value)
  (let ((result #xFF000000))
    (setf (ldb (byte 8 0) result) (ldb (byte 8 16) value))
    (setf (ldb (byte 8 8) result) (ldb (byte 8 8) value))
    (setf (ldb (byte 8 16) result) (ldb (byte 8 0) value))
    result))

(defun named-colors-init ()
  (load "colors")
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
     value)))

;; Convenience macros

(defmacro window (name &body forms)
  `(unwind-protect
        (when (begin ,name)
          ,@forms)
     (end)))

(defmacro group (&body forms)
  `(progn
     (begin-group)
     ,@forms
     (end-group)))

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
     (unwind-protect
          (progn ,@forms)
       (end-child))))

;; Calculator

(defvar *stack* '())
(defvar *current* 0)
(defvar *calc-button-width* 45)
(defvar *calc-button-height* 0)

(defun calc-button (object)
  (button (princ-to-string object)
          (list *calc-button-width*
                *calc-button-height*)))

(defun digit (i)
  (when (calc-button i)
    (setf *current* (+ (* *current* 10) i))))

(defun stack ()
  (when *stack*
    (dolist (x *stack*)
      (text (princ-to-string x)))
    (separator)))

(defun current ()
  (text (princ-to-string *current*))
  (separator))

(defun enter ()
  (when (plusp *current*)
    (push *current* *stack*)
    (setf *current* 0)))

(defun cs-button ()
  (when (calc-button "CS")
    (setf *stack* '())))

(defun c-button ()
  (when (calc-button "C")
    (setf *current* 0)))

(defmacro binop (op)
  `(when (calc-button ,(symbol-name op))
     (enter)
     (when (cdr *stack*)
       (let ((rhs (pop *stack*))
             (lhs (pop *stack*)))
         (push (,op lhs rhs) *stack*)))))

(defun pop-button ()
  (when (calc-button "POP")
    (pop *stack*)))

(defun enter-button ()
  (when (calc-button "ENTER")
    (enter)))

(defun window-calc ()
  (window "Calc"
    (stack)
    (current)
    (group
      (group (digit 7) (digit 4) (digit 1))
      (same-line)
      (group (digit 8) (digit 5) (digit 2) (digit 0))
      (same-line)
      (group (digit 9) (digit 6) (digit 3))
      (same-line)
      (group (cs-button) (binop *) (binop +) (pop-button))
      (same-line)
      (group (c-button) (binop /) (binop -) (enter-button)))))

;; Current Time

(defun current-time-string ()
  (multiple-value-bind (s m h date month year)
      (get-decoded-time)
    (format nil "~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date h m s)))

(defun window-current-time ()
  (window "Current time"
    (text (current-time-string))))

;; Property Editor example port from C++

(defvar *property-editor-float-values*
  (vector 1.0 1.0 1.0 1.0 1.0 1.0 0.1 0.1))

(defun window-property-editor ()
  (set-next-window-size '(430 450) :first-use-ever)
  (window "Property Editor"
    (with-style (:frame-padding '(2 2))
      (columns 2)
      (separator)
      (dolist (object '(1 2 3))
        (with-id object
          (align-text-to-frame-padding)
          (let ((node-open (tree-node "Object")))
            (next-column)
            (align-text-to-frame-padding)
            (text "my sailor is rich")
            (next-column)
            (when node-open
              (unwind-protect
                   (dotimes (i 8)
                     (with-id i
                       (align-text-to-frame-padding)
                       (next-column)
                       (set-next-item-width -1)
                       (if (> i 5)
                           (setf (aref *property-editor-float-values* i)
                                 (input-float "##value" (aref *property-editor-float-values* i) 1.0))
                           (setf (aref *property-editor-float-values* i)
                                 (drag-float "##value" (aref *property-editor-float-values* i))))
                       (next-column)))
                (tree-pop))))))
      (columns 1)
      (separator))))

;; Inspector

(defmacro with-inspector-node (name &body forms)
  (let ((node-open (gensym)))
    `(progn
       (align-text-to-frame-padding)
       (let ((,node-open (tree-node ,name)))
         (when ,node-open
           (unwind-protect
                (progn ,@forms)
             (tree-pop)))))))

(defvar *inspector-selection-category* nil)

(defvar *inspector-selection-object* nil)

(defun inspector-node-external-symbols-category (category predicate package)
  (with-inspector-node (category-label category)
    (do-external-symbols (symbol package)
      (when (funcall predicate symbol)
        (with-id (symbol-name symbol)
          (when (selectable (symbol-name symbol)
                            (and (eq *inspector-selection-category* category)
                                 (eq *inspector-selection-object* symbol)))
            (setf *inspector-selection-category* category)
            (setf *inspector-selection-object* symbol)))))))

(defun category-label (category)
  (substitute #\Space #\- (format nil "~:(~A~)" category)))

(defun inspector-package-node (package)
  (inspector-symbols-node package)
  (inspector-special-variables-node package)
  (inspector-operators-node package)
  (inspector-classes-node package))

(defun inspector-symbols-node (package)
  (inspector-node-external-symbols-category 'symbols (constantly t) package))

(defun inspector-special-variables-node (package)
  (inspector-node-external-symbols-category 'special-variables #'c::special-variable-p package))

(defun operator-p (object)
  (and (symbolp object)
       (fboundp object)))

(defun inspector-operators-node (package)
  (inspector-node-external-symbols-category 'operators #'operator-p package))

(defun class-p (object)
  (and (symbolp object)
       (find-class object nil)))

(defun inspector-classes-node (package)
  (inspector-node-external-symbols-category 'classes #'class-p package))

(defgeneric inspector-object-view* (category object))

(defmethod inspector-object-view* (category object)
  (declare (ignore category))
  (text (with-output-to-string (out)
          (describe object out))))

(defmethod inspector-object-view* ((category null) (object null))
  (text "Select an object to inspect."))

(defun inspector-object-view ()
  (inspector-object-view* *inspector-selection-category*
                          *inspector-selection-object*))

(defun window-package-inspector (package)
  (set-next-window-size '(430 450) :first-use-ever)
  (window (format nil "Inspector - Package ~A" (package-name package))
    (with-style (:frame-padding '(2 2))
      (child ("Tree" (list 200 0))
        (inspector-package-node package))
      (same-line)
      (child ("Object")
        (inspector-object-view)))))

(defun window-inspector ()
  (set-next-window-size '(430 450) :first-use-ever)
  (window "Inspector"
    (with-style (:frame-padding '(2 2))
      (columns 2)
      (separator)
      (dolist (package (list-all-packages))
        (with-id (package-name package)
          (align-text-to-frame-padding)
          (let ((node-open (tree-node (package-name package))))
            (next-column)
            (align-text-to-frame-padding)
            (text (or (documentation package 't) ""))
            (next-column)
            (when node-open
              (inspector-package-node package)
              (tree-pop)))))
      (columns 1)
      (separator))))

(defun app-cl-package-inspector ()
  (with-style (:window-rounding 4.0 :alpha 0.9)
    (window-package-inspector (find-package "CL"))))

;; Test

(defun window-test ()
  (window "Test"
    (text "This is a test")))

;; Metrics

(defstruct metric
  name
  unit
  query-function
  window
  counter
  running-total)

(defun metric-update (metric)
  (let ((new-value (funcall (metric-query-function metric))))
    (symbol-macrolet ((current-value (aref (metric-window metric)
                                           (metric-counter metric))))
      (decf (metric-running-total metric) current-value)
      (incf (metric-running-total metric) new-value)
      (setf current-value new-value)
      (setf (metric-counter metric)
            (mod (1+ (metric-counter metric))
                 (metric-window-size metric))))))

(defun metric-mean (metric)
  (float
   (/ (metric-running-total metric)
      (metric-window-size metric))))

(defun metric-window-size (metric)
  (length (metric-window metric)))

(defvar *metrics* '())

(defun add-metric (&key name unit query-function window-size initial-value)
  (setf initial-value (or initial-value 0))
  (push (make-metric :name name
                     :unit unit
                     :query-function query-function
                     :window (make-array window-size
                                         :initial-element initial-value
                                         :element-type '(unsigned-byte 32))
                     :counter 0
                     :running-total (* initial-value window-size))
        *metrics*))

(defun metrics-update ()
  (dolist (metric *metrics*)
    (metric-update metric)))

(defun window-metrics ()
  (metrics-update)
  (window "Metrics"
    (columns 2)
    (dolist (metric *metrics*)
      (text (format nil "~:(~A~) (~(~A~))"
                    (metric-name metric)
                    (metric-unit metric)))
      (next-column)
      (text (format nil "~,1F"
                    (metric-mean metric)))
      (next-column))))

(defun app-metrics ()
  (when (null *metrics*)
    (add-metric :name 'time-in-lisp
                :unit 'us
                :query-function (lambda () *time-in-lisp*)
                :window-size 1000
                :initial-value *time-in-lisp*))
  (window-metrics))

(defun metrics-restart ()
  (app-remove 'app-metrics)
  (setf *metrics* '())
  (app-add 'app-metrics))

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

;; Entry points

(defun init ()
  "INIT runs right after this file is loaded.")

(defun tick ()
  "TICK runs on each iteration of the UI loop."
  (app-tick))
