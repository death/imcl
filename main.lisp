;; Swank server (port 4242)

(require :asdf)
(load "~/quicklisp/setup.lisp")
(ql:quickload "swank")
(swank:create-server :port 4242 :dont-close t)

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
              (dotimes (i 8)
                (with-id i
                  (align-text-to-frame-padding)
                  (next-column)
                  (set-next-item-width -1)
                  (if (> i 5)
                      (setf (aref *inspector-float-values* i)
                            (input-float "##value" (aref *inspector-float-values* i) 1.0))
                      (setf (aref *inspector-float-values* i)
                            (drag-float "##value" (aref *inspector-float-values* i))))
                  (next-column)))
              (tree-pop)))))
      (columns 1)
      (separator))))

;; Inspector

(defmacro with-inspector-node (name &body forms)
  (let ((node-open (gensym)))
    `(progn
       (align-text-to-frame-padding)
       (let ((,node-open (tree-node ,name)))
         (unwind-protect
              (when ,node-open
                (unwind-protect
                     (progn ,@forms)
                  (tree-pop)))
           (next-column)
           (next-column))))))

(defun inspector-package-node (package)
  (inspector-symbols-node package)
  (inspector-special-variables-node package)
  (inspector-functions-node package)
  (inspector-macros-node package)
  (inspector-classes-node package))

(defun inspector-symbols-node (package)
  (with-inspector-node "Symbols"
    (do-external-symbols (symbol package)
      (with-id (symbol-name symbol)
        (with-inspector-node (symbol-name symbol))))))

(defun inspector-special-variables-node (package)
  (with-inspector-node "Special Variables"
    (do-external-symbols (symbol package)
      (when (c::special-variable-p symbol)
        (with-id (symbol-name symbol)
          (with-inspector-node (symbol-name symbol)))))))

(defun inspector-functions-node (package)
  (with-inspector-node "Functions"
    (do-external-symbols (symbol package)
      (when (and (fboundp symbol)
                 (not (special-operator-p symbol))
                 (null (macro-function symbol)))
        (with-id (symbol-name symbol)
          (with-inspector-node (symbol-name symbol)))))))

(defun inspector-macros-node (package)
  (with-inspector-node "Macros"
    (do-external-symbols (symbol package)
      (when (and (fboundp symbol)
                 (or (special-operator-p symbol)
                     (macro-function symbol)))
        (with-id (symbol-name symbol)
          (with-inspector-node (symbol-name symbol)))))))

(defun inspector-classes-node (package)
  (with-inspector-node "Classes"
    (do-external-symbols (symbol package)
      (let ((class (find-class symbol nil)))
        (when class
          (with-id (symbol-name symbol)
            (with-inspector-node (symbol-name symbol))))))))

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

(defun window-package-inspector (package)
  (set-next-window-size '(430 450) :first-use-ever)
  (window (format nil "Inspector - Package ~A" (package-name package))
    (with-style (:frame-padding '(2 2))
      (columns 2)
      (separator)
      (inspector-package-node package)
      (columns 1)
      (separator))))

;; Test

(defun window-test ()
  (window "Test"
    (text "This is a test")))

;; User tick

(defun user-tick ()
  (with-style (:window-rounding 4.0 :alpha 0.9)
    (window-package-inspector (find-package "CL"))))

;; Entry points

(defun init ())

(defun tick ()
  ;; TODO: Find out a way to have an nonblocking debugger using ECL.
  (handler-case
      (user-tick)
    (error (e)
      (set-next-window-size '(430 450) :first-use-ever)
      (window "Lisp error"
        (text (format nil "~A~%" e))))))
