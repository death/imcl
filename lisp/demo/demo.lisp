;;;; +----------------------------------------------------------------+
;;;; | IMCL                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:demo
  (:use #:cl #:imcl))

(in-package #:demo)

;; Menus

(defvar *main-menu*
  '(("Trivial"
     ("Menu Test" show-menu-test)
     ("Window Test" window-test)
     ("Current Time" window-current-time)
     ("Clipboard Test" show-clipboard-test)
     ("Style Chooser" show-style-chooser)
     ("Demo" show-demo-window))
    ("Easy"
     ("Calculator" window-calc)
     ("Property Editor" window-property-editor)
     ("Inspector" window-inspector)
     ("CL Package Inspector" app-cl-package-inspector)
     ("Metrics" app-metrics)
     ("Basic Widgets" show-basic-widgets)
     ("Colors" show-colors-window)
     ("Information" show-info-window)
     ("Drag & Drop" show-dnd-window)
     ("Textures" show-texture-test)
     ("Vecto Fun" show-vecto-fun))
    ("OpenGL"
     ("Load GL demo" (load-gl-demo)))
    ("Windows"
     ("Close All" remove-all-apps))))

(defun main-menu ()
  (when (begin-main-menu-bar)
    (loop for (menu . items) in *main-menu*
          do (when (begin-menu menu)
               (dolist (entry items)
                 (if (eq entry :separator)
                     (separator)
                     (destructuring-bind (name form) entry
                       (when (menu-item name)
                         (cond ((symbolp form)
                                (app-add form))
                               ((listp form)
                                (apply (car form) (cdr form))))))))
               (end-menu)))
    (end-main-menu-bar)))

(defun show-menu-test ()
  (when (begin "Menus" :menu-bar)
    (when (begin-menu-bar)
      (when (begin-menu "Menu")
        (when (begin-menu "Submenu")
          (when (menu-item "Close")
            (app-remove 'show-menu-test))
          (end-menu))
        (end-menu))
      (end-menu-bar))
    (end)))

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
  (vector (list 1.0)
          (list 1.0 1.0)
          (list 1.0 1.0 1.0)
          (list 1.0 1.0 1.0 1.0)
          (list 1.0)
          (list 1.0 1.0)
          (list 1.0 1.0 1.0)
          (list 1.0 1.0 1.0 1.0)))

(defun window-property-editor ()
  (set-next-window-pos '(100 100) :first-use-ever)
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
                       (if (>= i 4)
                           (input-float "##value" (aref *property-editor-float-values* i) 1)
                           (drag-float "##value" (aref *property-editor-float-values* i)))
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
  (window nil :type (simple-array single-float))
  (counter 0 :type (unsigned-byte 16))
  (running-total 0.0f0 :type single-float))

(defun metric-update (metric)
  (declare (optimize (speed 3)))
  (declare (type metric metric))
  (let ((new-value (coerce (funcall (metric-query-function metric)) 'single-float))
        (window (metric-window metric))
        (counter (metric-counter metric)))
    (declare (type single-float new-value))
    (declare (type (simple-array single-float) window))
    (declare (type (unsigned-byte 16) counter))
    (decf (metric-running-total metric) (aref window counter))
    (incf (metric-running-total metric) new-value)
    (setf (aref window counter) new-value)
    (setf (metric-counter metric)
          (mod (1+ counter) (metric-window-size metric)))))

(defun metric-mean (metric)
  (coerce
   (/ (metric-running-total metric)
      (metric-window-size metric))
   'single-float))

(defun metric-window-size (metric)
  (length (metric-window metric)))

(defvar *metrics* '())

(defun add-metric (&key name unit query-function window-size initial-value)
  (setf initial-value (coerce (or initial-value 0.0) 'single-float))
  (push (make-metric :name name
                     :unit unit
                     :query-function query-function
                     :window (make-array window-size
                                         :initial-element initial-value
                                         :element-type 'single-float)
                     :counter 0
                     :running-total (* initial-value window-size))
        *metrics*))

(defun metrics-update ()
  (dolist (metric *metrics*)
    (metric-update metric)))

(defun window-metrics ()
  (metrics-update)
  (window "Metrics"
    (dolist (metric *metrics*)
      (plot-lines (format nil "~:(~A~) (~(~A~))"
                          (metric-name metric)
                          (metric-unit metric))
                  (metric-window metric)
                  (metric-counter metric)
                  (format nil "~,1F" (metric-mean metric))
                  0.0
                  1800.0
                  '(350 80)))))

(defun app-metrics ()
  (when (null *metrics*)
    (add-metric :name 'time-in-im-tick
                :unit 'us
                :query-function (lambda () *time-in-im-tick*)
                :window-size 1000
                :initial-value *time-in-im-tick*)
    (add-metric :name 'time-in-gl-tick
                :unit 'us
                :query-function (lambda () *time-in-gl-tick*)
                :window-size 1000
                :initial-value *time-in-gl-tick*))
  (window-metrics))

(defun metrics-restart ()
  (app-remove 'app-metrics)
  (setf *metrics* '())
  (app-add 'app-metrics))

;; Basic Widgets

(defclass basic-widgets-model ()
  ((button-toggle :initform nil :accessor bw-button-toggle)
   (checkbox-toggle :initform nil :accessor bw-checkbox-toggle)
   (radio-choice :initform 0 :accessor bw-radio-choice)
   (combo-items :initform #("Apple" "Banana" "Cherry" "Dill Pickle" "Earwax")
                :accessor bw-combo-items)
   (combo-choice :initform 0 :accessor bw-combo-choice)
   (slider-float-values-list
    :initform (copy-tree '((0.0) (0.1 0.2) (0.3 0.4 0.5) (0.6 0.7 0.8 0.9)))
    :reader bw-slider-float-values-list)
   (slider-angle-value :initform (list 0.0) :reader bw-slider-angle-value)
   (slider-int-values-list
    :initform (copy-tree '((0) (1 2) (3 4 5) (6 7 8)))
    :reader bw-slider-int-values-list)
   (invisible-button-toggle :initform nil :accessor bw-invisible-button-toggle)
   (progress :initform 0.0 :accessor bw-progress)
   (single-line :initform (list "") :accessor bw-single-line)
   (bunch-o-lines :initform (list "") :accessor bw-bunch-o-lines)
   (float-range :initform (list 1.0 5.0) :accessor bw-float-range)
   (int-range :initform (list 1 5) :accessor bw-int-range)
   (symbol-histogram :initform nil :accessor bw-symbol-histogram)))

(defvar *basic-widgets-model*
  (make-instance 'basic-widgets-model))

(defun show-basic-widgets (&aux (model *basic-widgets-model*))
  (window "Basic Widgets"
    (text "This is a text widget")
    (when (button "Click Me")
      (setf (bw-button-toggle model)
            (not (bw-button-toggle model))))
    (when (bw-button-toggle model)
      (same-line)
      (text "Thanks for clicking me")
      (when (item-hovered-p)
        (set-tooltip "Really, thanks!")))
    (when (button "Show popup")
      (open-popup "mypopup"))
    (when (begin-popup "mypopup")
      (text "This is a popup")
      (end-popup))
    (when (button "Show modal")
      (open-popup "Hello"))
    (when (begin-popup-modal "Hello")
      (text "This is a modal")
      (when (button "Done")
        (close-current-popup))
      (end-popup))
    (setf (bw-checkbox-toggle model)
          (checkbox "Checkbox" (bw-checkbox-toggle model)))
    (dotimes (i 3)
      (when (radio-button (format nil "Radio ~D" i) (= (bw-radio-choice model) i))
        (setf (bw-radio-choice model) i))
      (when (item-hovered-p)
        (tooltip
          (bullet-text
           (aref #("First option" "Second option" "Third option") i))))
      (when (< i 2)
        (same-line)))
    (when (begin-combo "Edibles" (aref (bw-combo-items model) (bw-combo-choice model)))
      (dotimes (i (length (bw-combo-items model)))
        (let ((is-selected (= i (bw-combo-choice model))))
          (when (selectable (aref (bw-combo-items model) i) is-selected)
            (setf (bw-combo-choice model) i))
          (when is-selected
            (set-item-default-focus))))
      (end-combo))
    (tab-bar
      (tab-item "Item 1"
        (text "Content for first tab item")
        (button "This is a button")
        (small-button "This is a small button")
        (when (invisible-button "This is an invisible button" '(10 10))
          (setf (bw-invisible-button-toggle model)
                (not (bw-invisible-button-toggle model))))
        (when (bw-invisible-button-toggle model)
          (text "You found it!"))
        (dolist (dir '(:left :right :up :down))
          (arrow-button (format nil "arrow-~A" dir) dir)
          (same-line))
        (bullet)
        (new-line)
        (drag-float-range "Float Range" (bw-float-range model))
        (drag-int-range "Int Range" (bw-int-range model))
        (when (null (bw-symbol-histogram model))
          (let ((histogram (make-array 5 :initial-element 0.0 :element-type 'single-float)))
            (do-external-symbols (symbol "CL")
              (let* ((is-bound (boundp symbol))
                     (is-fbound (and (not is-bound) (fboundp symbol)))
                     (is-specop (and is-fbound (special-operator-p symbol)))
                     (is-mac (and is-fbound (not is-specop) (macro-function symbol)))
                     (is-func (and is-fbound (not is-specop) (not is-mac)))
                     (index (cond (is-specop 4)
                                  (is-mac 3)
                                  (is-func 2)
                                  (is-bound 1)
                                  (t 0))))
                (incf (aref histogram index))))
            (setf (bw-symbol-histogram model) histogram)))
        (plot-histogram "Symbol partitioning"
                        (bw-symbol-histogram model)
                        0
                        "S M F V U"
                        0.0
                        1000.0
                        '(150 80)))
      (tab-item "Item 2"
        (text "Content for second tab item")
        (progress-bar (bw-progress model))
        (when (< (random 1.0) 0.1)
          (incf (bw-progress model) (random 0.05))
          (when (> (bw-progress model) 1.0)
            (setf (bw-progress model) 0.0)))
        (input-text "Your Name" (bw-single-line model))
        (when (plusp (length (car (bw-single-line model))))
          (text (format nil "Hi there, ~A" (car (bw-single-line model)))))
        (input-text-with-hint "Come Again?" "Computers May Have Trouble Hearing" (bw-single-line model))
        (input-text-multiline "Life Story" (bw-bunch-o-lines model)))
      (tab-item "Sliders"
        (loop for i upfrom 1
              for v in (bw-slider-float-values-list model)
              do (slider-float (format nil "float ~R" i) v))
        (slider-angle "angle" (bw-slider-angle-value model))
        (loop for i upfrom 1
              for v in (bw-slider-int-values-list model)
              do (slider-int (format nil "int ~R" i) v))
        (vslider-float "vert float one" '(40 100) (first (bw-slider-float-values-list model)))
        (same-line)
        (vslider-int "vert int one" '(40 100) (first (bw-slider-int-values-list model)))))))

;; Clipboard test

(defclass clipboard-test-model ()
  ((candidates :initform '#1=("red" "green" "blue" . #1#)
               :accessor ct-candidates)))

(defvar *clipboard-test-model*
  (make-instance 'clipboard-test-model))

(defun show-clipboard-test (&aux (model *clipboard-test-model*))
  (window "Clipboard test"
    (text (format nil "Text in clipboard: [~A]" (get-clipboard-text)))
    (when (button "Set clipboard text")
      (set-clipboard-text (pop (ct-candidates model))))))

;; Style chooser

(defun show-style-chooser ()
  (window "Styles"
    (when (button "Dark")
      (style-colors :dark))
    (when (button "Light")
      (style-colors :light))
    (when (button "Classic")
      (style-colors :classic))))

;; Colors List

(defclass colors-window-model ()
  ((colors :initform (coerce (loop for (name value) on *named-colors-plist* by #'cddr
                                   collect name)
                             'vector)
           :reader colors-window-colors)
   (current :initform 0
            :accessor colors-window-current)
   (color1 :initform (list 0.8 0.0 0.0)
           :accessor colors-window-color1)
   (color2 :initform (list 0.1 0.5 0.0 1.0)
           :accessor colors-window-color2)))

(defvar *colors-window-model*
  (make-instance 'colors-window-model))

(defun show-colors-window (&optional (model *colors-window-model*))
  (let ((colors (colors-window-colors model)))
    (window "Colors"
      (tab-bar
        (tab-item "Named colors"
          (symbol-macrolet ((current (colors-window-current model)))
            (begin-listbox "Color names" (length colors) 10)
            (dotimes (i (length colors))
              (let ((color (aref colors i)))
                (when (selectable (string-capitalize (substitute #\Space #\- (symbol-name color)))
                                  (= i current))
                  (setf current i))))
            (end-listbox)
            (color-button "Current color" (color (aref colors current)) :none '(20 20))
            (same-line)
            (text-colored (aref colors current)
                          "The Quick Brown Fox Jumped Over The Lazy Dog's Back")))
        (tab-item "Widgets"
          (columns 2)
          (set-column-width 0 300)
          (set-column-width 1 400)
          (color-picker "RGB picker" (colors-window-color1 model))
          (next-column)
          (color-edit "RGB edit" (colors-window-color1 model))
          (color-edit "HSV edit" (colors-window-color1 model) '(:display-hsv :picker-hue-wheel))
          (columns)
          (separator)
          (color-edit "RGBA edit" (colors-window-color2 model) '(:alpha-bar :no-inputs :alpha-preview)))))))

;; Info

(defun yesno (boolean)
  (text (if boolean "Yes" "No")))

(defun ago (universal-time)
  (let* ((now (get-universal-time))
         (duration (- now universal-time)))
    (cond ((zerop duration)
           "Right about now (funk soul brotha)")
          ((= -1 duration)
           "1 second in the future")
          ((minusp duration)
           (format nil "~D seconds in the future" (- duration)))
          ((= 1 duration)
           "1 second ago")
          (t
           (format nil "~D seconds ago" duration)))))

(defclass info-model ()
  ((last-appearing :initform nil :accessor info-last-appearing)
   (last-collapsed :initform nil :accessor info-last-collapsed)
   (toggles :initform (make-hash-table) :accessor info-toggles)))

(defvar *info-model*
  (make-instance 'info-model))

(defun show-info-window (&optional (model *info-model*))
  (let ((show (begin "Information")))
    (unwind-protect
         (flet ((toggle (thing)
                  (if (gethash thing (info-toggles model))
                      (remhash thing (info-toggles model))
                      (setf (gethash thing (info-toggles model)) t)))
                (enabledp (thing)
                  (gethash thing (info-toggles model)))
                (disable (thing)
                  (remhash thing (info-toggles model))))
           (let ((appearing (window-appearing-p))
                 (collapsed (window-collapsed-p)))
             (when appearing
               (setf (info-last-appearing model) (get-universal-time))
               (setf (info-last-collapsed model) nil))
             (when collapsed
               (setf (info-last-collapsed model) (get-universal-time)))
             (when show
               (when (button "About")
                 (toggle :about))
               (same-line)
               (when (button "Metrics")
                 (toggle :metrics))
               (tab-bar
                 (tab-item "Window"
                   (columns 2)
                   (text "Appearing?")
                   (next-column)
                   (yesno appearing)
                   (next-column)
                   (text "Last Appearing")
                   (next-column)
                   (text (if (null (info-last-appearing model)) "-" (ago (info-last-appearing model))))
                   (next-column)
                   (text "Collapsed?")
                   (next-column)
                   (yesno collapsed)
                   (next-column)
                   (text "Last Collapsed")
                   (next-column)
                   (text (if (null (info-last-collapsed model)) "-" (ago (info-last-collapsed model))))
                   (next-column)
                   (text "Focused?")
                   (next-column)
                   (yesno (window-focused-p))
                   (next-column)
                   (text "Hovered?")
                   (next-column)
                   (yesno (window-hovered-p))
                   (next-column)
                   (text "Position")
                   (next-column)
                   (multiple-value-bind (x y) (get-window-pos)
                     (text (format nil "~D, ~D" x y)))
                   (next-column)
                   (text "Size")
                   (next-column)
                   (multiple-value-bind (x y) (get-window-size)
                     (text (format nil "~D, ~D" x y)))
                   (columns 1))
                 (tab-item "Style Editor"
                   (show-style-editor))
                 (tab-item "Inputs"
                   (when (collapsing-header "Keyboard" :leaf)
                     (let ((ret (get-key-index :enter)))
                       (columns 2)
                       (text "(RET) Key index")
                       (next-column)
                       (text (format nil "~D" ret))
                       (next-column)
                       (text "(RET) Key down?")
                       (next-column)
                       (text (if (key-down-p ret) "Yes" "No"))
                       (next-column)
                       (text "(RET) Key pressed?")
                       (next-column)
                       (text (if (key-pressed-p ret) "Yes" "No"))
                       (next-column)
                       (text "(RET) Key released?")
                       (next-column)
                       (text (if (key-released-p ret) "Yes" "No"))
                       (next-column)
                       (text "(RET) Key pressed amount")
                       (next-column)
                       (text (format nil "~D" (get-key-pressed-amount ret)))
                       (columns 1)))
                   (when (collapsing-header "Mouse" :leaf)
                     (columns 2)
                     (text "Mouse cursor")
                     (next-column)
                     (text (format nil "~A" (get-mouse-cursor)))
                     (next-column)
                     (text "Mouse position")
                     (next-column)
                     (multiple-value-bind (x y)
                         (get-mouse-pos)
                       (text (format nil "~F, ~F" x y)))
                     (next-column)
                     (text "Is any mouse down?")
                     (next-column)
                     (text (if (any-mouse-down-p) "Yes" "No"))
                     (next-column)
                     (text "(L) Is mouse down?")
                     (next-column)
                     (text (if (mouse-down-p) "Yes" "No"))
                     (next-column)
                     (text "(L) Is mouse clicked?")
                     (next-column)
                     (text (if (mouse-clicked-p) "Yes" "No"))
                     (next-column)
                     (text "(L) Is mouse double-clicked?")
                     (next-column)
                     (text (if (mouse-double-clicked-p) "Yes" "No"))
                     (next-column)
                     (text "(L) Is mouse released?")
                     (next-column)
                     (text (if (mouse-released-p) "Yes" "No"))
                     (next-column)
                     (text "(L) Is mouse dragging?")
                     (next-column)
                     (text (if (mouse-dragging-p) "Yes" "No"))
                     (next-column)
                     (text "(L) Mouse drag delta")
                     (next-column)
                     (multiple-value-bind (x y)
                         (get-mouse-drag-delta)
                       (text (format nil "~F, ~F" x y)))
                     (columns 1)))
                 (tab-item "Others"
                   (text "Version")
                   (indent)
                   (text (imgui-version))
                   (unindent)
                   (text "Style selector")
                   (indent)
                   (show-style-selector "Current style")
                   (unindent)
                   (text "Font selector")
                   (indent)
                   (show-font-selector "Current font")
                   (unindent)
                   (text "User guide")
                   (indent)
                   (show-user-guide)
                   (unindent)))
               (when (enabledp :about)
                 (when (not (show-about-window))
                   (disable :about)))
               (when (enabledp :metrics)
                 (when (not (show-metrics-window))
                   (disable :metrics))))))
      (end))))

;; Drag and Drop

(defclass dnd-model ()
  ((mode :initform :copy :accessor dnd-mode)
   (names :initform (vector "Bobby" "Beatrice" "Betty"
                            "Brianna" "Barry" "Bernard"
                            "Bibi" "Blaine" "Bryn")
          :accessor dnd-names)))

(defvar *dnd-model*
  (make-instance 'dnd-model))

(defun show-dnd-window (&optional (model *dnd-model*))
  (window "Drag & Drop"
    (text "Drag and drop to copy/swap items")
    (when (radio-button "Copy" (eq (dnd-mode model) :copy))
      (setf (dnd-mode model) :copy))
    (same-line)
    (when (radio-button "Move" (eq (dnd-mode model) :move))
      (setf (dnd-mode model) :move))
    (same-line)
    (when (radio-button "Swap" (eq (dnd-mode model) :swap))
      (setf (dnd-mode model) :swap))
    (loop for i upfrom 0
          for name across (dnd-names model)
          do (with-id i
               (when (plusp (mod i 3))
                 (same-line))
               (button name '(60 60))
               (when (begin-drag-drop-source)
                 (set-drag-drop-payload i)
                 (text (format nil "~@(~A~) ~A" (dnd-mode model) name))
                 (end-drag-drop-source))
               (when (begin-drag-drop-target)
                 (let ((payload (accept-drag-drop-payload)))
                   (when payload
                     (ecase (dnd-mode model)
                       (:copy
                        (setf (aref (dnd-names model) i)
                              (aref (dnd-names model) payload)))
                       (:move
                        (shiftf (aref (dnd-names model) i)
                                (aref (dnd-names model) payload)
                                ""))
                       (:swap
                        (rotatef (aref (dnd-names model) i)
                                 (aref (dnd-names model) payload))))))
                 (end-drag-drop-target))))))

;; Textures

(defclass texture-test-model ()
  ((texture :initform nil :accessor tt-texture)
   (width :initform nil :accessor tt-width)
   (height :initform nil :accessor tt-height)))

(defvar *texture-test-model*
  (make-instance 'texture-test-model))

(defun show-texture-test (&optional (model *texture-test-model*))
  (window "Textures"
    (when (null (tt-texture model))
      (setf (values (tt-texture model)
                    (tt-width model)
                    (tt-height model))
            (load-texture "lisplogo_alien_256.png")))
    (when (tt-texture model)
      (image (tt-texture model)
             (list (tt-width model) (tt-height model))))))

;; Vecto fun

(defclass vecto-fun-model ()
  ((texture :initform nil :accessor vf-texture)
   (width :initform nil :accessor vf-width)
   (height :initform nil :accessor vf-height)))

(defvar *vecto-fun-model*
  (make-instance 'vecto-fun-model))

(defun show-vecto-fun (&optional (model *vecto-fun-model*))
  (window "Vecto Fun"
    (unless (vf-texture model)
      (vecto-draw-something))
    (when (vf-texture model)
      (image (vf-texture model)
             (list (vf-width model) (vf-height model))))))

(defun vecto-save-texture (&optional (model *vecto-fun-model*))
  (let* ((state vecto::*graphics-state*)
         (width (vecto::width state))
         (height (vecto::height state))
         (image (vecto::image state))
         (data (zpng:image-data image))
         (texture (create-texture width height 4 data)))
    (when (vf-texture model)
      (delete-texture (vf-texture model))
      (setf (vf-texture model) nil))
    (setf (vf-width model) width)
    (setf (vf-height model) height)
    (setf (vf-texture model) texture)))

(defun vecto-draw-something ()
  (let ((width 100)
        (height 100))
    ;; Example taken from vecto's doc/examples.lisp
    (vecto:with-canvas (:width width :height height)
      (vecto:set-rgb-fill 1.0 0.65 0.3)
      (vecto:rounded-rectangle 0 0 100 100 10 10)
      (vecto:fill-path)
      (vecto:set-rgb-fill 1.0 1.0 1.0)
      (vecto:centered-circle-path 20 20 10)
      (vecto:fill-path)
      (flet ((quarter-circle (x y radius)
               (vecto:move-to (+ x radius) y)
               (vecto:arc x y radius 0 (/ pi 2))))
        (vecto:set-rgb-stroke 1.0 1.0 1.0)
        (vecto:set-line-width 15)
        (quarter-circle 20 20 30)
        (vecto:stroke)
        (quarter-circle 20 20 60)
        (vecto:stroke))
      (vecto:rounded-rectangle 5 5 90 90 7 7)
      (vecto:set-gradient-fill 50 90
                               1.0 1.0 1.0 0.7
                               50 20
                               1.0 1.0 1.0 0.0)
      (vecto:set-line-width 2)
      (vecto:set-rgba-stroke 1.0 1.0 1.0 0.1)
      (vecto:fill-and-stroke)
      (vecto-save-texture))))

;; GL demo

(defvar *loaded-gl-demo* nil)

(defun load-gl-demo ()
  (unless *loaded-gl-demo*
    (ql:quickload "gl-demo")
    (let ((setup (find-symbol "SETUP" "GL-DEMO")))
      (when setup
        (setf *main-menu* (funcall setup *main-menu*))))
    (setf *loaded-gl-demo* t)))

;; Entry points

(defun cl-user::im-tick ()
  "IM-TICK runs on each iteration of the UI loop."
  (with-simple-restart (skip "Skip this tick")
    (main-menu)
    (app-tick)))
