;; Swank server (port 4242)

(require :asdf)
(load "~/quicklisp/setup.lisp")
(setf ql:*local-project-directories*
      (append ql:*local-project-directories*
              (list "/home/death/dev/imgui-ecl/lisp/"
                    (ql:qmerge "third-party/"))))
(ql:quickload "swank")
(swank:create-server :port 4242 :dont-close t)

;; Entry points

(defun init ()
  "INIT runs right after this file is loaded.")

(defun im-tick ()
  "IM-TICK runs on each iteration of the UI loop."
  (with-simple-restart (skip "Skip this tick")))

(defun gl-tick ()
  "GL-TICK runs on each iteration of the UI loop, before imgui
rendering."
  (with-simple-restart (skip "Skip this tick")))

(ql:quickload "demo")
