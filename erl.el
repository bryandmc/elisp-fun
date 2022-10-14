;; Some comment here

;; @doc expand erlang macro's
(defun erl/expand-macro ()
  (interactive)
  (print "expand macro"))

(defun project-root-pls ()
  (if projectile-project-root
      (projectile-project-root) "."))

(defun erl/test ()
  (interactive)
  (print "Running tests")
  (let*
      ((module
        (spacemacs/copy-file-name-base))
       (output-buffer
        (get-buffer-create "*Erlang test output*"))
       (command
        (format "cd %s/.. && T_WILDCARD=%s make test"
                (project-root-pls) module)))
    (async-shell-command command output-buffer))
  (print "fin."))

(defun erl/fmt ()
  (interactive)
  (print "Running formatter")
  (let*
      ((module
        (spacemacs/copy-file-name-base))
       (output-buffer
        (get-buffer-create "*Erlang fmt output*"))
       (command
        (format "cd %s && /Users/bryanmccoid/dev/workspace/efmt/target/release/efmt -w --allow-partial-failure --parallel %s.erl"
                (project-root-pls) module)))
    (async-shell-command command output-buffer))
  (revert-buffer t t t)
  (print "fin."))


(defun erl/cbshell ()
  (let
      ((the-cmd
        (format "cd %s/.. && cbshell n_0 9000" (project-root-pls))))
    (shell-command the-cmd)
    (inferior-erlang "ls")
    (message the-cmd))
  (print "done."))

;; Example let form so that I don't keep forgetting it.
;;
;; (let ((var-name (format "%s" "something")))
;;   (message var-name))

;; *******************************

(defun formatter-sentinel (process event)
  (print process)
  (print event)
  (message event)
  (when (string-match-p "finished" event)
    (message "successfully formatted buffer.. closing")
    (erl/close-format-window)))

(defun erl/kill-async-formatter ()
  (interactive)
  (erl/close-format-window))

(defun erl/close-format-window ()
  ""
  (kill-buffer "*Async efmt*")
  (delete-other-windows))

(defun erl/format-cmd (module)
    (format "cd %s && /Users/bryanmccoid/dev/workspace/efmt/target/release/efmt -w --allow-partial-failure %s"
            (project-root-pls) module))

(defun erl/format ()
  "Runs erlang formatter on current file"
  (interactive)
  (let* ((file-name (shell-quote-argument (buffer-file-name)))
         (process (start-process-shell-command
                   "efmt"
                   "*Async efmt*"
                   (erl/format-cmd file-name))))
    (set-process-sentinel process 'formatter-sentinel)
    (setq new-window (split-window-below 60))
    (set-window-buffer new-window "*Async efmt*" t)
    (other-window 1)
    (set-process-query-on-exit-flag process nil)
    (local-set-key [t] 'erl/kill-async-formatter))
  (revert-buffer t t t))

;; (defclass person () ; No superclasses
;;   ((name :initarg :name
;;          :initform ""
;;          :type string
;;          :custom string
;;          :documentation "The name of a person.")
;;    (birthday :initarg :birthday
;;              :initform "Jan 1, 1970"
;;              :custom string
;;              :type string
;;              :documentation "The person's birthday.")
;;    (phone :initarg :phone
;;           :initform ""
;;           :documentation "Phone number."))
;;   "A class for tracking people I know.")

;; (cl-defmethod call-person ((pers person) &optional scriptname)
;;   "Dial the phone for the person PERS.
;; Execute the program SCRIPTNAME to dial the phone."
;;   (message "Dialing the phone for %s"  (slot-value pers 'name))
;;   (shell-command (concat (or scriptname "dialphone.sh")
;;                          " "
;;                          (slot-value pers 'phone))))

;; (setq pers (person :name "Bryan" :birthday "June" :phone "555-5555"))
;; (call-person pers)
