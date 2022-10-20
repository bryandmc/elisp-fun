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
  (let
      ((the-window
        (get-buffer-window "*Async efmt*")))
    (select-window the-window)
    (kill-buffer-and-window)))

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
    (other-window 1 nil t)
    (set-process-query-on-exit-flag process nil)
    (local-set-key [t] 'erl/kill-async-formatter))
  (revert-buffer t t t))

;; ##############################################

(defun formatter-sentinel-generic (process event)
  (print process)
  (print event)
  (message event)
  (when (string-match-p "finished" event)
    (message "successfully ran command on buffer.. closing")
    (close-window async-cmd-buffer)))


(defun format-buf-name (cmd)
    (format "*Async %s*" cmd))

(defun run-cmd-async-close-success (cmd)
  "Runs command on current file"
  (interactive)
  (let* ((file-name (shell-quote-argument (buffer-file-name)))
         (buf-name (format-buf-name cmd))
         (process (start-process-shell-command
                   cmd buf-name cmd))
         (win-size (window-size)))
    (set-process-sentinel process 'formatter-sentinel-generic)
    (if (> win-size 60)
        (message "window is large enough")
      (message "else branch, window is too small: %s" win-size))
    (setq new-window (split-window-below 60))
    (set-window-buffer new-window buf-name t)
    (other-window 1 nil t)
    (setq async-cmd-buffer buf-name)
    (set-process-query-on-exit-flag process nil)
    (local-set-key [t] 'kill-async-cmd-buffer)))

(defun kill-async-cmd-buffer ()
  (message "Async command buffer: '%s'" async-cmd-buffer)
  (interactive)
  (close-window async-cmd-buffer))

(defun close-window (buf-name)
  ""
  (let
      ((the-window
        (get-buffer-window buf-name)))
    (message "window: %s" the-window)
    (select-window the-window)
    (kill-buffer-and-window)))

;; ##############################################

(defun erl/ttest ()
  (run-cmd-async-close-success "cat"))

;; ##############################################

(defclass async-split-below-cmd () ; No superclasses
  ((name :initarg :name
         :initform ""
         :type string
         :custom string
         :documentation "The name of a command.")
   (cmd :initarg :cmd
             :initform "ls"
             :custom string
             :type string
             :documentation "The actual command to be run.")
   (uses-buffer :initarg :uses-buffer
        :initform nil
        :type boolean
        :documentation "The actual command to be run.")
   (env :initarg :env
          :initform ""
          :documentation "The environment to use."))
  "A class for creating and operating on async commands that split a small
window below and then close on success.")

(cl-defmethod call-command ((cmd async-split-below-cmd) &optional rest)
  "Call command on async-split-below-cmd."
  (message "Running command: %s"  (slot-value cmd 'name))
  (run-cmd-async-close-success (slot-value cmd 'cmd)))

(setq command
      (async-split-below-cmd
       :name "list files"
       :cmd "ls -lap"
       :uses-buffer t))
(call-command command)
