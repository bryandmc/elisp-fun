(defun async/formatter-sentinel-generic (process event)
  (print process)
  (print event)
  (message event)
  (when (string-match-p "finished" event)
    (message "successfully ran command on buffer.. closing")
    (async/close-window async-cmd-buffer)))

(defun async/format-buf-name (cmd)
    (format "*Async %s*" cmd))

(defun async/run-cmd-async-close-success (cmd)
  "Runs command on current file"
  (interactive)
  (let* ((file-name (shell-quote-argument (buffer-file-name)))
         (the-cmd (slot-value cmd 'cmd))
         (buf-name (async/format-buf-name the-cmd))
         (process (start-process-shell-command
                   the-cmd buf-name the-cmd))
         (win-size (window-size)))
    (set-process-sentinel process 'async/formatter-sentinel-generic)
    (if (> win-size 60)
        (progn
          (message "window is large enough")
          (setq set-size-win 60))
      (progn
        (message "else branch, window is too small: %s" win-size)
        (setq set-size-win 20)))
    ;; (if use-root
    ;;     (progn (message "USE ROOT TRUE"))
    ;;   (message "USE ROOT FALSE/NIL"))
    (setq new-window (split-window-below set-size-win))
    (set-window-buffer new-window buf-name t)
    (other-window 1 nil t)
    (setq async-cmd-buffer buf-name)
    (set-process-query-on-exit-flag process nil)
    (local-set-key [t] 'async/kill-async-cmd-buffer)))

(defun async/kill-async-cmd-buffer ()
  (message "Async command buffer: '%s'" async-cmd-buffer)
  (interactive)
  (async/close-window async-cmd-buffer))

(defun async/close-window (buf-name)
  ""
  (let
      ((the-window
        (get-buffer-window buf-name)))
    (message "window: %s" the-window)
    (select-window the-window)
    (kill-buffer-and-window)))

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
             :documentation "The actual command to be run."))
  "A class for creating and operating on async commands that split a small
window below and then close on success.")

(cl-defmethod call-command ((cmd async-split-below-cmd) &optional rest)
  "Call command on async-split-below-cmd."
  (message "Running command: %s"  (slot-value cmd 'name))
  (async/run-cmd-async-close-success cmd))

(provide 'async-split-below-cmd)

;; EXAMPLE USAGE:
;; (setq command
;;       (async-split-below-cmd :name "list files"
;;                              :cmd "ls -lap"
;;                              :uses-buffer t))
;; (call-command command)
