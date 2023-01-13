;;; async-cmd.el --- tools for running async shell commands with window split -*- lexical-binding:t -*-
;;;
;;; EXAMPLE USAGE:
;;; (setq command
;;;       (async-split-below-cmd :name "list files"
;;;                              :cmd "ls -lap"
;;;                              :uses-buffer t))
;;; (call-command command)

(defvar-local async/buffer-local-test-status :never-ran
  "The status of the current buffer, and whether or not we have succeeded
(or attempted at all) to run the tests on the local file.

Valid options are: [':never-ran' ':running' ':success' ':failure'].")

(defvar-local async/cmd-buffer nil
  "The buffer the command is running in.")

(defun async/formatter-sentinel-generic (process event)
  (when (string-match-p "finished" event)
    (message "successfully ran command on buffer.. closing")
    (setq-local async/buffer-local-test-status :success)
    (message "BUFFER LOCAL TEST STATUS: %S" async/buffer-local-test-status)
    (async/close-window async-cmd-buffer)))

(defun async/format-buf-name (cmd)
    (format "*Async %s*" cmd))

;;; TODO: figure out if this is necessary..
(defun async/calculate-window-size ()
  "calculate the window size and possible dimensions for splitting"
  (let ((win-size (window-size)))
    (if (> win-size 60)
        (progn (message "window is large enough") 60)
      (progn (message "else branch, window is too small: %s" win-size) 20))))

(defun async/run-cmd-async-close-success (cmd)
  "Runs command on current file"
  (let* ((file-name
          (shell-quote-argument (buffer-file-name)))
         (the-cmd
          (slot-value cmd 'cmd))
         (buf-name
          (async/format-buf-name the-cmd))
         (process
          (start-process-shell-command the-cmd buf-name the-cmd))
         (win-size
          (async/calculate-window-size)))
    (set-process-sentinel process 'async/formatter-sentinel-generic)
    ;; (set-process-query-on-exit-flag process nil)
    (setq-local async/buffer-local-test-status :running)
    (setq new-buffer (get-buffer-create buf-name t))
    (setq-local new-window
                (display-buffer-at-bottom new-buffer
                                                 `(mode . `(special-mode))))
    (set-window-buffer new-window new-buffer t)
    ;; (other-window 1 nil t)
    (setq-local async/cmd-buffer buf-name)
    ;; (set-process-query-on-exit-flag process t)
    ;; (local-set-key [t] 'async/kill-async-cmd-buffer)
    ))

(defun async/kill-async-cmd-buffer ()
  (interactive)
  (message "Async command buffer: '%s'" async-cmd-buffer)
  (async/close-window async/cmd-buffer))

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

(provide 'async-cmd)
