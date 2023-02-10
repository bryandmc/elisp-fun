;;; task.el --- http stuff -*- lexical-binding:t -*-
;;;

(require 'tq)

;;; :created :running :success :error
(defconst STATUS [:CREATED :RUNNING :SUCCESS :ERROR]
  "The available options for the task status")

(defvar-local task/status :CREATED
  "The current task's status")

(defvar tasks (make-hash-table) "All the tasks, ordered by their command")

(defun task/filter-output (args args2)
  "docstring"
  (message "FILTERED TASK OUTPUT: %s, args2: %s" args args2)
  args)

(defun task/sentinel (process event)
  (message "%S" process)
  (message "%S" event)
  (when (string-match-p "finished" event)
    (message "successfully ran command on buffer.. closing")))

(defun task/start (task)
  "Runs command on current file"
  (let* ((cmd (slot-value task 'cmd))
         (cmd-sym (make-symbol cmd))
         (buf-name (slot-value task 'buf-name))
         (timer (timer-create))
         (process (start-process-shell-command cmd buf-name cmd))
         (new-buf (get-buffer-create buf-name))
         (win (display-buffer-at-bottom new-buf
                                        `(mode . `(special-mode))))
         (tq (tq-create process)))
    (puthash cmd-sym tq tasks)
    (tq-enqueue tq cmd nil '()
                (lambda (closure answer)
                  (message "Answer: %s, closure: %s" answer closure))
                t)
    ;; (tq-filter tq "^.*ls.*$")
    ;; (message "After tq-queue-pop .. result: %s" (tq-queue-pop tq))
    (set-process-sentinel process 'task/sentinel)
    (set-process-filter process 'task/filter-output)
    (setq-local task/status :running)
    (set-window-buffer win new-buf t)
    (set-process-query-on-exit-flag process t)
    (local-set-key [t] 'task/close-window)
    (message "THE TIMER: %s -> %s" timer-list timer)
    (message "Hash table: %s" tasks)))

(defun task/kill-window ()
  (interactive)
  (task/close-window))

(defun task/close-window (buf-name)
  (let
      ((the-window
        (get-buffer-window buf-name)))
    (message "window: %s" the-window)
    (select-window the-window)
    (kill-buffer-and-window)))

(defclass task () ; No superclasses
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
   (buf-name :initarg :buf-name
             :initform (format "async-task-%s" (random 100000))
             :type string
             :custom string
             :documentation "The buffers name.")
   (status :initarg :status
           :initform :created
           :type symbol
           :custom symbol
           :documentation "The tasks' status.")
   (callbacks :initarg :callbacks
              :initform '()
              :type list
              :custom list
              :documentation "Callbacks that will be called: (callbackC (callbackB (callbackA result)))"))

  "A class for creating and operating on async commands that split a small
window below and then close on success.")

(cl-defmethod task/spawn ((cmd task) &optional rest)
  "Call command on async-split-below-cmd."
  (message "Running command: %s whole: %s"  (slot-value cmd 'name) cmd)
  (task/start cmd))

(provide 'task)

(task/spawn (task :name "task1" :cmd "ls -lah"))
