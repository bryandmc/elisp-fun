;;; completion.el --- completion related functions and tools -*- lexical-binding:t -*-
;;;
;;; TODO: finish/fix completion inside the inferior erlang shell to get auto-complete

;; this is your lang's keywords
(setq cb-complete-fake-keywords
      '("touch"
        "touch_start"
        "touch_end"
        "for"
        "foreach"
        "forall"
        ))

(defun erlshell-completion-at-point ()
  "This is the function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (save-excursion
    (goto-char (- (point) 1))
    (comint-completion-at-point)
    (message "INSIDE THE COMPLETION HOOOK!!! DEBUG: %S" (thing-at-point 'symbol))
    (let* (
           (bds (bounds-of-thing-at-point 'symbol))
           (line (split-string (thing-at-point 'line ) ":"))
           (namestr (format "%s" (thing-at-point 'symbol)))
           (start (car bds))
           (end (cdr bds)))
      (message "Bds: %s, start: %s -> end %s, name: %s" bds start end namestr)
      (message "Whole line: %S" line)
      (list start end cb-complete-fake-keywords . nil ))))

(defvar erl-module-dump-enabled nil "Whether or not we are dumping modules currently")

(defvar erl-module-map (make-hash-table) "Hash table with all the modules")

(defun erlshell-out-fun (args)
  "docstring"
  (message "GOT OUTPUT ARGS: %s %s" (type-of args) args)
  (when (ignore-errors (string-match "m()." args))
    (setq erl-module-dump-enabled 't) ;; this is required so we know it's running on next pass
    (message "w00000000t dumpstatus: %s" erl-module-dump-enabled))
  (when erl-module-dump-enabled
    (message "W00t the module dumping is enabled!!!!")
    (erlshell-parse-modules args))

  (message "After everything.."))

(defun erlshell-parse-modules (args)
  "Actually parse everythign.."
  (message "Full args: %s" args)
  (switch-to-buffer "erlang-module-parser")
  (special-mode)


  (when (string-match "Module.*" args)
    ;; (insert (substring args (match-end 0) -1))
    (let* ((temp1 (substring args (match-end 0) -1))

           (temp3 (split-string temp1))
           )
      (insert temp3)
      (message "LINES: %S" temp3)
      (puthash temp2 nil erl-module-map))
      ;; (message "GOT RESULT %s" temp2))
    (error "something")
    (message "STARTING!!!"))
  ;; TODO Actually parse the data coming in.. not in a buffer, yet
  ;; (setq erl-module-dump-enabled nil)
  ;; (let* ((module ())
  ;;        (path ()))
  ;;
  ;;   )
  (when (string-match "ok" args)
    (setq erl-module-dump-enabled nil)
    (message "ENDING!!!"))
  "Lulz")

(defun erlshell-sender-fun (args &rest other last)
  "docstring"
  (message "GOT SENDER ARGS: %s OTHER: %s last: %s" args other last)
  ;; (comint-send-input)
  args)

(defun erlshell-filter-fun (args)
  "docstring"
  (message "GOT INPUT ARGS: %s" args)
  args)

(define-derived-mode erlshell-mode erlang-shell-mode "erlshell"
  "Major mode for using erlang shell.."
  (add-hook 'comint-redirect-filter-functions 'erlshell-filter-fun nil nil)
  ;; (add-hook 'comint-preoutput-filter-functions 'erlshell-out-fun nil 'local)
  (add-hook 'comint-output-filter-functions 'erlshell-out-fun nil nil)
  ;; (setq comint-input-sender 'erlshell-sender-fun)
  ;; (setq comint-process-echoes t)
  (add-hook 'completion-at-point-functions 'erlshell-completion-at-point nil 'local))

(provide 'erlshell-mode)

(defun cb-complete/fancy-annotate (s)
  (message "Minibuffer completion table: %s" minibuffer-completion-table)
  (let ((item (assoc s minibuffer-completion-table)))
    (when item (concat " - " (second item)))))

(provide 'cb-complete)
