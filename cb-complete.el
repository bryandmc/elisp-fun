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

(defun erlshell-out-fun (args)
  "docstring"
  (message "GOT OUTPUT ARGS: %s" args)
  args)

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
  (add-hook 'comint-redirect-filter-functions 'erlshell-filter-fun nil 'local)
  (add-hook 'comint-preoutput-filter-functions 'erlshell-out-fun nil 'local)
  ;; (setq comint-input-sender 'erlshell-sender-fun)
  ;; (setq comint-process-echoes t)
  (add-hook 'completion-at-point-functions 'erlshell-completion-at-point nil 'local))

(provide 'erlshell-mode)

(defun cb-complete/fancy-annotate (s)
  (message "Minibuffer completion table: %s" minibuffer-completion-table)
  (let ((item (assoc s minibuffer-completion-table)))
    (when item (concat " - " (second item)))))

(provide 'cb-complete)
