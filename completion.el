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
  (message "INSIDE THE COMPLETION HOOOK!!! DEBUG: %S" (thing-at-point 'symbol))
  (let* (
         (bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end cb-complete-fake-keywords . nil )))

(define-derived-mode erlshell-mode erlang-shell-mode "erlshell"
  "Major mode for using erlang shell.."
  (add-hook 'completion-at-point-functions 'erlshell-completion-at-point nil 'local))

(provide 'erlshell-mode)

(defun cb-complete/fancy-annotate (s)
  (princ minibuffer-completion-table)
  (let ((item (assoc s minibuffer-completion-table)))
    (when item (concat "  -- " (second item)))))

(provide 'cb-complete)
