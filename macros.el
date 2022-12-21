;;; macros.el --- erlang macros handling -*- lexical-binding:t -*-
;;;
;;; handling macros .. expand, evaluate, etc..

(require 'cl)
(require 'dash)

(defun make-sense-of-thing-at-point ()
  "docstring"
  (interactive)
  (-let* ((symbols (bounds-of-thing-at-point 'symbol))
          (thepoint (point))
          ((start . end)
           (or symbols
               `(,thepoint . ,thepoint))))
    (if (= start end)
        (when (equal "?" (char-to-string (char-after)))
          (expand-macro-full start end))
      (progn
        (when start (goto-char start))
        (message "START: %s, END: %s" start end)
        (let ((true-start-pos
               (ignore-errors (search-backward "?" (- start 1)))))
          (message "true-start-pos: %s" true-start-pos)
          (when true-start-pos (expand-macro-full start end))))))
  (message "End of function.."))

(defun expand-macro-full (start end)
  "Expand macro"
  (message "Inside 'expand-macro-full' function.. ")
  ;;; right now I am playing with doign this with overlays.. but
  ;;; honestly this may not be the way to go.
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face 'match)))
