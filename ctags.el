;;; ctags.el --- erlang tools and whatnot -*- lexical-binding:t -*-
;;;
;;; Erlang tools and functions. Specifically formatting, running tests,
;;; expanding macros and connecting with a remote shell and more.

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(setq inputFile "~/dev/workspace/master2/ns_server/TAGSS")

;; other vars
(setq splitPos 1)  ;; cursor position of split, for each line
(setq fName "")
(setq restLine "")
(setq moreLines t)

(defun parse-ctags-file (filename)
  (find-file filename)
  (goto-char (point-min))
  (forward-line 1)
  (while moreLines
    (let* ((line (thing-at-point 'line t))
           (initial-point (point-min))
           (search-results (search-forward-regexp "^src/[a-z_]*.erl,[0-9]*$" nil t nil)))
      (goto-char
       (if search-results search-results
         initial-point))
      (message "LINE: %s:%s" line search-results))
    (setq moreLines (= 0 (forward-line 1)))))

(parse-ctags-file "~/dev/workspace/master2/ns_server/TAGSS")

(let* ((data (get-string-from-file "~/dev/workspace/master2/ns_server/TAGSS")))
  (message "%s" data))

(provide ctags)

;; ""
