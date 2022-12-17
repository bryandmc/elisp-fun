;;; files.el --- file tools and whatnot -*- lexical-binding:t -*-

(defun files/walk-dir (root &optional extension)
  (files/os-walk root extension))

(defun files/os-walk (root &optional extension)
  (let ((files '()) ;empty list to store results
        (current-list (directory-files root t)))
    ;;process current-list
    (while current-list
      (let ((fn (car current-list))) ; get next entry
        (cond
         ;; regular files
         ((and
           (file-regular-p fn)
           (if extension
               (string-match-p (concat "\\`[^.]+\\" extension "\\'") (file-name-nondirectory fn))
             t))
          (add-to-list 'files fn))
         ;; directories
         ((and
           (file-directory-p fn)
           ;; ignore . and ..
           (not (string-equal ".." (substring fn -2)))
           (not (string-equal "." (substring fn -1)))
           (not (equal nil (string-match-p "\\.git" (file-name-directory fn)))))
          ;; we have to recurse into this directory
          (setq files (append files (files/os-walk fn)))))
        ;; cut list down by an element
        (setq current-list (cdr current-list)))
      )
    files))

(provide 'files)
