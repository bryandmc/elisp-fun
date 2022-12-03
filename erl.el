;; One of these two should be used?
;; (require 'async-cmd)
;; (autoload 'async-split-below-cmd "async-cmd")
(require 'async-split-below-cmd "async-cmd")

;;;###autoload
;; @doc expand erlang macro's
(defun erl/expand-macro ()
  (interactive)
  (setv something else here)
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


(defun erl/ttest ()
  (interactive)
  (let ((command
         (async-split-below-cmd :name "list files"
                                :cmd (format "cd ../%s && T_WILDCARD=%s make test"
                                             (project-root-pls)
                                             (spacemacs/copy-file-name-base)))))
    (call-command command)
    (message "INSIDE: %s" command))
  (message "OUTSIDE"))

(cl-defstruct test-struct
  ;; ‘thing'
  (field (make-hash-table :test 'equal))

  major-modes ()
  ;; Function that will be called to decide if this language client
  ;; should manage a particular buffer. The function will be passed
  ;; the file name and major mode to inform the decision. Setting
  ;; `activation-fn' will override `major-modes', if
  ;; present.

  activation-fn nil)

(test-struct-p t)

;; Example call
;; (erl/ttest)

(defun convert-list-to-string (list)
  "Convert LIST to string."
  (let* ((string-with-parenthesis (format "%S" list))
         (end (- (length string-with-parenthesis) 2)))
    (substring string-with-parenthesis 2 end)))

(defun os-walk (root)
  (let ((files '()) ;empty list to store results
        (current-list (directory-files root t)))
    ;;process current-list
    (while current-list
      (let ((fn (car current-list))) ; get next entry
        (cond
         ;; regular files
         ((file-regular-p fn)
          (add-to-list 'files fn))
         ;; directories
         ((and
           (file-directory-p fn)
           ;; ignore . and ..
           (not (string-equal ".." (substring fn -2)))
           (not (string-equal "." (substring fn -1))))
          ;; we have to recurse into this directory
          (setq files (append files (os-walk fn))))
         )
        ;; cut list down by an element
        (setq current-list (cdr current-list)))
      )
    files))

(require 'cl)

(defun full-os-walk (directory-name &rest)
  (mapcar
   (lambda (x) (princ (format " - [[%s][%s]]\n" x (file-relative-name x "."))))
   (remove-if-not
    (lambda (x) (string= (file-name-extension x) "org"))
    (sort (os-walk directory-name) (lambda (a b) (string< (convert-list-to-string a) (convert-list-to-string b)))))))

(defun convert-list-to-string (list)
      "Convert LIST to string."
      (let* ((string-with-parenthesis (format "%S" list))
             (end (- (length string-with-parenthesis) 2)))
        (substring string-with-parenthesis 2 end)))

(defun os-walk (root)
  (let ((files '()) ;empty list to store results
        (current-list (directory-files root t)))
    ;;process current-list
    (while current-list
      (let ((fn (car current-list))) ; get next entry
        (cond
         ;; regular files
         ((file-regular-p fn)
          (add-to-list 'files fn))
         ;; directories
         ((and
           (file-directory-p fn)
           ;; ignore . and ..
           (not (string-equal ".." (substring fn -2)))
           (not (string-equal "." (substring fn -1))))
          ;; we have to recurse into this directory
          (setq files (append files (os-walk fn))))
         )
        ;; cut list down by an element
        (setq current-list (cdr current-list)))
      )
    files))

(defun full-os-walk (dir-inner)
  (interactive)
  (mapcar
   (lambda (x) (princ (format " - [[%s][%s]]\n" x (file-relative-name x "."))))
   (remove-if-not
    (lambda (x) (string= (file-name-extension x) "org"))
    (sort (os-walk dir-inner) (lambda (a b) (string< (convert-list-to-string a) (convert-list-to-string b)))))))

(full-os-walk dir)

(org-element-parse-buffer)

(save-excursion
  (org-link-search "tree")
  (org-element--current-element 1 'greater-element))

(save-excursion
  (org-attach-dired-to-subtree '("."))
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (el)
      (org-element-property :title el))))


(mapcar
 (lambda (arg)
   (* arg 10))
 full-range)

(mapcar #'identity '(1 2 3)) ; => (1 2 3)
(defvar x '(a b c d))
(message "Mapped with %s: %s" 'identity (mapcar #'identity x)) ; => (a b c d)

(defun symbol1 ()
  "docstring"
  ())

(spacemacs|define-transient-state transient-state-fake
  :title "VCS Transient State"
  :doc "
 Hunk Commands^^^^^^
----------------------------^^^^^^
 [_t_]^^^^      Run tests for current file
 [_T_]^^^^      Run tests for whole project
 [_\?_]^^^^      More commands? "
  :on-enter (print "entered transient state")
  :bindings
  ("t" (lambda () (interactive) (print "HIT LETTER 't'")))
  ("T" print-thingy :exit t)
  ("q" nil :exit t))

(spacemacs|define-micro-state micro-state-name
  :doc "Some documentation stuff..?"
  :use-minibuffer t
  :evil-leader "X"
  :bindings ("A" symbol1 :doc "documentation for symbol1"
                     :pre (message "PRE")
                     :post (message "POST")
                     :exit (message "EXIT")))
