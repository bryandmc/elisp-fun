;; One of these two should be used?
;; (require 'async-cmd)
;; (autoload 'async-split-below-cmd "async-cmd")
(require 'async-split-below-cmd "async-cmd")
(require 'projectile)

(defun erl/expand-macro ()
  (interactive)
  (print "expand macro"))

;;;###autoload
(defun project-root-pls ()
  (if projectile-project-root
      (projectile-project-root) "."))

;;;###autoload
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

;;;###autoload
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

(require 'comint)

(defun erl+/cbshelll ()
  "docstring"
  (interactive)
  (inferior-erlang)
  (inferior-erlang-wait-prompt)
  (inferior-erlang-send-command
   "net_kernel:start(['dilberrt123456@127.0.0.1', longnames]),
      erlang:set_cookie('518c82f4762801e9fbbfbe2969f9215357eabc5ebea7ebfc1d86e09c24c26f07').")
  (comint-send-input (kbd "C-q C-g")))
(erl+/cbshelll)

(defun erl/cbshell ()
  (interactive)
  (let
      ((the-cmd
        (format "cd %s/.. && cbshell n_0 9000" (project-root-pls))))
    (shell-command the-cmd)
    (inferior-erlang
     ;; "ls"
     ;; "net_kernel:start(['dilberrt@127.0.0.1', longnames]),
     ;;  erlang:set_cookie('518c82f4762801e9fbbfbe2969f9215357eabc5ebea7ebfc1d86e09c24c26f07')."
     )
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command
     "net_kernel:start(['dilberrt123@127.0.0.1', longnames]),
      erlang:set_cookie('518c82f4762801e9fbbfbe2969f9215357eabc5ebea7ebfc1d86e09c24c26f07').")
    (commint-send-input (kbd "C-c C-g"))

    (message the-cmd))
  (print "done."))


;;;###autoload
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

(defun convert-list-to-string (list)
      "Convert LIST to string."
      (let* ((string-with-parenthesis (format "%S" list))
             (end (- (length string-with-parenthesis) 2)))
        (substring string-with-parenthesis 2 end)))


;;;###autoload
(defun walk-dir> (root &optional extension)
  (os-walk root extension))

;;;###autoload
(defun os-walk (root &optional extension)
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
          (message "CURRENT: %s" )
          (setq files (append files (os-walk fn)))))
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

(require 'http)
(require 'json)

(defun get-erlang-cookie-local ()
  "get the erlang cookie for the local node"
  (ignore-errors
    (http/post-sync
     "http://127.0.0.1:9000/diag/eval"
     "erlang:get_cookie().")))

(defun get-otp-node-name-local ()
  "get local node's name"
  (let* ((jsondata (json-parse-string
                    (ignore-errors
                      (http/get-sync
                       "http://127.0.0.1:9000/pools/default"))
                    :object-type 'plist))
         (otpnode (plist-get
                   (aref
                    (plist-get jsondata :nodes) 0)
                   :otpNode)))
    (print otpnode)
    otpnode))

(defun get-otp-nodes ()
  "get all the otp nodes running locally by checking pools/default"
  (let* ((jsondata (json-parse-string
                    (ignore-errors
                      (http/get-sync
                       "http://127.0.0.1:9000/pools/default"))
                    :object-type 'plist)))
    (mapcar
     (lambda (item)
       (message "ITEM: %s" item)
       (let* ((otpnode
               (intern (plist-get item :otpNode))))
         otpnode))
     (plist-get jsondata :nodes))))

(defun connect-erlang-remote-shell (&optional node)
  "connect to a remote shell"
  (interactive)
  (inferior-erlang)
  (let* ((node-name
          (format "emacs-remote-shell-%s" (random 10000)))
         (erlang-cookie
          (get-erlang-cookie-local)))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command
     (format "net_kernel:start(['%s', longnames])." node-name))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command
     (format "erlang:set_cookie('%s')." erlang-cookie))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "")
    (inferior-erlang-wait-prompt)
    (let* ((nodename (if node
                         node
                       (get-otp-node-name-local))))
      (inferior-erlang-send-command (format "r '%s'" nodename)))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "c 2")
    (inferior-erlang-wait-prompt)))

;;;###autoload
(defun erl-remote-shell-select ()
  "connect to a remote node using erlang shell"
  (interactive)
  (connect-erlang-remote-shell
   (completing-read "Select which node to connect to: "
                    (get-otp-nodes))))


;; (spacemacs|define-transient-state transient-state-fake
;;   :title "VCS Transient State"
;;   :doc "
;;  Hunk Commands^^^^^^
;; ----------------------------^^^^^^
;;  [_t_]^^^^      Run tests for current file
;;  [_T_]^^^^      Run tests for whole project
;;  [_\\?_]^^^^    More commands? "
;;   :on-enter (print "entered transient state")
;;   :bindings
;;   ("t" (lambda () (interactive) (print "HIT LETTER 't'")))
;;   ("T" print-thingy :exit t)
;;   ("q" nil :exit t))

;; (spacemacs|define-micro-state micro-state-name
;;   :doc "Some documentation stuff..?"
;;   :use-minibuffer t
;;   :evil-leader "X"
;;   :bindings ("A" symbol1 :doc "documentation for symbol1"
;;                      :pre (message "PRE")
;;                      :post (message "POST")
;;                      :exit (message "EXIT")))
