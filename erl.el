;;; erl.el --- erlang tools and whatnot -*- lexical-binding:t -*-
;;;
;;; Erlang tools and functions. Specifically formatting, running tests,
;;; expanding macros and connecting with a remote shell and more.

(require 'async-cmd)

;;; honestly this test command works just as well as the custom one.. custom one
;;; needs more work but is fully configurable. ###autoload
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

(defun erl/ttest ()
  (let ((command
         (async-split-below-cmd :name "list files"
                                :cmd (format "cd ../%s && T_WILDCARD=%s make test"
                                             (project-root-pls)
                                             (spacemacs/copy-file-name-base)))))
    (call-command command)
    (message "INSIDE: %s" command))
  (message "OUTSIDE"))

(defun list->string (list)
      "Convert LIST to string."
      (let* ((string-with-parenthesis (format "%S" list))
             (end (- (length string-with-parenthesis) 2)))
        (substring string-with-parenthesis 2 end)))

;;; the projectile root dir
(defun project-root-pls ()
  (require 'projectile)
  (if projectile-project-root
      (projectile-project-root) "."))

;;; currently unimplemented
(defun expand-macro ()
  (interactive)
  (print "expand macro"))

; remote shell stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'http)
(require 'json)
(require 'dash)
(require 'cb-complete)

(defun erl/remote-shell-select ()
  "connect to a remote node using erlang shell"
  (let* ((passwd
          (read-passwd "Password for user 'Administrator'?: " nil "asdasd"))
         (completion-extra-properties
          '(:annotation-function fancy-node-info-annotation)))
    (connect-erlang-remote-shell
     (completing-read "Select which node to connect to: "
                      (get-otp-nodes passwd))
     passwd)))

(defun get-erlang-cookie-local (&rest pass)
  "get the erlang cookie for the local node"
  (http/post-sync
   "http://127.0.0.1:9000/diag/eval"
   "erlang:get_cookie()."
   pass))

(defun pools/default (host pass)
  "Get the pools/default data and parse it from json -> plist."
  (json-parse-string
   (http/get-sync
    (format "http://%s/pools/default" host) pass)
   :object-type 'plist))

(defun get-otp-node-name-local (&rest pass)
  "get local node's name"
  (let* ((jsondata (pools/default "127.0.0.1:9000" pass))
         (otpnode (plist-get
                   (aref (plist-get jsondata :nodes) 0) :otpNode)))
    (print otpnode)
    otpnode))

(defun get-otp-nodes (&rest pass)
  "get all the otp nodes running locally by checking pools/default"
  (let* ((jsondata (pools/default "127.0.0.1:9000" pass)))
    (--map (let*
               ((otpnode (plist-get it :otpNode))
                (svcs (plist-get it :services))
                (server-group (plist-get it :serverGroup))
                (encryption-enabled (plist-get it :nodeEncryption))
                (version (plist-get it :version))
                (display (format "services: %s version: %s group: %s encryption: %s"
                                 svcs version server-group encryption-enabled)))
             `(,otpnode ,display))
           (plist-get jsondata :nodes))))

(defun connect-erlang-remote-shell (&optional node pass)
  "connect to a remote shell"
  (interactive)
  (let* ((node-name
          (format "emacs-remote-shell-%s" (random 10000)))
         (erlang-cookie
          (get-erlang-cookie-local pass)))
    (inferior-erlang)
    (inferior-erlang-display-buffer)
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
                       (get-otp-node-name-local pass))))
      (inferior-erlang-send-command (format "r '%s'" nodename)))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "c 2")
    (inferior-erlang-wait-prompt)))

(provide 'erl)

;;; erl.el ends here...
