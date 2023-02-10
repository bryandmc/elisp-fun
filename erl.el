;;; erl.el --- erlang tools and whatnot -*- lexical-binding:t -*-
;;;
;;; Erlang tools and functions. Specifically formatting, running tests,
;;; expanding macros and connecting with a remote shell and more.

(require 'async-cmd)
(require 'aio)

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


; structures for credentials ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-generic)

(cl-defstruct credentials
  "The user credentials username/password pair" username password)

(defmacro username (c)
  `(credentials-username ,c))

(defmacro password (p)
  `(credentials-password ,p))

(cl-defmethod credentials/to_vec ((c credentials))
  `(vector ,(username c) ,(password c)))

(cl-defmethod credentials/to_list ((c credentials))
  `(list ,(username c) ,(password c)))

; structures for credentials ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct cookie
  "The actual cookie we retrieve from the server" value)

(defmacro cookie/unwrap (c)
  `(cookie-value ,c))

(cl-defmethod cookie/non-nil? ((c cookie))
  "Is the cookie set? or is it nil?"
  (message "COOKIE: %s" c)
  (not (equal (cookie/unwrap c) nil)))


; remote shell stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'http)
(require 'json)
(require 'dash)
(require 'cb-complete)
(require 'aio)
(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW LATEST ENTRYPOINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(aio-defun async/connect-shell (host)
  "Inner entrypoint to connect to erlang shell."
  (let* ((passwd
          (read-passwd "Password for user 'Administrator'?: " nil "asdasd"))
         (host
          (or host "127.0.0.1:9000"))
         (completion-extra-properties
          '(:annotation-function complete/fancy-annotate-nodelist))
         (credentials
          (make-credentials :username "Administrator" :password passwd))
         (config-promise
          (async/pools-data host credentials))
         (cookie-promise
          (async/erlang-cookie host credentials)))
    (aio-await (aio-all (list cookie-promise config-promise)))
    (let* ((cookie
            (funcall (aio-result cookie-promise)))
           (config
            (funcall (aio-result config-promise)))
           (nodelist
            (extract-otp-nodes config)))
      (configure-erl-shell
       config cookie credentials
       (completing-read "Select which node to connect to: " nodelist)))))

(defun extract-otp-nodes (config)
  "get all the otp nodes running locally by checking pools/default"
  (--map (let* ((otpnode
                 (plist-get it :otpNode))
                (svcs
                 (plist-get it :services))
                (server-group
                 (plist-get it :serverGroup))
                (encryption-enabled
                 (plist-get it :nodeEncryption))
                (version
                 (plist-get it :version))
                (status
                 (if (equal "healthy"
                            (plist-get it :status))
                     "✅ (good)"
                   "❌ (bad)"))
                (display
                 (format "services: %s | version: %s | group: %s | encryption: %s | health status: %s"
                         svcs version server-group encryption-enabled status)))
           `(,otpnode ,display))
         (plist-get config :nodes)))

(aio-defun async/pools-data (host credentials)
  "Loads the /pools/default data, and parses it into a plist "
  (let* ((buf (aio-await
               (http/get-async
                (format "http://%s/pools/default" host) credentials)))
         (parsed (with-current-buffer buf
                   (json-parse-buffer :object-type 'plist))))
    (kill-buffer buf)
    parsed))

(aio-defun async/erlang-cookie (host credentials)
  "get the erlang cookie for the local node"
  (let* ((cookie-buffer
          (aio-await (http/post-async
                      (format "http://%s/diag/eval" host)
                      "erlang:get_cookie()."
                      credentials))))
    (with-current-buffer cookie-buffer
      (let* ((response-string
              (buffer-substring-no-properties (point) (point-max))))
        (make-cookie :value response-string)))))

(defun configure-erl-shell (config cookie credentials node)
  "connect to a remote shell.. asynchronously (sorta)"
  (let* ((remsh-nodename
          (format "emacs-remote-shell-%s" (random 10000)))
         (target-nodename (message "%s" node)))
    (inferior-erlang)
    (inferior-erlang-display-buffer)
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command
     (format "net_kernel:start(['%s', longnames])." remsh-nodename))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command
     (format "erlang:set_cookie(%s)."
             (cookie/unwrap cookie)))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "")
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command (format "r '%s'" target-nodename))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "c 2")
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "m(active_cache).")
    (inferior-erlang-wait-prompt)))

(provide 'erl)
