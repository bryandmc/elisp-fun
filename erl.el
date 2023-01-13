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

(aio-defun async/remsh-select (&optional host)
  "connect to a remote node using erlang shell"
  (let* ((passwd
          (read-passwd "Password for user 'Administrator'?: " nil "asdasd"))
         (host (or host "127.0.0.1:9000"))
         (completion-extra-properties
          '(:annotation-function cb-complete/fancy-annotate)))
    (aio-await (async/connect-remsh
                (completing-read "Select which node to connect to: "
                                 (aio-await (async/otp-nodes host passwd)))
                passwd))))

(defun erl/remote-shell-select (&optional host)
  "connect to a remote node using erlang shell"
  (let* ((passwd
          (read-passwd "Password for user 'Administrator'?: " nil "asdasd"))
         (host (or host "127.0.0.1:9000"))
         (completion-extra-properties
          '(:annotation-function cb-complete/fancy-annotate)))
    (connect-erlang-remote-shell
     (completing-read "Select which node to connect to: "
                      (get-otp-nodes host passwd))
     passwd)))

(defun get-erlang-cookie-local (pass)
  "get the erlang cookie for the local node"
  (http/post-sync
   "http://127.0.0.1:9000/diag/eval"
   "erlang:get_cookie()."
   pass))

(aio-defun async/erlang-cookie (pass)
  "get the erlang cookie for the local node"
  (message "Getting erlang OTP cookie")
  (aio-await (http/post-async
             "http://127.0.0.1:9000/diag/eval"
             "erlang:get_cookie()."
             pass)))

(defun pools/default (host pass)
  "Get the pools/default data and parse it from json -> plist."
  (message "Going to use password: %s" pass)
  (json-parse-string
   (http/get-sync
    (format "http://%s/pools/default" host) pass)
   :object-type 'plist))

(aio-defun async/pools/default (host pass)
  "Get the pools/default data and parse it from json -> plist."
  (message "Going to use password: %s" pass)
  (json-parse-string
   (aio-await (http/get-async
               (format "http://%s/pools/default" host) pass))
   :object-type 'plist))

(defun get-otp-node-name-local (&rest pass)
  "get local node's name"
  (let* ((pass (or pass "asdasd"))
         (jsondata (pools/default "127.0.0.1:9000" pass))
         (otpnode (plist-get
                   (aref (plist-get jsondata :nodes) 0) :otpNode)))
    (print otpnode)
    otpnode))

(aio-defun async/otp-node-name (pass)
  "get local node's name"
  (message "Getting otp node names..")
  (let* ((pass (or pass "asdasd"))
         (jsondata (aio-await
                    (async/pools/default "127.0.0.1:9000" pass)))
         (otpnode (plist-get
                   (aref (plist-get jsondata :nodes) 0) :otpNode)))
    (print otpnode)
    otpnode))

;; (aio-wait-for (async/otp-node-name))

;; (defun erl/ns_config ()
;;   "get the ns_config, put it in a buffer and format it nicely"
;;   (interactive "P")
;;   (let* ((host ())
;;          (password ()))
;;     (http/post-sync (message "http://%s/diag/eval" host) "ns_config:get()." password)
;;     ))


(defun get-otp-nodes (host pass)
  "get all the otp nodes running locally by checking pools/default"
  (message "What is input?: %s:%s" host pass)
  (let* ((jsondata (pools/default host pass)))
    ;; (message "JSONDATA: %s" jsondata)
    (--map (let*
               ((otpnode (plist-get it :otpNode))
                (svcs (plist-get it :services))
                (server-group (plist-get it :serverGroup))
                (encryption-enabled (plist-get it :nodeEncryption))
                (version (plist-get it :version))
                (status (if (equal "healthy"
                                   (plist-get it :status))
                            "✅ (good)"
                          "❌ (bad)"))
                (display (format "services: %s | version: %s | group: %s | encryption: %s | health status: %s"
                                 svcs version server-group encryption-enabled status)))
             `(,otpnode ,display))
           (plist-get jsondata :nodes))))

(aio-defun async/otp-nodes (host pass)
  "get all the otp nodes running locally by checking pools/default"
  (message "What is input?: %s:%s" host pass)
  (let* ((jsondata
          (aio-await (async/pools/default host pass))))
    (print jsondata)
    (--map (let*
               ((otpnode (plist-get it :otpNode))
                (svcs (plist-get it :services))
                (server-group (plist-get it :serverGroup))
                (encryption-enabled (plist-get it :nodeEncryption))
                (version (plist-get it :version))
                (status (if (equal "healthy"
                                   (plist-get it :status))
                            "✅ (good)"
                          "❌ (bad)"))
                (display (format "services: %s | version: %s | group: %s | encryption: %s | health status: %s"
                                 svcs version server-group encryption-enabled status)))
             `(,otpnode ,display))
           (plist-get jsondata :nodes))))

;; (aio-wait-for (async/otp-nodes "127.0.0.1:9000" "asdasd"))

;; (defun format-remote-conn-string (remote-name node-name host  cookie)
;;   "format the remote connection string"
;;   (let ((formatted-string (format "/usr/local/bin/fish -c 'erl -name %s@%s -remsh %s@%s -setcookie %s -hidden'" remote-name host node-name host cookie)))
;;     (message "Here is formatted string: %S" formatted-string)
;;     (term formatted-string)))

;; (switch-to-buffer (format-remote-conn-string "n_666" "n_0" "127.0.0.1" "fc6fc910f7896a7f90d99fb63fa0acfbc7131f77801055ffdae64aa96e35243e"))

(defconst ns_server_modules '("active_cache" "alarm_handler" "ale")
  "The actual modules we currently have..")

(defun connect-erlang-remote-shell (node pass)
  "connect to a remote shell"
  (let* ((pass
          (or pass "asdasd"))
         (remsh-nodename
          (format "emacs-remote-shell-%s" (random 10000)))
         (target-nodename
          (or node (get-otp-node-name-local pass)))
         (erlang-cookie
          (get-erlang-cookie-local pass)))
    (inferior-erlang)
    (inferior-erlang-display-buffer)
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command
     (format "net_kernel:start(['%s', longnames])." remsh-nodename))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command
     (format "erlang:set_cookie(%s)." erlang-cookie))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "")
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command (format "r '%s'" target-nodename))
    (inferior-erlang-wait-prompt)
    (inferior-erlang-send-command "c 2")
    (inferior-erlang-wait-prompt)
    ;;; This next command will output a bunch of module information that we
    ;;; are going to steal to create a completion table.
    ;; (inferior-erlang-send-command "m().")

    ;; TODO: for completion, we can potentially scrape data from the m(module). command.
    ;; Also the m(). by itself lists all modules loaded, and where we loaded them from.
    (let* ((modules (--map (message "m(%s)." it) the_modules)))
      (message "GOT THIS LIST: %s" modules)
      )
    (inferior-erlang-send-command "m(active_cache).")))

(require 'aio)

(aio-defun async/connect-remsh (node pass)
  "connect to a remote shell.. asynchronously (sorta)"
  (let* ((pass
          (or pass "asdasd"))
         (remsh-nodename
          (format "emacs-remote-shell-%s" (random 10000)))
         (target-nodename
          (async/otp-node-name pass))
         (erlang-cookie
          (async/erlang-cookie pass)))

    ;;; Wait for promises to resolve .. In theory, these two HTTP request
    ;;; should happen concurrently. That is why we create the inert promises
    ;;; above and THEN await them BOTH here instead of individually, in serial.
    (aio-wait-for
     (aio-all
      (list target-nodename erlang-cookie)))
    (message "Aio wait over..")

    ;;; bind some variables that are used later when connecting to remsh
    (let* ((target-nodename
            (funcall (aio-result target-nodename))) ;;; the results are wrapped in functions
           (erlang-cookie
            (funcall (aio-result erlang-cookie)))) ;;; the results are wrapped in functions
      (message "OUTPUT VALUES: %s, %s, %s" output-val target-nodename erlang-cookie)
      (inferior-erlang)
      (inferior-erlang-display-buffer)
      (inferior-erlang-wait-prompt)
      (inferior-erlang-send-command
       (format "net_kernel:start(['%s', longnames])." remsh-nodename))
      (inferior-erlang-wait-prompt)
      (inferior-erlang-send-command
       (format "erlang:set_cookie(%s)." erlang-cookie))
      (inferior-erlang-wait-prompt)
      (inferior-erlang-send-command "")
      (inferior-erlang-wait-prompt)
      (inferior-erlang-send-command (format "r '%s'" target-nodename))
      (inferior-erlang-wait-prompt)
      (inferior-erlang-send-command "c 2")
      (inferior-erlang-wait-prompt)
      (inferior-erlang-send-command "m(active_cache)."))))

(provide 'erl)

;; (require 'tablist)
;; (require 'dash)
;; (require 'aio) ;; async
;; (require 's) ;; long lost string manipulation lib

;; (require 'generator)

;; (aio-defun fetch-fortune-3 (url)
;;   (print "inside fetch-fortune-3")
;;   (let ((buffer
;;          (cdr (aio-await (aio-url-retrieve url 'nil 'nil)))))
;;     (with-current-buffer buffer
;;       (print buffer)
;;       (message "HTML: %s" (buffer-string))
;;       (kill-buffer))))

;; (print (aio-wait-for (fetch-fortune-3 "http://www.google.com")))
;; (message "AIO-ALL: %s" (aio-all `(,(fetch-fortune-3 "http://www.google.com") . ,(fetch-fortune-3 "http://www.yahoo.com"))))

;; (aio-wait-for (http/get-async "http://www.google.com" "asdasd"))

;; ;;; erl.el ends here...

;; (require 'tabulated-list)

;; (define-derived-mode tbl-mode tabulated-list-mode "FANCY TABLE"
;;   (setq-local tabulated-list-format 'nil)
;;   (setq somevalue "someValue")
;;   (setq somevalue2 "someValue2")
;;   (setq-local tabulated-list-entries (list `(,somevalue ["description 1"])))
;;   (tabulated-list-print))

;; (define-derived-mode list-demo-mode tabulated-list-mode "list-demo-mode"
;;   "Major mode for tabulated list example."
;;   (setq tabulated-list-format [("Column_1" 10 t)
;;                                ("Column_2" 10 nil)
;;                                ("Column_3"  10 t)
;;                                ("Column_4" 0 nil)]);; last columnt takes what left
;;   (setq-local tabulated-list-entries (list
;;                                 (list "1" ["50" "A2" "A3" "A4"])
;;                                 (list "2" ["10" "B2" "B3" "B4"])
;;                                 (list "3" ["15" "C2" "C3" "C4"])))
;;   (setq-local tabulated-list-padding 4)
;;   (setq-local tabulated-list-sort-key (cons "Column_1" nil))
;;   (tabulated-list-init-header)
;;   (tabulated-list-print t))
