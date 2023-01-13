;;; http.el --- http stuff -*- lexical-binding:t -*-
;;;
;;; TODO: switch over to aio?

(require 'dash)

(defconst HTTP-METHODS '(:GET :POST :PUT :DELETE)
  "The possible symbols that represent HTTP methods.")

(defun http-method-symbol-to-string (method)
  (pcase method
    (`:GET "GET")
    (`:POST "POST")
    (`:PUT "PUT")
    (`:DELETE "DELETE")))

(http-method-symbol-to-string :GET)

(defun b64-basic-auth-password (password)
  "Get the basic auth header for the password provided."
  (concat "Basic "
          (base64-encode-string
           (concat "Administrator" ":" password))))

(defun basic-auth-headers (base64)
  `(("Accept" . "*/*")
    ("Authorization" . ,base64)))

(defun create-form-headers2 (base64)
  `(,(basic-auth-headers base64)
   ("Content-Type" . "application/x-www-form-urlencoded")))

(defun form-headers (base64)
  `(("Content-Type" . "application/x-www-form-urlencoded")
    ("Accept" . "*/*")
    ("Authorization" . ,base64)))


(defmacro http-get-boilerplate (url password &rest body)
  "Macro containing the majority of the logic for getting strings from http requests."
  `(progn
     (message "Using URL: %s with password: %s" url password)
     (let* ((response-string-get nil)
            (url-request-method (http-method-symbol-to-string :GET))
            (base64 (b64-basic-auth-password password))
            (url-request-extra-headers (basic-auth-headers base64)))
       (switch-to-buffer
        (,@body url)) ;; synchronous / asynchronous (using macro)
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (setq response-string-get
             (buffer-substring-no-properties (point) (point-max)))
       (print response-string-get)
       (kill-buffer (current-buffer))
       response-string-get)))

(defun http/get-sync (url password)
  (http-get-boilerplate url password
                        (lambda (url)
                          (url-retrieve-synchronously url))))

(aio-defun http/get-async (url password)
  (http-get-boilerplate url password
                        (lambda (url)
                          (cdr (aio-await (aio-url-retrieve url))))))

(defun test-async-get (url password)
  (aio-wait-for (http/get-async url password)))

(test-async-get "http://127.0.0.1:9000" "asdasd")

(defmacro http-post-boilerplate (url eval password &rest body)
  "Macro containing boilerplate for http requests.. sync and async"
  `(progn
     (let* ((response-string nil)
            (url-request-method (http-method-symbol-to-string :POST))
            (base64 (b64-basic-auth-password password))
            (url-request-extra-headers (form-headers base64))
            (url-request-data eval))
       (print url-request-data)
       (switch-to-buffer
        (,@body url)) ;; synchronous / asynchronous (using macro)
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (setq response-string
             (buffer-substring-no-properties (point) (point-max)))
       (kill-buffer (current-buffer))
       response-string)))

(defun http/post-sync (url eval password)
  (http-post-boilerplate url eval password
                         (lambda (url)
                           (url-retrieve-synchronously url))))

(aio-defun http/post-async (url eval password)
  (http-post-boilerplate url eval password
                         (lambda (url)
                           (cdr (aio-await (aio-url-retrieve url))))))

(defun test-async-post (url eval password)
  (aio-wait-for (http/post-async url eval password)))

(test-async-post "http://127.0.0.1:9000" "ns_config:get()." "asdasd")

;; (message "AIO-BLAH: %s"
;;          (aio-with-async
;;            (aio-wait-for
;;             (aio-all
;;              (list
;;               (async/otp-node-name "asdasd")
;;               (async/erlang-cookie "asdasd"))))))

(defun fetch-bounded (url-list max-parallel callback)
  (let ((sem (aio-sem max-parallel)))
    (dolist (url url-list)
      (aio-with-async
        (aio-await (aio-sem-wait sem))
        (cl-destructuring-bind (status . buffer)
            (aio-await (aio-url-retrieve url))
          (aio-sem-post sem)
          (funcall callback
                   (with-current-buffer buffer
                     (prog1 (buffer-string)
                       (kill-buffer)))))))))

;; (fetch '("http://google.com" "http://youtube.com" "http://yahoo.com") 3 (lambda (val) (message "val: %s" val)))
;; (aio-listen (aio-await async/erlang-cookie "asdasd") (lambda (item) (message "ITEM AIO-LISTEN: %s" item)))
;; (aio-with-async (async/erlang-cookie "asdasd"))


(provide 'http)
