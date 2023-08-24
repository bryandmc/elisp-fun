;;; http.el --- http stuff -*- lexical-binding:t -*-
;;;
;;; TODO: switch over to aio?

(require 'dash)

(defconst HTTP-METHODS '(:GET :POST :PUT :DELETE)
  "The possible symbols that represent HTTP methods.")

(defun http-method->string (method)
  (pcase method
    (`:GET "GET")
    (`:POST "POST")
    (`:PUT "PUT")
    (`:DELETE "DELETE")))

(defun b64-basic-auth-password (password)
  "Get the basic auth header for the password provided."
  (concat "Basic "
          (base64-encode-string
           (concat "Administrator" ":" password))))

(defun basic-auth-headers (base64)
  `(("Accept" . "*/*")
    ("Authorization" . ,base64)))

(defun form-headers (base64)
  `(("Content-Type" . "application/x-www-form-urlencoded")
    ("Accept" . "*/*")
    ("Authorization" . ,base64)))

(defmacro http-get-boilerplate-callback (url credentials body after)
  "Macro containing the majority of the logic for getting strings from http requests."
  (let* ((url-request-method (http-method->string :GET)))
    `(progn
       (message "INSIDE PROGN")
       (switch-to-buffer
        (aio-result (if credentials
                        (let* ((base64 (b64-basic-auth-password (password credentials)))
                               (url-request-extra-headers (basic-auth-headers base64)))
                          (message "INNER LET")
                          (body url)) ;; synchronous / asynchronous (using macro)
                      (funcall body url)))) ;; synchronous / asynchronous (using macro)
       (message "AFTER LET")
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (funcall after)
       (current-buffer))))

(defmacro http-get-boilerplate (url credentials &rest body)
  "Macro containing the majority of the logic for getting strings from http requests."
  `(progn
     (if credentials
         (let* ((url-request-method (http-method->string :GET))
                (base64 (b64-basic-auth-password (password credentials)))
                (url-request-extra-headers (basic-auth-headers base64)))
           (switch-to-buffer
            (,@body url))) ;; synchronous / asynchronous (using macro)
       (let* ((url-request-method (http-method->string :GET)))
         (switch-to-buffer
          (,@body url))))
     (goto-char (point-min))
     (re-search-forward "\n\n")
     (current-buffer)))

(defun http/get-sync (url password)
  (http-get-boilerplate url password
                        (lambda (url)
                          (url-retrieve-synchronously url))))

(aio-defun http/get-async (url credentials)
  (http-get-boilerplate url credentials
                        (lambda (url)
                          (cdr (aio-await (aio-url-retrieve url))))))

(defmacro http-post-boilerplate (url eval credentials &rest body)
  "Macro containing boilerplate for http requests.. sync and async"
  `(progn
     (let* ((response-string nil)
            (url-request-method (http-method->string :POST))
            (base64 (b64-basic-auth-password (password credentials)))
            (url-request-extra-headers (form-headers base64))
            (url-request-data eval))
       (print url-request-data)
       (switch-to-buffer
        (,@body url)) ;; synchronous / asynchronous (using macro)
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (current-buffer))))

(defun http/post-sync (url eval credentials)
  (http-post-boilerplate url eval credentials
                         (lambda (url)
                           (url-retrieve-synchronously url))))

(aio-defun http/post-async (url eval credentials)
  (http-post-boilerplate url eval credentials
                         (lambda (url)
                           (cdr (aio-await (aio-url-retrieve url))))))

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

(provide 'http)
