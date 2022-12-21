(defun http/get-sync (url password)
  "Go to URL as a GET request."
  (message "Using URL: %s with password: %s" url password)
  (let* ((response-string-get nil)
         (url-request-method "GET")
         (base64 (concat "Basic "
                         (base64-encode-string
                          (concat "Administrator" ":" password))))
         (url-request-extra-headers `(("Accept" . "*/*")
                                      ("Authorization" . ,base64))))
    (message "Going to use header: %s" base64)
    (switch-to-buffer
     (url-retrieve-synchronously url)) ;; synchronous
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string-get
          (buffer-substring-no-properties (point) (point-max)))
    (print response-string-get)
    (kill-buffer (current-buffer))
    response-string-get))

(defun http/post-sync (url eval-string password)
  "Send EVAL-STRING to URL as a POST request."
  (let* ((response-string nil)
         (url-request-method "POST")
         (base64 (concat "Basic "
                         (base64-encode-string
                          (concat "Administrator" ":" password))))
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Accept" . "*/*")
            ("Authorization" . ,base64)))
         (url-request-data eval-string))
    (print url-request-data)
    (switch-to-buffer
     (url-retrieve-synchronously url)) ;; synchronous
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string
          (buffer-substring-no-properties (point) (point-max)))
    (kill-buffer (current-buffer))
    response-string))

(provide 'http)
