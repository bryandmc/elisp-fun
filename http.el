(defun http/get-sync (url)
    "Go to URL as a GET request."
    (let ((response-string-get nil)
          (url-request-method "GET")
          (url-request-extra-headers `(("Accept" . "*/*"))))
      (switch-to-buffer
       (url-retrieve-synchronously url)) ;; synchronous
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (setq response-string-get
            (buffer-substring-no-properties (point) (point-max)))
      (print response-string-get)
      (kill-buffer (current-buffer))
      response-string-get))

  (defun http/post-sync (url eval-string)
    "Send EVAL-STRING to URL as a POST request."
    (let (
          (response-string nil)
          (url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data eval-string))
      (switch-to-buffer
       (url-retrieve-synchronously url)) ;; synchronous
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (setq response-string
            (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer))
      response-string))

(provide 'http)
