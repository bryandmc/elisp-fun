;;; couchbase.el --- couchbase/erlang tools and functions -*- lexical-binding:t -*-

;;; Load the erlang stuff for sure
(require 'erl)
(require 'files)
(require 'cb-complete)
(require 'aio)
(require 'gerrit)

;;;###autoload
(defun cb/connect-erl-shell (host)
  "Connect to an Erlang remote shell."
  (interactive
   (list
    (read-string "What host?:" "127.0.0.1:9000")))
  (aio-wait-for
   (async/connect-shell host)))

;;;###autoload
(defun cb/erl-eunit-test-this ()
  "Run eunit tests for the file we are currently visiting."
  (interactive)
  (erl/ttest))

;;;###autoload
(defun cb/walk-dir (root &optional extension)
  "walk the directory and return all the files. Optionally only
return files of 'extension' file extension."
  (files/walk-dir root extension))


;;; this package provides 'couchbase'
(provide 'couchbase)
