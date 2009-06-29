;; libcurl.el - Provides a library that makes it easy to interact with "curl"
;; Author: Christopher Miles <twitch@nervestaple.com>

;; This file is not a part of GNU Emacs

(defgroup libcurl '()
  "A small package that makes it easy to interact with the
utility 'curl'. curl is a command line tool for transferring
files with URL syntax. It supports FTP, FTPS, HTTP, HTTPS and
more.")

;; I wrote this package because I was having problems getting the
;; built-in URL support that comes with Emacs to work through the
;; SOCKS4 proxy that I use at work. This is my first stab at elisp
;; code, there's most likely many bits that could be improved.

;; Installation: Put this file somewhere in your load-path and then
;; add the following to your emacs initialization file:

;; (require 'libcurl)

;; There aren't any end-user facing functions in this package, it is
;; meant to be used by developers who need to interact with curl.

(defun libcurl-parse-data (data)
  "Parses a property list of data and returns a list that can be
handed to curl when posting data to a URL."
  (let ((post-data nil)
	(index 0))
    (while (<= index (length data))
      (let ((item (car data)))
	(setq post-data
	      (concat post-data
		      (if (not (null post-data))
			  "&")
		      (url-hexify-string (car item))
		      "="
		      (url-hexify-string (cdr item)))))
	(setq data (cdr data))
	(setq index (1+ index)))
      post-data))

(defun libcurl (args url)
  "Invokes curl with the given arguments and the provided url. A
new buffer with the result will be returned. Note that all calls
to curl are made with the '--silent' and '--show-error' flagsp,
this is so the output is predictable."
  (let ((buffer-temp (generate-new-buffer "libcurl-result")))
    (save-excursion
      (apply 'call-process 
	     (append 
	      (list "curl" nil  buffer-temp nil) 
	      (append '("--silent" "--show-error") args) 
	      (list url))))
    buffer-temp))

(defun libcurl-errorp (buffer-in)
  "Checks a buffer of curl output and returns true if the buffer
contains a curl error message."
  (if (bufferp buffer-in)
      (save-excursion
	(set-buffer buffer-in)
	(goto-char (point-min))
	(skip-chars-forward "\t\r\n\f\b ")
	(let ((start-pos (point)) word)
	  (forward-word)
	  (forward-char)
	  (setq word (buffer-substring-no-properties start-pos (point)))
	  (if (string= "curl:" word) t nil)))
    (signal 'wrong-type-argument '("The argument provided is not a buffer"))))

(defun libcurl-error-code (buffer-in)
  "Returns the error code in the buffer of curl result data."
  (if (bufferp buffer-in)
      (save-excursion
	(set-buffer buffer-in)
	(goto-char (point-min))
	(goto-char (search-forward "curl: ("))
	(let ((start-pos (point)))
	  (forward-word)
	  (buffer-substring-no-properties start-pos (point))))
    (signal 'wrong-type-argument '("The argument provided is not a buffer"))))

(defun libcurl-error-code-description (buffer-in)
  "Returns the descriptive description of the error code in the
buffer of curl result data"
  (if (bufferp buffer-in)
      (save-excursion
	(set-buffer buffer-in)
	(goto-char (point-min))
	(goto-char (re-search-forward "curl: ([0-9]*) "))
	(let ((start-pos (point)))
	  (end-of-line)
	  (buffer-substring-no-properties start-pos (point))))
    (signal 'wrong-type-argument '("The argument provided is not a buffer"))))

(provide 'libcurl)
