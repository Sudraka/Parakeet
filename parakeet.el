;; parakeet.el - Provides a library that makes it easy to interact
;; with the Twitter webservice (http://twitter.com)
;; Author: Christopher Miles <twitch@nervestaple.com>

;; This file is not a part of GNU Emacs.

(defgroup parakeet '()
  "A package that makes it easy to interact with the Twitter
  webservice (http://twitter.com). It has all the features that
  I've been looking for and very few of the ones that I don't
  use.")

;; This package provides functions for interacting with the Twitter
;; webservice (http://twitter.com). It does not aim to be a
;; full-fledged client, rather, it provides the functions that would
;; be needed to write such a client.

;; Installation: Put this file somewhere in your load-path and then
;; add the following to your initialization file:

;; (require 'parakeet)

;; There aren't any nice end-user function yet but I'll get there
;; someday.

;; required packages
(require 'json)
(require 'libcurl)

;; Customizable variables

(defcustom parakeet-twitter-user "cmiles74"
  "Your Twitter username."
  :type 'string
  :group 'parakeet)

(defcustom parakeet-twitter-password "orange84"
  "Your Twitter passeword."
  :type 'string
  :group 'parakeet)

(defcustom parakeet-socks-proxy nil
  "Your SOCKS4 proxy server."
  :type 'string
  :group 'parakeet)

;; Constants

(defconst parakeet-public-timeline-url
  "https://twitter.com/statuses/public_timeline.json"
  "Twitter Public Timeline URL")

;; Package errors

;; this is our general "I can't get Twitter on the line" error
(put 'communication-error
     'error-conditions
     '(error parakeet-errors 'communication-error))

(defun parakeet-curl-args ()
  "Returns the arguments to use when invoking curl."
  (append
   (list "--insecure")
   (if parakeet-socks-proxy
       (list "--socks4" parakeet-socks-proxy))))

(defun parakeet-public-timeline-data ()
  "Returns an array of data that contains the twenty most recent
tweets on the Twitter public timeline"

  ;; setup a variable for the data
  (let ((json-data nil))
    (save-excursion

      ;; pass our arguments to curl and grab the returned buffer
      (let ((buffer-temp (libcurl (parakeet-curl-args)
				  parakeet-public-timeline-url)))

	;; if curl returns an error, signal an error of our own
	(if (libcurl-errorp buffer-temp)
	    (let* ((error-message (libcurl-error-code-description 
				   buffer-temp)))
	      (kill-buffer buffer-temp)
	      (signal 'communication-error (list error-message)))

	  ;; consume the data in the buffer and then kill it
	  (progn
	    (set-buffer buffer-temp)
	    (goto-char (point-min))
	    (setq json-data (json-read))
	    (kill-buffer buffer-temp)))))
    json-data))

(defun parakeet-tweet-value (key tweet)
  "Returns the value that matches the key in the given tweet or
nil if the provided tweet doesn't have a value with the given
key."

  ;; setup variables to hold the value and the tweet
  (let ((result nil)
	(tweet-in tweet))

    ;; loop through the tweets, looking for the jey
    (while (car tweet-in)

      ;; if they match, save the result and nil the tweet to stop
      ;; looping
      (if (string= (car (car tweet-in)) key)
	  (progn
	    (setq result (cdr (car tweet-in)))
	    (setq tweet-in nil))
	(setq tweet-in (cdr tweet-in))))
    result))

(provide 'parakeet)