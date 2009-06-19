;; parakeet.el - Provides a library that makes it easy to interact
;; with the Twitter webservice (http://twitter.com)
;; Author: Christopher Miles <twitch@nervestaple.com>

;; This file is not a part of GNU Emacs.

(defgroup parakeet '()
  "A package that makes it easy to interact with the Twitter
  webservice (http://twitter.com). It has all the features that
  I've been looking for and very few of the ones that I don't
  use.")

;; I often feel the need to Twitter from inside the Emacs environment
;; and find it to be kind of a hassle to grab the mouse and interact
;; with a desktop Twitter client. This package provides all the
;; functionality that I've been looking for an none of the
;; functionality that I haven't.

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

(defcustom parakeet-socks-proxy "http://localhost:9080"
  "Your SOCKS4 proxy server."
  :type 'string
  :group 'parakeet)

;; Constants

(defconst parakeet-public-timeline-url
  "https://twitter.com/statuses/public_timeline.json"
  "Twitter Public Timeline URL")

(defconst parakeet-curl-args
  (list "--insecure"
	"--socks4" parakeet-socks-proxy)
  "Arguments to pass to curl when contacting the Twitter webservice.")

;; Package errors

(put 'communication-error
     'error-conditions
     '(error parakeet-errors 'communication-error))

(provide 'parakeet)

(defun parakeet-public-timeline ()
  "Returns an array of data that contains the twenty most recent
tweets on the Twitter public timeline"
  (let ((json-data nil))
    (save-excursion
      (let ((buffer-temp (libcurl parakeet-curl-args  
				  parakeet-public-timeline-url)))
	(if (libcurl-errorp buffer-temp)
	    (let ((error-message (libcurl-error-code-description buffer-temp)))
	      (signal 'communication-error (list error-message)))
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
 (let ((result nil)
       (tweet-in tweet))
   (while (car tweet-in)
     (if (string= (car (car tweet-in)) key)
	 (progn
	   (setq result (cdr (car tweet-in)))
	   (setq tweet-in nil))
       (setq tweet-in (cdr tweet-in))))
   result))
parakeet-tweet-value

;; Test code starts here!

(defun parakeet-output-tweet (buffer tweet)
  "Returns a string containing the pretty representation of the tweet."
  nil)
  
(setq buffer-out (generate-new-buffer "test-out"))

(condition-case error-in
    (setq test-data (parakeet-public-timeline))
  (error error-in))

(elt test-data 1)
((source . "<a href=\"http://orangatame.com/products/twitterberry/\">TwitterBerry</a>") (id . 2230204171.0) (in_reply_to_user_id) (in_reply_to_status_id) (truncated . :json-false) (created_at . "Thu Jun 18 23:47:05 +0000 2009") (in_reply_to_screen_name) (user (profile_sidebar_border_color . "9f24b7") (statuses_count . 437) (id . 28397184) (location . "MK, England") (verified . :json-false) (following) (screen_name . "wooozyfbaby") (profile_sidebar_fill_color . "4f1c46") (favourites_count . 0) (profile_image_url . "http://s3.amazonaws.com/twitter_production/profile_images/230347900/IMG00092-20090511-1851_normal.jpg") (profile_background_color . "212430") ...) (text . "Tell me if ur angry!!!! Rrrrrrrr lol") (favorited . :json-false))

(parakeet-tweet-value "text" (elt test-data 1))
"Tell me if ur angry!!!! Rrrrrrrr lol"

(parakeet-tweet-value "name" 
		      (parakeet-tweet-value "user" (elt test-data 1)))
"Wuraola Fanimokun"

