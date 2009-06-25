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

;; This library requires "libcurl.el". This is a simple wrapper around
;; the command line version of curl. You can download it from...

;; [provide path to libcurl.el website]

;; This library requires "json.el". You can get this from...

;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs

;; This library uses (and requires) the "curl" command to interact
;; with the Twitter webservice and looks for this by using your "PATH"
;; environment variable. If you are having problems, check this first.

;; Most of the time curl isn't distributed with the root certificates
;; needed to verify SSL certificates, for this reason the "--insecure"
;; flag is passed to curl. If your installation does have the root
;; certificates available, you can override this by setting the
;; "parakeet-curl-args" variable to nil.

;; (custom-set-variables '(parakeet-defaut-curl-args nil))

;; All of the data returned will be in the form of a list.

;; Functions

;; parakeet-public-timeline-data - Returns public timeline data

;; parakeet-friend-timeline-data - Returns the user's friend timeline
;; data

;; parakeet-tweet-value (key tweet) - Returns the value with the given
;; key in the provided tweet.

;; required packages
(require 'json)
(require 'libcurl)

;; Customizable variables

(defcustom parakeet-twitter-user "username"
  "Your Twitter username."
  :type 'string
  :group 'parakeet)

(defcustom parakeet-twitter-password "password"
  "Your Twitter passeword."
  :type 'string
  :group 'parakeet)

(defcustom parakeet-socks-proxy nil
  "Your SOCKS4 proxy server."
  :type 'string
  :group 'parakeet)

(defcustom parakeet-default-curl-args
  (list "--insecure")
  "These arguments are passed to curl upon every invocation."
  :type 'list
  :group 'parakeet)

;; Constants

(defconst parakeet-public-timeline-url

  "Twitter Public Timeline URL")

(defconst parakeet-friends-timeline-url

  "Twitter Private Friend Timeline")

(defconst parakeet-urls
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public "http://twitter.com/statuses/public_timeline.json" hash)
    (puthash 'friend "https://twitter.com/statuses/friends_timeline.json" hash)
    hash)
  "A hash of the URL's used to communicate with the Twitter web service.")

(defconst parakeet-curl-args
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public 'parakeet-public-curl-args hash)
    (puthash 'friend 'parakeet-private-curl-args hash)
    hash)
  "A hash of the arguments that need to be passed to curl when
  communicating with the Twitter web service.")

;; Package errors

;; this is our general "I can't get Twitter on the line" error
(put 'communication-error
     'error-conditions
     '(error parakeet-errors 'communication-error))

(defun parakeet-public-curl-args ()
  "Returns the arguments to use when invoking curl to load a
public Twitter endpoint."
  (append
   parakeet-default-curl-args
   (if parakeet-socks-proxy
       (list "--socks4" parakeet-socks-proxy))))

(defun parakeet-private-curl-args ()
  "Returns the arguments to use when invoking curl to load
private Twitter endpoint that requires authentication."
  (append
   parakeet-default-curl-args
   (list "-u" (concat parakeet-twitter-user ":" parakeet-twitter-password))
   (if parakeet-socks-proxy
       (list "--socks4" parakeet-socks-proxy))))

(defun parakeet-timeline-data (timeline-type)
  "Returns an array of data that contains the most recent tweets
for the provided timeline type."

  ;; setup a variable for the data
  (let ((json-data nil))
    (save-excursion

      ;; pass our arguments to curl and grab the returned buffer
      (let ((buffer-temp (libcurl
                          (funcall (gethash timeline-type parakeet-curl-args))
                  (gethash timeline-type parakeet-urls))))

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

(defun parakeet-public-timeline-data ()
  "Returns an array of data that contains the twenty most recent
tweets on the Twitter public timeline"
  (parakeet-timeline-data 'public))

(defun parakeet-friend-timeline-data ()
  "Returns an array of data that contains the twenty most recent
tweets from the user's private Twitter friend timeline."
  (parakeet-timeline-data 'friend))

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