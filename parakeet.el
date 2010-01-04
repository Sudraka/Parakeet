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

;; The functions that need to authenticate to Twitter are expeciting a
;; list with the username and password. This list should be in the
;; format (username password).

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

(defconst parakeet-source-info
  (list (list "source" "Parakeet"))
  "A list that describes the source for posts to the Twitter
  webservice.")

(defconst parakeet-curl-headers
  (list "X-Twitter-Client: Parakeet"
	"X-Twitter-Client-Version: 1.0a"
	"X-Twitter-Client-URL: http://blogs.escapecs.com/parakeet-twitter-info.xml")
  "A list of the headers used when communicating with the Twitter
web service.")

(defconst parakeet-urls
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public "http://twitter.com/statuses/public_timeline.json" hash)
    (puthash 'friend "https://twitter.com/statuses/friends_timeline.json" hash)
    (puthash 'update "https://twitter.com/statuses/update.json" hash)
    (puthash 'user "http://twitter.com/statuses/user_timeline/" hash)
    hash)
  "A hash of the URL's that return timeline data. These are used
  to communicate with the Twitter web service.")

(defconst parakeet-user-urls
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'show "http://twitter.com/users/show/" hash)
    hash)
  "A hash of the URL's that return user data. These are used to
  communication with the Twitter web service.")

;; Package errors

;; this is our general "I can't get Twitter on the line" error
(put 'communication-error
     'error-conditions
     '(error parakeet-errors 'communication-error))

;; this covers all error responses from Twitter
(put 'twitter-error
     'error-conditions
     '(error parakeet-errors 'twitter-error))

;; errors where the user types in something crazy
(put 'bad-input-error
     'error-conditions
     '(error parakeet-errors 'parakeet-error))

(defun parakeet-curl-args (&optional credentials)
  "Returns the arguments to use when invoking curl to load data
from the Twitter webservice."
  (append
   parakeet-default-curl-args
   (libcurl-parse-headers parakeet-curl-headers)
   (if credentials
       (list "-u" (concat parakeet-twitter-user ":" parakeet-twitter-password)))
   (if parakeet-socks-proxy
       (list "--socks4" parakeet-socks-proxy))))

(defun parakeet-curl-post-args (data &optional credentials)
  "Returns the arguments to use when invoking curl to post data
to the Twitter webservice. The 'data' should be in the format of
a property list."
  (append
   (parakeet-curl-args credentials)
   (list "--request" "POST" "--data" 
	 (libcurl-parse-data (append parakeet-source-info data)))))

(defun parakeet-timeline-data (timeline-type &optional credentials username)
  "Returns an array of data that contains the most recent tweets
for the provided timeline type."

  ;; setup a variable for the data
  (let ((json-data nil))
    (save-excursion

      ;; pass our arguments to curl and grab the returned buffer
      (let ((buffer-temp (libcurl
                          (parakeet-curl-args credentials)
                          (if (string= timeline-type 'user)
                              (if (< 0 (length username))
                                  (concat (gethash timeline-type parakeet-urls)
                                          username ".json")
                                (signal 'bad-input-error
                                        (list "I need the name of a person's timeline to display!")))
                            (gethash timeline-type parakeet-urls)))))

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
    
    ;; check the returned data for errors
    (if (and (listp json-data) (string= (first (first json-data)) 'error))
	(signal 'twitter-error (list (cdr (first json-data)))))
    json-data))

(defun parakeet-post (post-type post-data &optional credentials)
  "Posts the provided data to the given URL. If credentials are
provided, they are used. The result from the Twitter webservice
is returned."

  ;; setup a variable for the data
  (let ((json-data nil))
    (save-excursion

      ;; pass our arguments to curl and grab the returned buffer
      (let ((buffer-temp (libcurl
                          (parakeet-curl-post-args 
			   (list (list "status" post-data))
			   credentials)
			  (gethash post-type parakeet-urls))))

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
    
    ;; check the returned data for errors
    (if (and (listp json-data) (string= (first (first json-data)) 'error))
	(signal 'twitter-error (list (cdr (first json-data)))))
    json-data))

(defun parakeet-public-timeline-data (&optional credentials)
  "Returns an array of data that contains the twenty most recent
tweets on the Twitter public timeline"
  (parakeet-timeline-data 'public credentials))

(defun parakeet-friend-timeline-data (credentials &optional username)
  "Returns an array of data that contains the twenty most recent
tweets from the user's private Twitter friend timeline. To log
into Twitter, the values in the credentials list will be
used. They should be in the format (username password)."
  (parakeet-timeline-data 'friend credentials))

(defun parakeet-user-timeline-data (credentials username)
  "Returns an array of data that contains the twenty most recent
  tweets from the user's public timeline."
  (parakeet-timeline-data 'user credentials username))

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

(defun parakeet-user-data (query-type username &optional credentials)
  "Returns an array of data that contains the extended
  information about the user."

  ;; setup a variable for the data
  (let ((json-data nil))
    (save-excursion

      ;; pass our arguments to curl and grab the returned buffer
      (let ((buffer-temp (libcurl
                          (parakeet-curl-args credentials)
                          (concat (gethash query-type parakeet-user-urls)
                                  username ".json"))))

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

    ;; check the returned data for errors
    (if (and (listp json-data) (string= (first (first json-data)) 'error))
	(signal 'twitter-error (list (cdr (first json-data)))))
    json-data))

(defun parakeet-user-show (username &optional credentials)
  "Returns a hash table containing the provided user's extended information."
  (parakeet-user-data 'show username))

(provide 'parakeet)