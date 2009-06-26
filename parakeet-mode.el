;; parakeet-mode.el - Provides a mode for managing your Twitter feed
;; Author: Christopher Miles <twitch@nervestaple.com>

;; This file is not part of GNU Emacs.

(defgroup parakeet-mode '()
  "A package for managing your Twitter feed! View and post to
Twitter right from the comfy confines of your Emacs session.")

;; I often feel the need to Twitter from inside the Emacs environment
;; and find it to be kind of a hassle to grab the mouse and interact
;; with a desktop Twitter client. This package provides all the
;; functionality that I've been looking for an none of the
;; functionality that I haven't.

;; Installation: Put this file somewhere in your load-path and then
;; add the following to your initialization file:

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/parakeet")
;; (load "~/.emacs.d/site-lisp/parakeet/autoload.el")

;; That will oad in the parakeet-mode code and get it ready for use if
;; you have a proxy server, you can set that as well...

;; (custom-set-variables '(parakeet-socks-proxy "http://localhost:9080"))

;; This package isn't complete yet. Right now, the following functions
;; are available:

;; parakeet-public-timeline - Displays the public Twitter timeline

;; parakeet-friend-timeline - Displays the user's friend timeline

;; Customizable variables

(defcustom parakeet-date-format "%m/%d/%Y %I:%M %p"
  "The format used when displaying dates from Twitter"
  :type 'string
  :group 'parakeet-mode)

(defcustom parakeet-dark-theme
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'header-background "#183152" hash)
    (puthash 'header "#FFFFFF" hash)
    (puthash 'user "#ABCAE2" hash)
    (puthash 'tweet "#FFFFFF" hash)
    (puthash 'meta "#375D81" hash)
    hash)
  "Color theme for display against dark backgrounds"
  :type 'hash-table
  :group 'parakeet-mode)

(defcustom parakeet-theme parakeet-dark-theme
  "The theme used to display output."
  :type 'hash-table
  :group 'parakeet-mode)

;; Constants

(defconst parakeet-headers
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public "Twitter Public Timeline" hash)
    (puthash 'friend "Twitter Friend Timeline" hash)
    hash)
  "A hash of headings for the Twitter output.")

(defconst parakeet-buffer-names
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public "*parakeet*" hash)
    (puthash 'friend "*parakeet*" hash)
    hash)
  "A hash of buffer names for the Twitter output.")

(defconst parakeet-data-functions
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public 'parakeet-public-timeline-data hash)
    (puthash 'friend 'parakeet-friend-timeline-data hash)
    hash)
  "A hash of functions that retrieve Twitter data.")

(defconst parakeet-fetch-messages
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public "Fetching the public Twitter timeline..." hash)
    (puthash 'friend "Fetching your friend Twitter timeline..." hash)
    hash)
  "A hash of status messages to display while fetching Twitter data.")

;; required packages
(require 'parakeet)

;; mode and keybindings

(define-derived-mode parakeet-mode
  text-mode "Parakeet"
  "Major mode for Twitter."
  (setq case-fold-search nil))

(define-key parakeet-mode-map (kbd "C-n") 'parakeet-next-tweet)
(define-key parakeet-mode-map (kbd "C-p") 'parakeet-previous-tweet)

(put 'parakeet-mode 'mode-class 'special)

(defun parakeet-handle-error (error-in)
  "Informs the user that an error occurred via the minibuffer"
  (message "%s"
       (concat "I couldn't communicate with Twitter. "

           ;; if we have an error message, display it
           (if (and (car (cdr error-in))
                (stringp (car (cdr error-in))))
               (car (cdr error-in)))))
  error-in)

(defun parakeet-format-twitter-date (date-in)
  "Formats a date from Twitter for display."
  (format-time-string parakeet-date-format (date-to-time date-in)))

(defun parakeet-format-date (date-in)
  "Formats a date from for display."
  (format-time-string parakeet-date-format date-in))

(defun prkt-is-url-p (text-in)
  "Returns true if the text is a valid URL"
  (if (string-match '"<a .*>.*</a>" text-in)
      t
    nil))

(defun prkt-url-text (url-in)
  "Returns the text portion of a valid URL"
  (nth 2 (split-string url-in "[<>]")))

(defun prkt-url-target (url-in)
  "Returns the anchor target portion of a valid URL"
  (nth 1
       (split-string
    (nth 1 (split-string
        (nth 1 (split-string url-in "[<>]")) "=")) "\"")))

(defun parakeet-format-source (tweet-source)
  "Returns the source in a format for display."
  (if (prkt-url-text tweet-source)
      (prkt-url-text tweet-source)
    tweet-source))

(defun parakeet-print-tweet (tweet)
  "Inserts the Tweet into the current buffer."

  ;; pull our variables from the tweet
  (let* ((prkt-user (parakeet-tweet-value 'user tweet))
     (prkt-user-name (parakeet-tweet-value 'name prkt-user))
     (prkt-tweet (parakeet-tweet-value 'text tweet))
     (prkt-source (parakeet-tweet-value 'source tweet))
     (prkt-time (parakeet-tweet-value 'created_at tweet))
     (prkt-point-start (point)))

    ;; print the tweet to the buffer
    (insert (propertize "%tweet-start%" 'invisible 't 'face
            (list :foreground (gethash 'user parakeet-theme))))
    (insert (propertize "%user%" 'invisible 't 'face
            (list :foreground (gethash 'user parakeet-theme))))
    (insert (propertize prkt-user-name 'face
            (list :foreground (gethash 'user parakeet-theme))))
    (insert (propertize "%user%" 'invisible 't))
    (insert '": ")
    (insert (propertize "%tweet%" 'invisible 't))
    (insert (propertize prkt-tweet 'face
            (list :foreground (gethash 'tweet parakeet-theme))))
    (insert (propertize "%tweet%" 'invisible 't))
    (insert (propertize "%meta%" 'invisible 't))
    (insert (propertize
         (propertize
          (concat '" at " (parakeet-format-twitter-date prkt-time)
              '", from " (parakeet-format-source prkt-source))
          'face 'italic) 'face
          (list :foreground (gethash 'meta parakeet-theme))))
    (insert (propertize "%meta%" 'invisible 't))
    (insert (propertize "%tweet-end%" 'invisible 't))
    (insert (propertize " " 'invisible nil))

    ;; fill the paragraph
    (fill-region prkt-point-start (point))))

(defun parakeet-insert-header (header-in)
  "Insert a header with the supplied text into the current buffer"
  (setq header-line-format (list (concat " " header-in))))

(defun parakeet-print-timeline (header twitter-data buffer-name)
  "Prints the provided Twitter data with the provided header to a
buffer with the name provided. If that buffer exists already, it
is killed and re-created."

  ;; kill the buffer if it's already extant
  (if (get-buffer buffer-name)
      (kill-buffer buffer-name))

  ;; create our new buffer to hold the output
  (let ((twitter-out (get-buffer-create buffer-name)))
    (save-excursion
      (set-buffer twitter-out)
      (parakeet-mode)
      (goto-char (point-min))

      ;; insert a header
      (parakeet-insert-header header)
      (terpri twitter-out)

      ;; loop through the tweets
      (let ((index 0))
    (while (< index (length twitter-data))
      (parakeet-print-tweet (elt twitter-data index))
      (terpri twitter-out) (terpri twitter-out)
      (setq index (1+ index))))
      (setq buffer-read-only t))

    ;; switch focus to the new buffer
    (set-window-buffer (selected-window) twitter-out)
    (goto-char (point-min))))

(defun parakeet-display-timeline (timeline-type)
  "Displays a timeline of Twitter data to the user through a buffer."

  ;; setup variables for the data and any errors
  (let ((prkt-data nil)
    (error-result nil))

    (with-temp-message (gethash timeline-type parakeet-fetch-messages)
      (condition-case error-in
          (setq prkt-data (funcall
                           (gethash timeline-type parakeet-data-functions)))
        (error
         (setq error-result error-in))))

    ;; display the data if we have it
    (if (not (null prkt-data))
    (parakeet-print-timeline (gethash timeline-type parakeet-headers)
                             prkt-data
                             (gethash timeline-type parakeet-buffer-names)))

    ;; display any errors
    (if error-result
    (parakeet-handle-error error-result))))

(defun parakeet-public-timeline ()
  "Displays the public Twitter timeline."
  (interactive)
  (parakeet-display-timeline 'public))

(defun parakeet-friend-timeline ()
  "Displays your friend Twitter timeline."
  (interactive)
  (parakeet-display-timeline 'friend))

(defun parakeet-next-tweet ()
  "Move point to the beginning of the next tweet."
  (interactive)
  (search-forward "%tweet-start%"))

(defun parakeet-previous-tweet ()
  "Moves the point to the beginning of the previous tweet."
  (interactive)
  (search-backward "%tweet-start%"))

(provide 'parakeet-mode)
