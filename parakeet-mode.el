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

;; That will load in the parakeet-mode code and get it ready for use.

;; You can then set some variable with your Twitter username and
;; password.

;; (custom-set-variables '(parakeet-twitter-user "SomePerson"))
;; (custom-set-variables '(parakeet-twitter-password "clever-password"))

;; If you have a proxy SOCKS server, you can set that as well...

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

(defcustom parakeet-mode-initialize-fns
  (list 'auto-fill-mode)
  "Functions to call when entering Parakeet mode."
  :type 'list
  :group 'parakeet-mode)

(defcustom parakeet-mode-edit-initialize-fns
  (list (lambda () (flyspell-mode 1)))
  "Functions to call when entering Parakeet edit mode (i.e., to
post a tweet)."
  :type 'list
  :group 'parakeet-mode)

;; Constants

(defconst parakeet-headers
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public (lambda (&optional arguments)
                       "Twitter Public Timeline")  hash)
    (puthash 'friend (lambda (&optional arguments)
                       "Twitter Friend Timeline") hash)
    (puthash 'user (lambda (arguments)
                     (concat "Twitter Timeline for " arguments)) hash)
    hash)
  "A hash of headings for the Twitter output.")

(defconst parakeet-buffer-names
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public "*parakeet*" hash)
    (puthash 'friend "*parakeet*" hash)
    (puthash 'user "*parakeet*" hash)
    hash)
  "A hash of buffer names for the Twitter output.")

(defconst parakeet-data-functions
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public 'parakeet-public-timeline-data hash)
    (puthash 'friend 'parakeet-friend-timeline-data hash)
    (puthash 'user 'parakeet-user-timeline-data hash)
    hash)
  "A hash of functions that retrieve Twitter data.")

(defconst parakeet-fetch-messages
  (let ((hash (make-hash-table :test 'eql)))
    (puthash 'public "Fetching the public Twitter timeline..." hash)
    (puthash 'friend "Fetching your friend Twitter timeline..." hash)
    (puthash 'user "Fetching the person's Twitter timeline..." hash)
    hash)
  "A hash of status messages to display while fetching Twitter data.")

(defconst parakeet-input-buffer-name
  "*parakeet-input*"
  "The name of buffers used by Parakeet to collect user input.")

;; required packages
(require 'parakeet)
(require 'parakeet-utils)

;; mode and keybindings

(define-derived-mode parakeet-mode
  text-mode "Parakeet"
  "Major mode for Twitter."
  (setq case-fold-search nil))

(define-key parakeet-mode-map (kbd "C-n") 'parakeet-next-tweet)
(define-key parakeet-mode-map (kbd "C-p") 'parakeet-previous-tweet)
(define-key parakeet-mode-map (kbd "RET") 'parakeet-next-user-timeline)
(define-key parakeet-mode-map (kbd "C-c l") 'parakeet-tweet-length-feedback)
(define-key parakeet-mode-map (kbd "C-c C-c") 'parakeet-post-status)

;; the rest of the code

(put 'parakeet-mode 'mode-class 'special)

(defun parakeet-handle-error (error-in)
  "Informs the user that an error occurred via the minibuffer"
  (let ((error-type (first error-in))
    (error-message (first (cdr error-in))))

    (message "%s"
         (concat
          (if (string= error-type "communication-error")
              "There was a problem communicating with Twitter: ")
          (if (string= error-type "twitter-error")
              "Twitter had a problem: ")
          (if (string= error-type "parakeet-error")
              "I had a problem: ")
          error-message))
    error-in))

(defun parakeet-credentials ()
  "Returns a list that contains the user's Twitter username and
password."
  (list parakeet-twitter-user parakeet-twitter-password))

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
         (prkt-screen-name (parakeet-tweet-value 'screen_name prkt-user))
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
    (insert (propertize "%screen-name%" 'invisible 't))
    (insert (propertize prkt-screen-name 'invisible 't))
    (insert (propertize "%screen-name%" 'invisible 't))
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
      (parakeet-invoke-list parakeet-mode-initialize-fns)

      ;; make sure flyspell mode is off
      (flyspell-mode -1)
      
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

      ;; move to the top of the buffer, set read-only
      (goto-line 2)
      (beginning-of-line)
      (recenter)
      (setq buffer-read-only t))

    ;; switch focus to the new buffer
    (set-window-buffer (selected-window) twitter-out)))

(defun parakeet-display-timeline (timeline-type &optional arguments)
  "Displays a timeline of Twitter data to the user through a buffer."

  ;; setup variables for the data and any errors
  (let ((prkt-data nil)
        (error-result nil))

    (with-temp-message (gethash timeline-type parakeet-fetch-messages)
      (condition-case error-in
          (setq prkt-data (funcall
                           (gethash timeline-type parakeet-data-functions)
                           (parakeet-credentials) arguments))
        (error
         (setq error-result error-in))))

    ;; display the data if we have it
    (if (not (null prkt-data))
        (parakeet-print-timeline
         (funcall (gethash timeline-type parakeet-headers) arguments)
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
  (parakeet-display-timeline 'friend (parakeet-credentials)))

(defun parakeet-user-timeline (username)
  "Displays the Twitter timeline of the specified user."
  (interactive "MTwitter User's screen name: ")
  (parakeet-display-timeline 'user username))

(defun parakeet-next-tweet ()
  "Move point to the beginning of the next tweet."
  (interactive)
  (search-forward "%tweet-start%%user%"))

(defun parakeet-previous-tweet ()
  "Moves the point to the beginning of the previous tweet."
  (interactive)
  (search-backward "%tweet-start%"))

(defun parakeet-next-user-timeline ()
  "Displays the timeline of the next user in the timeline buffer."
  (interactive)
  (search-forward-regexp "%screen-name%\\(\\w+\\)%screen-name%")
  (let ((screen-name (match-string-no-properties 1)))
    (parakeet-user-timeline screen-name)))

(defun parakeet-status-buffer (&optional text-in)
  "Creates a new buffer for collecting user input. If text-in is
present, the contents of the variable is inserted into the
buffer."

  ;; kill the input buffer if it's already open
  (if (get-buffer parakeet-input-buffer-name)
      (kill-buffer parakeet-input-buffer-name))

  ;; create and open the input buffer
  (let ((input-buffer (get-buffer-create parakeet-input-buffer-name))
    (input-window (split-window)))
    (select-window input-window)
    (switch-to-buffer input-buffer)
    (parakeet-mode)
    (parakeet-invoke-list parakeet-mode-initialize-fns)
    (parakeet-invoke-list parakeet-mode-edit-initialize-fns)
    (if (not (null text-in))
	(insert text-in))
    input-window))

(defun parakeet-status ()
  "Prompts the user for their current status and posts it to
Twitter. Returns the window that is expecting input."
  (interactive)
  (parakeet-status-buffer))

(defun parakeet-status-region (start end)
  "Prompts the user for their current status and posts it to
Twitter. The contents of the selected region is inserted into the
buffer. Returns the window that is expecting input."
  (interactive "r")
  (parakeet-status-buffer (parakeet-region-string start end)))

(defun parakeet-trim-trailing (text-in)
  "Trims the whitespace from the end of a string."
  (replace-regexp-in-string "[ \t]*$" "" text-in))

(defun parakeet-strip-newlines (text-in)
  "Removes all of the newlines from the string."
  (replace-regexp-in-string "[\n]+" " " text-in))

(defun parakeet-trim-leading (text-in)
  "Trims the whitespace from the front of a string."
  (replace-regexp-in-string "^[ \t]+" "" text-in))

(defun parakeet-strip (text-in)
  "Trims space from the beginning and end of a string."
  (parakeet-trim-leading
   (parakeet-trim-trailing (parakeet-strip-newlines text-in))))

(defun parakeet-post-status ()
  "Grabs the text in the current buffer and uses that text to
update the current status."
  (interactive)

  ;; make sure we're in the parakeet input buffer
  (if (not (string= (buffer-name (current-buffer))
                    parakeet-input-buffer-name))
      (message "%s" "You can't just post anything as your Twitter status!")

    ;; get the buffer text
    (progn
      (let ((raw-tweet (parakeet-strip
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
            (prkt-data nil)
            (error-result nil))

        ;; make sure our tweet isn't too lengthy
        (if (not (<= (length raw-tweet) 140))
            (message "%s" "Your tweet must be 140 characters or less. :(")

          ;; post the new tweet
          (progn
            (with-temp-message "Updating your Twitter status..."
              (condition-case error-in
                  (setq prkt-data
                        (parakeet-post 'update raw-tweet
                                       (parakeet-credentials)))
                (error
                 (setq error-result error-in)))

              ;; make sure the tweet posted, then kill the buffer
              (if (not (null prkt-data))
                  (progn
                    (set-buffer-modified-p nil)
                    (delete-windows-on parakeet-input-buffer-name)
                    (kill-buffer parakeet-input-buffer-name)
                    (message "%s" "Your Twitter status has been updated!"))

                ;; let the user know something went wrong
                (if error-result
                    (parakeet-handle-error error-result))))))))))

(defun parakeet-tweet-length-feedback ()
  "Provides feedback to the user indicating how long their tweet
is."
  (interactive)
  (save-excursion
    (let ((prkt-length (length (parakeet-strip
				(buffer-substring-no-properties
				 (point-min) (point-max))))))
      (if (<= prkt-length 140)
	  (message "%s" (concat (number-to-string prkt-length)
				" Your tweet is characters long."))
	(message "%s" (concat "Your tweet is " 
			      (number-to-string (- prkt-length 140))
			      " characters too long!"))))))

(provide 'parakeet-mode)