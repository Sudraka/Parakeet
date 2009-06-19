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

;; (require 'parakeet-mode)

;; This package isn't complete yet. Right now, the following functions
;; are available:
;;
;; (parakeet-public-timeline) - Displays the public Twitter timeline

;; required packages
(require 'parakeet)

(defconst parakeet-public-timeline-buffer-name
  "*parakeet-public-timeline*"
  "The name of the buffer that Parakeet uses to display the
public Twitter timeline.")

(defun parakeet-handle-error (error-in)
  "Informs the user that an error occurred via the minibuffer"
  (message "%s" 
	   (concat "I couldn't communicate with Twitter. "

			   ;; if we have an error message, display it
			   (if (car (cdr error-in))
				   (car (cdr error-in)))))
  error-in)

(defun parakeet-print-tweet (tweet buffer-in)
  "Prints the tweet to the buffer provided"

  ;; pull our variables from the tweet
  (let* ((prkt-user (parakeet-tweet-value 'user tweet))
		 (prkt-user-name (parakeet-tweet-value 'name prkt-user))
		 (prkt-tweet (parakeet-tweet-value 'text tweet))
		 (prkt-source (parakeet-tweet-value 'source tweet))
		 (prkt-time (parakeet-tweet-value 'created_at tweet)))

	;; print the tweet to the buffer
	(princ prkt-user-name buffer-in)
	(princ '": " buffer-in)
	(princ prkt-tweet buffer-in)
	(terpri buffer-in)
	(princ prkt-time buffer-in)
	(princ '", from " buffer-in)
	(princ prkt-source buffer-in)
	(terpri buffer-in)))

(defun parakeet-print-public-timeline (twitter-data)
  "Displays the public Twitter timeline"
  
  ;; kill the buffer if it's already extant
  (if (get-buffer parakeet-public-timeline-buffer-name)
	  (kill-buffer parakeet-public-timeline-buffer-name))
  
  ;; create our new buffer to hold the output
  (let ((twitter-out (get-buffer-create parakeet-public-timeline-buffer-name)))
	(save-excursion
	  (set-buffer twitter-out)
	  (goto-char (point-min))

	  ;; loop through the tweets
	  (let ((index 0))
		(while (< index (length twitter-data))
		  (parakeet-print-tweet (elt twitter-data index) twitter-out)
		  (terpri twitter-out)
		  (setq index (1+ index))))
	  (setq buffer-read-only t))

	;; switch focus to the new buffer
	(set-window-buffer (selected-window) twitter-out)
	(goto-char (point-min))))

(defun parakeet-public-timeline ()
  "Displays the public Twitter timeline."

  ;; setup variables for the data and any errors
  (let ((prkt-data nil)
		(error-result nil))

	;; warn the user and start fetching the data from the net
	(with-temp-message "Fetching the public Twitter timeline..."
	  (condition-case error-in
		  (setq prkt-data (parakeet-public-timeline-data))
		(error 
		 (setq error-result error-in))))
	
	;; display the data if we have it
	(if (not (null prkt-data))
		(parakeet-print-public-timeline prkt-data))

	;; display any errors
	(if error-result
		(parakeet-handle-error error-result))))

(provide 'parakeet-mode)

;; Test code starts here!

(parakeet-public-timeline)

