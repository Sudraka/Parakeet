;; autoload.el - Loads Parakeet and it's required files, sets up key bindings.
;; Author: Christopher Miles <twitch@nervestaple.com>

;; This file is not a part of GNU Emacs.

;; load libraries
(load "libcurl.el")

;; load Parakeet
(load "parakeet.el")

;; load the Parakeet Mode for viewing tweets
(load "parakeet-mode.el")

;; setup a key bindings
(global-set-key (kbd "C-c ' p p") 'parakeet-public-timeline)
(global-set-key (kbd "C-c ' p f") 'parakeet-friend-timeline)

