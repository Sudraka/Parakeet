;; autoload.el - Loads Parakeet and it's required files, sets up key bindings.
;; Author: Christopher Miles <twitch@nervestaple.com>

;; This file is not a part of GNU Emacs.

;; load libraries
(require 'libcurl)
(require 'parakeet)
(require 'parakeet-utils)
(require 'parakeet-mode)

;; setup a key bindings
(global-set-key (kbd "C-c ' p p") 'parakeet-public-timeline)
(global-set-key (kbd "C-c ' p f") 'parakeet-friend-timeline)
(global-set-key (kbd "C-c ' p u") 'parakeet-status)
(global-set-key (kbd "C-c ' p U") 'parakeet-status-region)
