;; parakeet-utils.el - Provides utilities for Parakeet mode.
;; Author: Christopher Miles <twitch@nervestaple.com>

;; This file is not part of GNU EMacs.

(defgroup parakeet-utils '()
  "A package that provides utility functions for Parakeet mode.")

;; This package provides utility functions for Parakeet mode. You
;; probably don't want to use these functions for anything all on it's
;; own.

(defun parakeet-invoke-list (list-in)
  "Invoked each function in the list without any arguments."
  (let ((prkt-list-fns list-in))
    (while (car prkt-list-fns)
      (funcall (car prkt-list-fns))
      (setq prkt-list-fns (cdr prkt-list-fns)))))

(provide 'parakeet-utils)