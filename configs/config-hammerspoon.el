;;; configs/config-hammerspoon.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name
                         "editWithEmacs.spoon"
                         "~/.hammerspoon/Spoons"))
;; lazy load
(defun hammerspoon-edit-begin ()
  "Receive, from Hammerspoon, text to edit in Emacs"
  (interactive)
  (require 'hammerspoon)
  (funcall #'hammerspoon-edit-begin))

(defun hammerspoon-do (command)
  "Send Hammerspoon the given COMMAND."
  (interactive "sHammerspoon Command:")
  (require 'hammerspoon)
  (funcall #'hammerspoon-do command))

(message "[config] Apply config-hammerspoon")
(provide 'config-hammerspoon)
