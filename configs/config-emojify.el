;;; configs/config-emojify.el -*- lexical-binding: t; -*-

(require 'config-borg)

(add-hook 'doom-first-buffer-hook
          (lambda ()
            ;; set emoji font
            (when (member "Apple Color Emoji" (font-family-list))
              (if (version< "27.0" emacs-version)
                  (set-fontset-font
                   "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
                (set-fontset-font
                 t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))
            (use-package emojify
              :config
              (setq emojify-display-style 'unicode)
              (setq emojify-emoji-styles '(unicode)))))

              ;; (bind-key* (kbd "C-c .") #'emojify-insert-emoji)) ; override binding in any mode
(message "[config] Apply config-emojify")
(provide 'config-emojify)
