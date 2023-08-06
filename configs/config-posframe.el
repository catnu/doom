;;; configs/config-posframe.el -*- lexical-binding: t; -*-

(when (modulep! :completion vertico +childframe)
  (use-package! vertico-posframe
   :init
   (setq vertico-posframe-font "Mononoki Nerd Font Mono 18"))

  (message "[config] Apply config-posframe"))
(provide 'config-posframe)
