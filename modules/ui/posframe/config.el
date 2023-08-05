;;; ui/postframe/config.el -*- lexical-binding: t; -*-

(when (modulep! :completion vertico +childframe)
  (use-package! vertico-posframe
   :init
   (setq vertico-posframe-font "Mononoki Nerd Font Mono 18")))
