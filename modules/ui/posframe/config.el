;;; ui/postframe/config.el -*- lexical-binding: t; -*-

(when (modulep! :completion vertico +childframe)
  (use-package! vertico-posframe
   :init
   (setq vertico-posframe-font "Sarasa Term SC Nerd 16")))
