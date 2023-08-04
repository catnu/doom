;; -*- no-byte-compile: t; -*-
;;; ui/postframe/packages.el

(when (modulep! :completion vertico +childframe)
  (package! posframe)
  (package! vertico-posframe))
