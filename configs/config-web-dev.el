;;; configs/config-web-dev.el -*- lexical-binding: t; -*-
(use-package! web-mode
  :config
  ;; Define vue-mode as kind of web-mode
  (define-derived-mode vue-mode web-mode "Vue")
  (setq auto-mode-alist (delete '("\\.vue\\'" . web-mode) auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode) 'append))

(setq restclient-inhibit-cookies t)

(message "[config] Apply config-web-dev")
(provide 'config-web-dev)
