;;; configs/config-windows.el -*- lexical-binding: t; -*-

(use-package! ace-window
  :defer t
  :init
  ;; (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-leading-char-face '(:foreground "green" :bold t))
  ;; (setq aw-font-size 20)
  (custom-set-faces
   '(aw-leading-char-face
     ((((class color)) (:foreground "orange" :height 3.0))
      (((background dark)) (:foreground "gray100"))
      (((background light)) (:foreground "gray0"))
      (t (:foreground "gray100" :underline nil)))))
  (setq aw-scope 'global
        aw-background t)
  (add-to-list 'aw-ignored-buffers "*sort-tab*"))

(map! (:leader
       ;; SPC w
       ;; split sv
       :n "ws" #'+evil/window-split-and-follow
       :n "wS" #'evil-window-split
       :n "wv" #'+evil/window-vsplit-and-follow
       :n "wV" #'evil-window-vsplit
       ;; navigation hjkl
       ;; enlage/ballence fF
       :n "wf" #'doom/window-enlargen
       :n "wF" #'balance-windows
       ;; selection/exchange/deletion aowedD
       :n "wa" #'ace-window
       :n "wo" #'other-window
       :n "we" #'evil-window-exchange
       :n "wd" #'ace-delete-other-windows
       :n "wD" #'ace-delete-window ))

(message "[config] Apply config-windows")
(provide 'config-windows)
