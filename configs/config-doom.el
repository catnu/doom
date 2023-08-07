;;; config/config-doom.el -*- lexical-binding: t; -*-

(defvar required-fonts '("LXGW WenKai Mono"
                         "Mononoki Nerd Font Mono"
                         "JetBrains Mono"
                         "Overpass"
                         "JuliaMono"
                         "IBM Plex Mono"
                         "Fira Code"      ; for ligatures
                         ;; "Merriweather"
                         ;; "Alegreya"
                         ))

(let ((font-chinese "LXGW WenKai Mono"))
  ;; font
  (setq doom-font (font-spec :family "JetBrains Mono" :size 18)
        doom-big-font (font-spec :family "JetBrains Mono" :size 30)
        doom-unicode-font (font-spec :family "JuliaMono")
        doom-serif-font (font-spec :family "IBM Plex Mono" :size 16 :weight 'light)
        ;; doom-variable-pitch-font (font-spec :family font-chinese :size 18)
        ;; doom-variable-pitch-font (font-spec :family "Overpass" :size 20)
        )
  ;; 字体
  (add-hook! emacs-startup :append
    ;; (set-fontset-font t ?中 font-chinese nil 'prepend)
    ;; (set-fontset-font t ?言 font-chinese nil 'prepend)
    (set-fontset-font t 'cjk-misc font-chinese nil 'prepend)
    (set-fontset-font t 'han font-chinese nil 'prepend)))

(setq doom-theme 'doom-vibrant)

(after! doom-modeline
  :config
  (setq doom-modeline-lsp t
        doom-modeline-height 10
        ;; doom-modeline-bar-width 2
        )
  (display-time-mode 1)                             ; Enable time in the mode-line
  (when (string-match-p "^Power Battery" (battery)) ; On laptops...
    (display-battery-mode 1))                       ; it's nice to know how much power you have

  ;; (set-face-attribute 'mode-line-active nil :family "JuliaMono Bold" :height 160) ; For 29+
  (set-face-attribute 'mode-line nil :family "Mononoki Nerd Font Mono" :height 180)
  (set-face-attribute 'mode-line-inactive nil :family "Mononoki Nerd Font Mono" :height 180))

;; (remove-hook 'window-setup-hook #'doom-init-theme-h)
;; (add-hook 'after-init-hook #'doom-init-theme-h 'append)

(message "[config] Apply config-doom")
(provide 'config-doom)
