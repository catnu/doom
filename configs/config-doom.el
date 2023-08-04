;;; config/config-doom.el -*- lexical-binding: t; -*-

;; font and theme
(setq doom-font (font-spec :family "JetBrains Mono" :size 18)
      doom-big-font (font-spec :family "JetBrains Mono" :size 30)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 20)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 16 :weight 'light))
(setq doom-theme 'doom-vibrant)

(defvar required-fonts '("JetBrains Mono" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya"))

;; (remove-hook 'window-setup-hook #'doom-init-theme-h)
;; (add-hook 'after-init-hook #'doom-init-theme-h 'append)

(message "apply config-doom")
(provide 'config-doom)
