;;; configs/config-better-default.el -*- lexical-binding: t; -*-

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil)      ; I don't think I've ever found this useful

(display-time-mode 1)                             ; Enable time in the mode-line

(when (string-match-p "^Power Battery" (battery)) ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

;; (global-subword-mode 1)                           ; Iterate through CamelCase words

;; (setq evil-vsplit-window-right t
;;       evil-split-window-below t)

;; action follow split window
;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (consult-buffer))

;; (map! :map evil-window-map
;;       "SPC" #'rotate-layout
;;       ;; Navigation
;;       "<left>"     #'evil-window-left
;;       "<down>"     #'evil-window-down
;;       "<up>"       #'evil-window-up
;;       "<right>"    #'evil-window-right
;;       ;; Swapping windows
;;       "C-<left>"       #'+evil/window-move-left
;;       "C-<down>"       #'+evil/window-move-down
;;       "C-<up>"         #'+evil/window-move-up
;;       "C-<right>"      #'+evil/window-move-right)

(message "[config] Apply config-better-default")
(provide 'config-better-default)
