;;; config-evil.el --- -*- lexical-binding: t; -*-

(use-package evil
  :config
  ;; vim-like clipboard
  (setq x-select-enable-clipboard nil)
  ;; restore mac system key bind
  ;; cmd-v
  (dolist (state '(emacs motion normal visual insert))
    (eval `(define-key ,(intern (format "evil-%s-state-map" state))
            (kbd "s-v") #'clipboard-yank)))
  (global-set-key (kbd "s-v") #'clipboard-yank)
  ;; cmd-c
  (dolist (state '(emacs motion normal visual insert))
    (eval `(define-key ,(intern (format "evil-%s-state-map" state))
            (kbd "s-c") #'clipboard-kill-ring-save)))
  (global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
  ;; cmd-x
  (dolist (state '(emacs motion normal visual insert))
    (eval `(define-key ,(intern (format "evil-%s-state-map" state))
            (kbd "s-x") #'clipboard-kill-region)))
  (global-set-key (kbd "s-x") #'clipboard-kill-region)
  ;; git hunk
  (map! :n "g[" #'+vc-gutter/previous-hunk
        :n "g]" #'+vc-gutter/next-hunk ))

(defun ++evil/embark-visual-select (_str)
  "Switch to visual state with region selected"
  ;; (interactive (list (region-beginning) (region-end)))
  ;; (evil-visual-select STATE END)
  (message "-- VISUAL EMBARK BOUNDS --"))

(cl-defun ++evil/visul-select-for-embark-act (&key bounds &allow-other-keys)
  "Go to end of the target BOUNDS."
  (when (number-or-marker-p (cdr bounds))
    (evil-visual-select (car bounds) (cdr bounds))))

(use-package embark
  :config
  ;; hook with embarked upon
  ;; embark-pre-action-hooks
  ;; embark-around-action-hooks
  ;; embark-post-action-hooks
  (add-to-list 'embark-post-action-hooks '(++evil/embark-visual-select ++evil/visul-select-for-embark-act))
  ;; interactively function call method is different to normal function
  (define-key embark-general-map "v" #'++evil/embark-visual-select))

(message "[config] Apply config-evil")
(provide 'config-evil)
;;; config-evil.el ends here
