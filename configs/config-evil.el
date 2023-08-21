;;; config-evil.el ---                               -*- lexical-binding: t; -*-

(use-package evil
  :config
  ;; vim-like clipboard
  (setq x-select-enable-clipboard nil)
  ;; restore mac system key bind
  (define-key evil-normal-state-map (kbd "s-v") #'clipboard-yank)
  (define-key evil-motion-state-map (kbd "s-v") #'clipboard-yank)
  (global-set-key (kbd "s-v") #'clipboard-yank)
  (define-key evil-normal-state-map (kbd "s-c") #'clipboard-kill-ring-save)
  (define-key evil-motion-state-map (kbd "s-c") #'clipboard-kill-ring-save)
  (global-set-key (kbd "s-c") #'clipboard-kill-ring-save))

(with-eval-after-load 'ediff-util
  (add-hook 'ediff-mode-hook #'evil-emacs-state)
  ;; modify my ediff mode
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "c" #'ediff-copy-both-to-C)
    (define-key ediff-mode-map "i" #'(lambda () (interactive)
                                       (switch-to-buffer-other-window ediff-buffer-C)
                                       (or (key-binding (kbd "C-c C-c"))
                                           (local-set-key (kbd "C-c C-c")
                                                          #'(lambda ()
                                                              (interactive)
                                                              (switch-to-buffer-other-window "*Ediff Control Panel*")))))))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))


(message "[config] Apply config-evil")
(provide 'config-evil)
;;; config-evil.el ends here
