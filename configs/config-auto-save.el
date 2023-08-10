;;; configs/config-auto-save.el -*- lexical-binding: t; -*-

(setq auto-save-default t) ; Nobody likes to loose work, I certainly don't

;; auto save for evil
(when (modulep! :editor evil)

  (evil-define-local-var config/auto-save--local-timer nil)

  (defvar config/auto-save--buffer-name-blacklist
    '("COMMIT_EDITMSG")
    "The blacklist of buffers to auto save.")

  (defvar config/auto-save--disabled-modes
    '(fundamental-mode so-long-mode)
    "The mode list not ot enable auto save")

  (defun config/auto-save--save-file-idle ()
    (let ((buffer (current-buffer)))
      (and
       (null config/auto-save--local-timer)
       ;; (buffer-file-name buffer) ; only buffer is visted file
       (buffer-modified-p)
       ;; (not (memq (buffer-name buffer)
       ;;            config/auto-save--buffer-name-blacklist))
       (setq config/auto-save--local-timer
             (run-with-idle-timer
              6 nil ; idle second, repeat nil
              #'config/auto-save--timer-run-save buffer)))))

  (defun config/auto-save--timer-run-save (buffer)
    (and (get-buffer buffer)
         (with-current-buffer buffer
           ;; from idle save so set local timer to nil
           (setq config/auto-save--local-timer nil)
           (and (buffer-modified-p)
                (save-buffer)))))

  (defun config/auto-save--follow-evil-insert-to-normal ()
    "when form insert state call save-file-idle"
    (and (eq 'insert evil-previous-state)
         ;; (not (derived-mode-p 'special-mode))
         (config/auto-save--save-file-idle)))

  (defun config/auto-save--follow-after-change (_begin _end _length)
    "every change outside `insert, emacs state' launch auto save"
    (or (memq evil-state '(insert emacs)) (config/auto-save--save-file-idle)))

  (defun config/auto-save--cancel-timer-when-need ()
    "break timer to stop next auto save file idle round"
    (unless (null config/auto-save--local-timer)
      (cancel-timer config/auto-save--local-timer)
      (setq config/auto-save--local-timer nil)))

  (defun config/auto-save--global-enable-mode ()
    (or (derived-mode-p 'special-mode)
        (memq major-mode config/auto-save--disabled-modes)
        (null (buffer-file-name (current-buffer))); only buffer is visted file
        (memq (buffer-name (current-buffer)) config/auto-save--buffer-name-blacklist)
        (config/auto-save-mode +1)))

  (define-globalized-minor-mode config/global-auto-save-mode
    config/auto-save-mode
    config/auto-save--global-enable-mode)

  (define-minor-mode config/auto-save-mode
    "A minor mode that separate inline automatically."
    :init-value nil
    :global nil
    (if (not config/auto-save-mode)
        ;;disable
        (progn
          (remove-hook 'evil-normal-state-entry-hook #'config/auto-save--follow-evil-insert-to-normal t)
          (remove-hook 'after-change-functions #'config/auto-save--follow-after-change t)
          (remove-hook 'evil-insert-state-entry-hook #'config/auto-save--cancel-timer-when-need t)
          (remove-hook 'before-save-hook #'config/auto-save--cancel-timer-when-need t)))
    ;; enable
    (progn
      ;; moment to start idle timer
      (add-hook 'evil-normal-state-entry-hook #'config/auto-save--follow-evil-insert-to-normal nil t)
      (add-hook 'after-change-functions #'config/auto-save--follow-after-change nil t)
      ;; moment to stop idle timer
      (add-hook 'evil-insert-state-entry-hook #'config/auto-save--cancel-timer-when-need nil t)
      (add-hook 'before-save-hook #'config/auto-save--cancel-timer-when-need nil t)))

  ;; enable auto save global
  (config/global-auto-save-mode)
  (message "[config] Apply config-auto-save"));; end of when

(provide 'config-auto-save)
