;;; configs/config-auto-save.el -*- lexical-binding: t; -*-

;; auto save for evil
(when (modulep! :editor evil)
  ;; no need for config auto save
  (setq auto-save-default nil) ; Nobody likes to loose work, I certainly don't
  ;; for auto-save-mode not auto-save-visited-mode

  (evil-define-local-var ++auto-save/local-timer nil)

  (defvar ++auto-save/buffer-name-blacklist
    '("COMMIT_EDITMSG")
    "The blacklist of buffers to auto save.")

  (defvar ++auto-save/disabled-modes
    '(fundamental-mode so-long-mode)
    "The mode list not ot enable auto save")

  (defmacro ++with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))

  (defun ++auto-save/save-file-idle ()
    (let ((buffer (current-buffer)))
      (and
       (null ++auto-save/local-timer)
       ;; (buffer-file-name buffer) ; only buffer is visted file
       (buffer-modified-p)
       ;; (not (memq (buffer-name buffer)
       ;;            ++auto-save/buffer-name-blacklist))
       (setq ++auto-save/local-timer
             (run-with-timer
              6 nil ; idle second, repeat nil
              #'++auto-save/timer-run-save buffer)))))

  (defun ++auto-save/timer-run-save (buffer)
    (and (get-buffer buffer)
         (with-current-buffer buffer
           ;; from auto save so set local timer to nil
           (setq ++auto-save/local-timer nil)
           (and (buffer-modified-p)
                (++with-suppressed-message (save-buffer))))))

  (defun ++auto-save/follow-evil-insert-to-normal ()
    "when form insert state call save-file-idle"
    (and (eq 'insert evil-previous-state)
         ;; (not (derived-mode-p 'special-mode))
         (++auto-save/save-file-idle)))

  (defun ++auto-save/follow-after-change (_begin _end _length)
    "every change outside `insert, emacs state' launch auto save"
    (or (memq evil-state '(insert emacs)) (++auto-save/save-file-idle)))

  (defun ++auto-save/cancel-timer-when-need ()
    "break timer to stop next auto save file idle round"
    (unless (null ++auto-save/local-timer)
      (cancel-timer ++auto-save/local-timer)
      (setq ++auto-save/local-timer nil)))

  (defun ++auto-save/global-enable-mode ()
    (or (derived-mode-p 'special-mode)
        (memq major-mode ++auto-save/disabled-modes)
        (null (buffer-file-name (current-buffer))); only buffer is visted file
        (memq (buffer-name (current-buffer)) ++auto-save/buffer-name-blacklist)
        (++auto-save-mode +1)))

  (define-globalized-minor-mode ++global-auto-save-mode
    ++auto-save-mode
    ++auto-save/global-enable-mode)

  (define-minor-mode ++auto-save-mode
    "A minor mode that separate inline automatically."
    :init-value nil
    :global nil
    (if (not ++auto-save-mode)
        ;;disable
        (progn
          (remove-hook 'evil-normal-state-entry-hook #'++auto-save/follow-evil-insert-to-normal t)
          (remove-hook 'after-change-functions #'++auto-save/follow-after-change t)
          (remove-hook 'evil-insert-state-entry-hook #'++auto-save/cancel-timer-when-need t)
          (remove-hook 'before-save-hook #'++auto-save/cancel-timer-when-need t)))
    ;; enable
    (progn
      ;; moment to start idle timer
      (add-hook 'evil-normal-state-entry-hook #'++auto-save/follow-evil-insert-to-normal nil t)
      (add-hook 'after-change-functions #'++auto-save/follow-after-change nil t)
      ;; moment to stop idle timer
      (add-hook 'evil-insert-state-entry-hook #'++auto-save/cancel-timer-when-need nil t)
      (add-hook 'before-save-hook #'++auto-save/cancel-timer-when-need nil t)))

  ;; enable auto save global
  (++global-auto-save-mode)
  (message "[config] Apply config-auto-save"));; end of when

(provide 'config-auto-save)