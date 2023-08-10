;;; configs/config-auto-save.el -*- lexical-binding: t; -*-
(setq auto-save-default t) ; Nobody likes to loose work, I certainly don't

;; auto save for evil
(when (modulep! :editor evil)
  (evil-define-local-var config-auto-save/local-timer nil)

  (defvar config-auto-save/buffer-name-blacklist
    '("COMMIT_EDITMSG")
    "The blacklist of buffers to auto save.")

  (defun config-auto-save/save-file-idle ()
    (let ((buffer (current-buffer)))
      (and
       (null config-auto-save/local-timer)
       (buffer-file-name buffer) ; only buffer is visted file
       (buffer-modified-p)
       (not (memq (buffer-name buffer)
                  config-auto-save/buffer-name-blacklist))
       (setq config-auto-save/local-timer
             (run-with-idle-timer
              6 nil ; idle second, repeat nil
              #'config-auto-save/timer-run-save buffer)))))

  (defun config-auto-save/timer-run-save (buffer)
    (and (get-buffer buffer)
         (with-current-buffer buffer
           ;; from idle save so set local timer to nil
           (setq config-auto-save/local-timer nil)
           (and (buffer-modified-p)
                (save-buffer)))))

  (defun config-auto-save/follow-evil-insert-to-normal ()
    "when form insert state call save-file-idle"
    (and (eq 'insert evil-previous-state) (config-auto-save/save-file-idle)))

  (defun config-auto-save/follow-after-change (_begin _end _length)
    "every change outside `insert, emacs state' launch auto save"
    (or (memq evil-state '(insert emacs)) (config-auto-save/save-file-idle)))

  (defun config-auto-save/cancel-timer-when-need ()
    "break timer to stop next auto save file idle round"
    (unless (null config-auto-save/local-timer)
      (cancel-timer config-auto-save/local-timer)
      (setq config-auto-save/local-timer nil)))
  ;; moment to start idle timer
  (add-hook 'evil-normal-state-entry-hook #'config-auto-save/follow-evil-insert-to-normal)
  (add-to-list 'after-change-functions #'config-auto-save/follow-after-change)
  ;; moment to stop idle timer
  (add-hook 'evil-insert-state-entry-hook #'config-auto-save/cancel-timer-when-need)
  (add-hook 'before-save-hook #'config-auto-save/cancel-timer-when-need))

(message "[config] Apply config-auto-save")
(provide 'config-auto-save)
