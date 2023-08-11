;;; configs/config-modeline.el -*- lexical-binding: t; -*-

;; Whether highlight the modified buffer name.
(setq doom-modeline-highlight-modified-buffer-name nil)
;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon nil)

(defconst ++modeline/default-color
  (cons (face-background 'mode-line)
        (face-foreground 'mode-line)))

(defvar ++modeline/show-modify nil)

(defun ++modeline/set-modeline-color (&optional color)
  (let ((color (or color (if ++modeline/show-modify '("#444488" . "#ffffff")
                           ++modeline/default-color))))
    (set-face-background 'mode-line-active (car color))
    (set-face-background 'mode-line-active (car color))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(defun ++modeline/show-modify-state ()
  "Change mode line color to notify user evil current state."
  (let ((file-modified (and (not (minibufferp))
                            (buffer-file-name (current-buffer))
                            (buffer-modified-p))))
    (unless (eq ++modeline/show-modify file-modified)
      (setq ++modeline/show-modify file-modified)
      (++modeline/set-modeline-color))))

(defun ++modeline/follow-after-save ()
  ;; timer run at current buffer, but after save run at saved buffer
  (run-with-timer 0.5 nil #'++modeline/show-modify-state))

(add-hook 'post-command-hook #'++modeline/show-modify-state)
(add-hook 'after-save-hook #'++modeline/follow-after-save)

(message "[config] Apply config-modeline")
(provide 'config-modeline)
