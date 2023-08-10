;;; configs/config-modeline.el -*- lexical-binding: t; -*-

;; Whether highlight the modified buffer name.
(setq doom-modeline-highlight-modified-buffer-name nil)
;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon nil)

(defconst config/modeline--default-color
  (cons (face-background 'mode-line)
        (face-foreground 'mode-line)))

(defvar config/modeline--show-modify nil)

(defun config/modeline-set-modeline-color (&optional color)
  (let ((color (or color (if config/modeline--show-modify '("#444488" . "#ffffff")
                           config/modeline--default-color))))
    (set-face-background 'mode-line-active (car color))
    (set-face-background 'mode-line-active (car color))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(defun config/modeline-show-modify-state ()
  "Change mode line color to notify user evil current state."
  (let ((file-modified (and (not (minibufferp))
                            (buffer-file-name (current-buffer))
                            (buffer-modified-p))))
    (unless (eq config/modeline--show-modify file-modified)
      (setq config/modeline--show-modify file-modified)
      (config/modeline-set-modeline-color))))

(defun config/modeline-follow-after-save ()
  ;; timer run at current buffer, but after save run at saved buffer
  (run-with-timer 0.5 nil #'config/modeline-show-modify-state))

(add-hook 'post-command-hook #'config/modeline-show-modify-state)
(add-hook 'after-save-hook #'config/modeline-follow-after-save)

(message "[config] Apply config-modeline")
(provide 'config-modeline)
