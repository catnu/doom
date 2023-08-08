;;; configs/config-rg.el -*- lexical-binding: t; -*-

(require 'cl)

(defvar config-rg-deadgrep--last-buffer nil
  "Last buffer of deadgrep")

(defun config-rg-deadgrep-record-last-buffer ()
  (setq config-rg-deadgrep--last-buffer (current-buffer)))

(defun config-rg-jump-backward-respect-deadgrep ()
  (interactive)
  (let ((buf (current-buffer)))
    (unless (eq buf config-rg-deadgrep--last-buffer)
      (if (buffer-live-p config-rg-deadgrep--last-buffer)
          (let ((switch-method (if (get-buffer-window config-rg-deadgrep--last-buffer)
                                   #'switch-to-buffer-other-window
                                 #'switch-to-buffer)))
            (funcall switch-method config-rg-deadgrep--last-buffer)
            (setq config-rg-deadgrep--last-buffer buf))
        (funcall-interactively #'better-jumper-jump-backward)))))

(defun config-rg-deadgrep-or-finish-editing ()
  (interactive)
  (if (string-match-p "^*deadgrep " (buffer-name))
      (if (eq major-mode 'deadgrep-edit-mode)
          (deadgrep-mode)
        (progn
          (kill-buffer)
          (call-interactively #'config-rg-deadgrep)))
    (call-interactively #'config-rg-deadgrep)))

(use-package! deadgrep
  :config
  ;; use completing-read instead of reading-from-minibuffer
  (fset 'original-read-from-minibuffer (symbol-function 'read-from-minibuffer))
  (defun config-rg-deadgrep (search-term &optional directory)
    (interactive (list
                  (flet ((read-from-minibuffer
                          (prompt &optional a b c history sym-name d)
                          (flet ((read-from-minibuffer ; prevent looping
                                  (prompt &optional a b c history sym-name d)
                                  (original-read-from-minibuffer
                                   prompt a b c history sym-name d)))
                                (completing-read prompt (eval history) nil nil sym-name))))
                        (deadgrep--read-search-term))))
    (funcall-interactively #'deadgrep search-term directory))

  (defun config-rg-deadgrep-view-result-other-window ()
    "Goto the search result at point, opening in another window."
    (interactive)
    (config-rg-deadgrep-record-last-buffer)
    (let ((buf (current-buffer)))
      (deadgrep--visit-result #'find-file-other-window)
      (switch-to-buffer-other-window buf)))

  (advice-add #'deadgrep-visit-result :before #'config-rg-deadgrep-record-last-buffer)
  (advice-add #'deadgrep-visit-result-other-window :before #'config-rg-deadgrep-record-last-buffer)
  ;; modify keybind
  (global-set-key [remap deadgrep-visit-result-other-window] #'config-rg-deadgrep-view-result-other-window)
  (global-set-key [remap deadgrep-visit-result] #'deadgrep-visit-result-other-window))

(use-package! rg)

(map! :desc "rig" :n "gO" #'config-rg-jump-backward-respect-deadgrep ; :n go view result
      :leader
      :desc "ripgrep" :n "sr" #'config-rg-deadgrep-or-finish-editing
      :desc "ripgrep" :n "sR" #'rg-menu)

(message "[config] Apply config-rg")
(provide 'config-rg)
