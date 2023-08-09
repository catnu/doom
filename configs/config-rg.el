;;; configs/config-rg.el -*- lexical-binding: t; -*-

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
  (defun config-rg-completing-read-from-minibuffer (prompt &optional a b c history sym-name d)
    "hack first read-from-minibuffer call to completing-read"
    (fset 'read-from-minibuffer (symbol-function 'original-read-from-minibuffer)) ;prevent loop and reset to normal
    (completing-read prompt (eval history) nil nil sym-name))
  (defun config-rg-deadgrep (search-term &optional directory)
    (interactive (list
                  (progn
                    (fset 'read-from-minibuffer (symbol-function 'config-rg-completing-read-from-minibuffer))
                    (deadgrep--read-search-term))))
    (fset 'read-from-minibuffer (symbol-function 'original-read-from-minibuffer)) ;reset to normal
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

(use-package rg)

(use-package color-rg
  :config
  (add-hook 'color-rg-mode-hook 'evil-insert-state))

(map! :desc "rig" :n "gO" #'config-rg-jump-backward-respect-deadgrep ; :n go view result
      :leader
      :desc "ripgrep" "sR" #'rg-menu
      "sr" nil)

(map! :leader
      :desc "directory special file" "fd" #'find-file-in-current-directory-by-selected
      :desc "project special file" "sf" #'find-file-in-project-by-selected
      (:prefix ("sr" . "ripgrep refactor")
       :desc "symbol in current file" "f" #'color-rg-search-symbol-in-current-file
       :desc "current directory all file" "d" #'color-rg-search-symbol
       :desc "current directory special file" "D" #'color-rg-search-symbol-with-type
       :desc "current project all file" "p" #'color-rg-search-project
       :desc "current project special file" "P" #'color-rg-search-project-with-type
       :desc "deadgrep" "r" #'config-rg-deadgrep-or-finish-editing))

(message "[config] Apply config-rg")
(provide 'config-rg)
