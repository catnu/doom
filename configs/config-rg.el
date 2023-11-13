;;; configs/config-rg.el -*- lexical-binding: t; -*-

(defvar ++rg/deadgrep--last-buffer nil
  "Last buffer of deadgrep")

(defun ++rg/deadgrep-record-last-buffer ()
  (setq ++rg/deadgrep--last-buffer (current-buffer)))

(defun ++rg/jump-backward-respect-deadgrep ()
  (interactive)
  (let ((buf (current-buffer)))
    (unless (eq buf ++rg/deadgrep--last-buffer)
      (if (buffer-live-p ++rg/deadgrep--last-buffer)
          (let ((switch-method (if (get-buffer-window ++rg/deadgrep--last-buffer)
                                   #'switch-to-buffer-other-window
                                 #'switch-to-buffer)))
            (funcall switch-method ++rg/deadgrep--last-buffer)
            (setq ++rg/deadgrep--last-buffer buf))
        (funcall-interactively #'better-jumper-jump-backward)))))

(defun ++rg/deadgrep-or-finish-editing ()
  (interactive)
  (if (string-match-p "^*deadgrep " (buffer-name))
      (if (eq major-mode 'deadgrep-edit-mode)
          (deadgrep-mode)
        (progn
          (kill-buffer)
          (call-interactively #'++rg/deadgrep)))
    (call-interactively #'++rg/deadgrep)))

(use-package! deadgrep
  :config
  (add-hook 'deadgrep-mode-hook #'+word-wrap-mode)
  ;;;override
  (defun ++rg/deadgrep--read-search-term ()
    "Read a search term from the minibuffer.
If region is active, return that immediately.  Otherwise, prompt
for a string, offering the current word as a default."
    (let (search-term)
      (if (use-region-p)
          (progn
            (setq search-term
                  (buffer-substring-no-properties (region-beginning) (region-end)))
            (deactivate-mark))
        (let* ((sym (symbol-at-point))
               (sym-name (when sym
                           (substring-no-properties (symbol-name sym))))
               ;; TODO: prompt should say search string or search regexp
               ;; as appropriate.
               (prompt
                (deadgrep--search-prompt sym-name)))
          (setq search-term
                (completing-read prompt deadgrep-history nil nil sym-name))
          (when (equal search-term "")
            (setq search-term sym-name))))
      (unless (equal (car deadgrep-history) search-term)
        (push search-term deadgrep-history))
      search-term))

  (defun ++rg/deadgrep (search-term &optional directory)
    (interactive (list (++rg/deadgrep--read-search-term)))
    (funcall-interactively #'deadgrep search-term directory))

  (defun ++rg/deadgrep-view-result-other-window ()
    "Goto the search result at point, opening in another window."
    (interactive)
    (++rg/deadgrep-record-last-buffer)
    (let ((buf (current-buffer)))
      (deadgrep--visit-result #'find-file-other-window)
      (switch-to-buffer-other-window buf)))

  (advice-add #'deadgrep-visit-result :before #'++rg/deadgrep-record-last-buffer)
  (advice-add #'deadgrep-visit-result-other-window :before #'++rg/deadgrep-record-last-buffer)
  ;; modify keybind
  (global-set-key [remap deadgrep-visit-result-other-window] #'++rg/deadgrep-view-result-other-window)
  (global-set-key [remap deadgrep-visit-result] #'deadgrep-visit-result-other-window))

(use-package rg)

(use-package color-rg
  :config
  (add-hook 'color-rg-mode-hook 'evil-insert-state))

(map! :desc "rig" :n "gO" #'++rg/jump-backward-respect-deadgrep ; :n go view result
      :leader
      :desc "ripgrep" "sR" #'rg-menu
      "sr" nil)

(map! :leader
      :desc "directory special file" "fd" #'find-file-in-current-directory-by-selected
      :desc "directory special file" "fp" #'find-file-in-project-by-selected
      :desc "project special file" "sf" #'find-file-in-project-by-selected
      (:prefix ("sr" . "ripgrep refactor")
       :desc "symbol in current file" "f" #'color-rg-search-symbol-in-current-file
       :desc "current directory all file" "d" #'color-rg-search-symbol
       :desc "current directory special file" "D" #'color-rg-search-symbol-with-type
       :desc "current project all file" "p" #'color-rg-search-project
       :desc "current project special file" "P" #'color-rg-search-project-with-type
       :desc "deadgrep" "r" #'++rg/deadgrep-or-finish-editing))

(message "[config] Apply config-rg")
(provide 'config-rg)
