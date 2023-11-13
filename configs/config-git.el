;;; configs/config-git.el -*- lexical-binding: t; -*-

(defmacro without-yes-or-no (&rest body)
  "Override `yes-or-no-p' & `y-or-n-p',
not to prompt for input and return t."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
     ,@body))

(with-eval-after-load 'ediff-util
  (add-hook 'ediff-mode-hook #'evil-emacs-state)
  ;; modify my ediff mode
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (defun ++ediff/resume ()
    (interactive)
    (let ((ediff-panel-buf (get-buffer "*Ediff Control Panel*")))
      (if ediff-panel-buf
          (switch-to-buffer ediff-panel-buf)
        (user-error "no actived Ediff session"))
      (ediff-recenter)))

  (defun ++ediff/silent-quit ()
    (interactive)
    (without-yes-or-no (ediff-quit nil)))

  (defun ++ediff/silent-quit-previous-session ()
    (and (get-buffer "*Ediff Control Panel*")
         (with-current-buffer "*Ediff Control Panel*"
           (message "quit previous Ediff session")
           (++ediff/silent-quit))))

  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "Q" #'++ediff/silent-quit)
    (define-key ediff-mode-map "z" #'ediff-suspend)
    (define-key ediff-mode-map "c" #'ediff-copy-both-to-C)
    (define-key ediff-mode-map "i" #'(lambda () (interactive)
                                       (switch-to-buffer-other-window
                                        (if (buffer-live-p ediff-buffer-C)
                                            ediff-buffer-C
                                          ediff-buffer-B))
                                       (message "C-c C-c to finish modifiction and back to Ediff session")
                                       (or (key-binding (kbd "C-c C-c"))
                                           (local-set-key (kbd "C-c C-c")
                                                          #'(lambda ()
                                                              (interactive)
                                                              (switch-to-buffer-other-window "*Ediff Control Panel*")))))))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

(with-eval-after-load 'magit-ediff
  ;;;override
  (defun magit-ediff-show-working-tree (file)
    "Show changes between `HEAD' and working tree using Ediff. (ignore staged/index)
FILE must be relative to the top directory of the repository."
    (interactive
     (list
      (let ((section (magit-current-section)))
        (cond
         ((magit-file-section-p section)
          (message (oref section value)))
         ((magit-hunk-section-p section)
          (message (alist-get 'path (oref section value))))
         (t (magit-read-file-choice "Show changes in file"
                                    (magit-changed-files "HEAD")
                                    "No changed files"))))))
    (++ediff/silent-quit-previous-session)
    (magit-ediff-buffers ((magit-get-revision-buffer "HEAD" file)
                          (magit-find-file-noselect  "HEAD" file))
                         ((get-file-buffer file)
                          (find-file-noselect file)))))

(with-eval-after-load 'code-review
  (defun code-review-ediff-compare-file-at-point ()
    (interactive)
    (++ediff/silent-quit-previous-session)
    (let* ((section (magit-current-section))
           (file (cond
                  ((magit-file-section-p section)
                   (oref section value))
                  ((magit-hunk-section-p section)
                   (alist-get 'path (oref section value)))
                  (t (user-error "not in magit-section"))))
           (pr (code-review-db-get-pullreq))
           (base-upstream-branch (magit-get-upstream-branch (oref pr base-ref-name)))
           ;; assume same remote with base
           (remote (car (split-string base-upstream-branch "/"))))
      (magit-ediff-compare base-upstream-branch
                           (format "%s/%s" remote (oref pr head-ref-name))
                           file file)))
  (map! :map code-review-mode-map :n "e" #'code-review-ediff-compare-file-at-point))

(message "[config] Apply config-git")
(provide 'config-git)
