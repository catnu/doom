;;; configs/config-org-protocol.el -*- lexical-binding: t; -*-
(defvar ++vault-path-alist nil)

(defun ++org-protocol/vault-path (name)
  (alist-get (intern name) ++vault-path-alist))

(defun ++org-protocol/view-code-space (fname)
  (let* ((splitparts (org-protocol-parse-parameters fname nil '(:valut :workspace :filepath :test)))
         (vault (++org-protocol/vault-path (plist-get splitparts :vault)))
         (filepath (expand-file-name (string-replace
                                      ":/" "/" (org-protocol-sanitize-uri (plist-get splitparts :filepath)))
                                     vault))
         (test (plist-get splitparts :test))
         (workspace (or (plist-get splitparts :workspace) "view-codes")))
    (if (file-exists-p filepath)
        (progn
          (++utils/+workspace-new-or-switch-to workspace)
          (delete-other-windows)
          (find-file (expand-file-name filepath vault))
          (when test
            (+evil/window-split-and-follow)
            (find-file (expand-file-name test (file-name-directory filepath)))
            (message "SPC m t s/f to run single/file")))
      (user-error "file not exist"))
    nil))

;; org pop
(defvar ++org-pop/frame nil)

(defun ++org-pop/select-frame-by-name-regexp (regexp)
  (let ((frames (frame-list)))
    (catch 'found
      (dolist (frame frames)
        (when (string-match-p regexp (frame-parameter frame 'name))
          (select-frame-set-input-focus frame)
          (throw 'found frame))))))

(defun ++org-pop/focus (frame-name)
  "like `org-protocol://pop-focus?name=Doom%20Emacs'"
  (let* ((splitparts (org-protocol-parse-parameters frame-name nil '(:name)))
         (name (plist-get splitparts :name)))
    (pcase name
      ("Doom Emacs" (++org-pop/select-frame-by-name-regexp name))
      ("Org Journal"
       (unless (frame-live-p ++org-pop/frame)
         (setq ++org-pop/frame
               (make-frame '((name . "org-journal")
                             (alpha-background . 30)
                             (alpha . 85))))
         ;; (set-frame-parameter ++org-pop/frame 'alpha-background 30)
         ;; (set-frame-parameter ++org-pop/frame 'alpha '(85 . 50))
         (select-frame-set-input-focus ++org-pop/frame)
         (call-interactively #'org-journal-new-entry)
         (do-applescript ; press cmd + ctrl - 4
          "tell application \"System Events\"\nkey code 21 using {command down, control down}\nend tell"))
       ;; (select-frame-set-input-focus ++org-pop/frame)
       ;; Set the size and position of the emacs-everywhere frame.
       (cl-destructuring-bind (x . y)
           (mouse-absolute-pixel-position)
         (set-frame-position ++org-pop/frame
                             (- x 100)
                             (- y 50)))))
    nil))

(with-eval-after-load 'org-protocol
  (add-to-list 'org-protocol-protocol-alist
               '("view-code-space"
                 :protocol "view-code-space"
                 :function ++org-protocol/view-code-space))
  (add-to-list 'org-protocol-protocol-alist
               '("pop-focus"
                 :protocol "pop-focus"
                 :function ++org-pop/focus)))

(message "[config] Apply config-org-protocol")
(provide 'config-org-protocol)
