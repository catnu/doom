;;; configs/config-notes.el -*- lexical-binding: t; -*-

;; (use-package obsidian
;;   :config
;;   (setq obsidian-inbox-directory "Inbox"))

(defun ++notes/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(defun ++notes/org-journal-capture-from-clipboard ()
  (with-selected-frame (if (frame-live-p ++org-pop/frame)
                           ++org-pop/frame
                         (selected-frame))
    (++notes/org-journal-find-location)
    (insert "\n- ")
    (insert (format-time-string "%H:%M "))
    (clipboard-yank)))

(message "[config] Apply config-notes")
(provide 'config-notes)