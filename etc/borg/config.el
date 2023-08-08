;;; etc/borg/config.el -*- lexical-binding: t; -*-

(defun borg-batch-rebuild-init ()
  (message "\n--- [init.el] ---\n")
  (message "skipp by etc/borg/config.el"))

;; dependence from doom optionally
;; (let ((repos '(;; for lsp-bridge
;;                "yasnippet"
;;                "markdown-mode"
;;                ;; for org-gtd-habitica
;;                "org-gtd"
;;                ))
;;       (straight-builded-dir
;;        (expand-file-name
;;         (format ".local/straight/build-%s" emacs-version)
;;         user-emacs-directory)))
;;   (dolist (repo repos)
;;     (add-to-list 'load-path (format "%s/%s" straight-builded-dir repo))))

;; dependence from doom
(let ((default-directory
       (expand-file-name
        (format ".local/straight/build-%s" emacs-version)
        user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))
