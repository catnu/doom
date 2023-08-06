;;; etc/borg/config.el -*- lexical-binding: t; -*-

(defun borg-batch-rebuild-init ()
  (message "\n--- [init.el] ---\n")
  (message "skipp by etc/borg/config.el"))

;; dependence from doom
(let ((repos '("yasnippet"
               "markdown-mode"))
      (straight-builded-dir
       (expand-file-name
        (format ".local/straight/build-%s" emacs-version)
        user-emacs-directory)))
  (dolist (repo repos)
    (add-to-list 'load-path (format "%s/%s" straight-builded-dir repo))))
