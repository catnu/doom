;;; configs/config-borg.el -*- lexical-binding: t; -*-

(setq epkg-repository (expand-file-name ".local/epkgs" doom-user-dir))
(add-to-list 'load-path (expand-file-name "lib/borg" doom-user-dir))
(require 'borg)

;; (package-initialize)
(borg-initialize)

(defun ++borg/custom-assimilate (host repo)
  "use borg install package"
  (let* ((package-name (file-name-base
                        (and (string-match "/\\(.+\\)$" repo)
                             (match-string 1 repo))))
         (url (format (pcase host
                        ;; add support here
                        ("github"  "git@github.com:%s.git")
                        ) repo)))
    (borg-assimilate package-name url)))

(message "[config] Apply config-borg")
(provide 'config-borg)
