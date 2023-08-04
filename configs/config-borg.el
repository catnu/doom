;;; configs/config-borg.el -*- lexical-binding: t; -*-

(setq epkg-repository (expand-file-name ".local/epkgs" doom-user-dir))
(add-to-list 'load-path (expand-file-name "lib/borg" doom-user-dir))
(require 'borg)

;;(package-initialize)
(borg-initialize)

(message "[config] Apply config-borg")
(provide 'config-borg)
