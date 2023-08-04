;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))

;; pirvate variable
(defvar private-file (expand-file-name ".private.el" doom-private-dir))
(when (file-exists-p private-file) (load private-file))

(add-to-list 'load-path (expand-file-name "configs/" doom-user-dir))
;; design: depend on seperated config file name conversation
;; filename: configs/config-<name>.el
;; provide: (provide 'config-<name>)
(let ((subconfigs
       '("doom"              ; doom ui, doom font
         "borg"              ; secondary packages management, lib management
         "better-default")))
  (dolist (name subconfigs)
    (require (intern (format "config-%s" name)))))

;; private default
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file) (load custom-file))
