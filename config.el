;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(add-to-list 'load-path (expand-file-name "configs/" doom-user-dir))
;; design: depend on seperated config file name conversation
;; filename: configs/config-<name>.el
;; provide: (provide 'config-<name>)
(let ((subconfigs
       '("doom"            ; doom ui, doom font
         ;; "better-default"
         )))
  (dolist (name subconfigs)
    (require (intern (format "config-%s" name)))))
