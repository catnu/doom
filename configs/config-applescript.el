;;; configs/config-applescript.el -*- lexical-binding: t; -*-

(add-hook! emacs-startup :append
           ;;;; Allow editing of binary .scpt files (applescript) on mac.
           (add-to-list 'jka-compr-compression-info-list
                        `["\\.scpt\\'"
                          "converting text applescript to binary applescprit "
                          ,(expand-file-name "etc/shell/applescript-helper.sh" doom-user-dir) nil
                          "converting binary applescript to text applescprit "
                          ,(expand-file-name "etc/shell/applescript-helper.sh" doom-user-dir) ("-d")
                          nil t "FasdUAS"])
           ;;It is necessary to perform an update!
           (jka-compr-update)
           ;;fix auto-mode-alist for applescript
           (setq auto-mode-alist (delete '("\\.scpt\\'" nil jka-compr) auto-mode-alist))
           (use-package applescript-mode))

(message "[config] Apply config-applescript")
(provide 'config-applescript)
