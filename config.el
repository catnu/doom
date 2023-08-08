;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))

;; pirvate variable
(defvar private-file (expand-file-name ".private.el" doom-user-dir))
(when (file-exists-p private-file) (load private-file))

(add-to-list 'load-path (expand-file-name "configs/" doom-user-dir))
;; design: depend on seperated config file name conversation
;; filename: configs/config-<name>.el
;; provide: (provide 'config-<name>)
;; packages: from doom/packages.el or doom/lib
(let ((subconfigs
       '("doom"              ; doom ui, doom font
         "posframe"          ; pop childframe
         "rg"
         "borg"              ; secondary packages management, lib management
         "emojify"           ; native emojis in emacs
         "applescript"
         "web-dev"
         "lsp-bridge"
         "better-default")))
  (dolist (name subconfigs)
    (require (intern (format "config-%s" name)))))

;; global replace
;; (global-set-key [remap +default/find-in-notes] #'consult-notes)
(global-set-key [remap isearch-forward] #'consult-line)             ; C-s
(global-set-key [remap list-buffers] #'consult-buffer-other-window) ; C-x C-b
(global-set-key (kbd "C-j") nil)                                ; prevent C-j

(map! :v "C-f" #'indent-for-tab-command ;; = evil-indent

      (:leader
       (:prefix ("\\" . "Quick")
        :desc "Cleanup posframe cache" "c"  #'(lambda () (interactive)
                                                (message "do vertico-posframe-cleanup")
                                                (vertico-posframe-cleanup))
        :desc "Prpjectile refresh cache" "p" #'projectile-invalidate-cache
        :desc "Save all buffers" "s" #'save-all-buffers)))

(defun save-all-buffers ()
  "save buffers if there is modfiy remained"
  (interactive)
  (let ((need-to-save
         (memq t (mapcar (lambda (buf)
                           (and (buffer-file-name buf)
                                (buffer-modified-p buf)))
                         (buffer-list)))))
    (if need-to-save (progn
                       (message "saving all buffers...")
                       (save-some-buffers 0)
                       (message "all buffers saved"))
      (message "no modified buffers"))))

;; (advice-add 'risky-local-variable-p :override #'ignore-risky-local-variable-p-1)
;; (defun ignore-risky-local-variable-p-1 (sym &optional _ignored)
;;   (advice-remove 'risky-local-variable-p #'ignore)
;;   nil)
;; (advice--p (advice--symbol-function 'risky-local-variable-p))
