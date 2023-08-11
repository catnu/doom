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
         "windows"           ; tab and windows
         "borg"              ; secondary packages management, lib management
         "emojify"           ; native emojis in emacs
         "lookup"
         "rg"
         "habitica"
         "applescript"
         "web-dev"
         "lsp-bridge"
         "better-default")))
  (dolist (name subconfigs)
    (require (intern (format "config-%s" name)))))

(use-package habitica)

(add-to-list '+lookup-provider-url-alist
             '("Emacs China" "https://emacs-china.org/search?expanded=true&q=%s"))

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

(add-hook 'doom-first-buffer-hook
          (lambda ()
            ;; SPC tab 1~9 for workspace
            ;; SPC 1~9 for tab
            (sort-tab-turn-on) ;(require 'config-windows)
            ;; enable mode here
            (+global-word-wrap-mode) ;need wrod-wrap moduole
            ;; show 80 charater boundary
            (set-face-foreground 'fill-column-indicator "gray40")
            (global-display-fill-column-indicator-mode)
            ;; tracking
            ;; (global-wakatime-mode)
            ;; vimish fold mode
            ;; (vimish-fold-global-mode 1)
            ;; fix evil-collection error
            (require 'config-modeline)
            (require 'config-auto-save); auto save with evil
            ))

;; global replace
;; (global-set-key [remap +default/find-in-notes] #'consult-notes)
(global-set-key [remap isearch-forward] #'consult-line)             ; C-s
(global-set-key [remap list-buffers] #'consult-buffer-other-window) ; C-x C-b
(global-set-key (kbd "C-<backspace>") #'(lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-j") nil)                                ; prevent C-j

(map! :v "C-f" #'indent-for-tab-command ;; = evil-indent
      (:leader
       :desc "Pop up scratch buffer" "x"
       #'(lambda () (interactive)
           (let ((window (get-buffer-window "*doom:scratch*")))
             (if window (delete-window window)
               (call-interactively #'doom/open-scratch-buffer))))
       ;; my new prefix
       (:prefix ("\\" . "Quick")
        :desc "Cleanup posframe cache" "c"  #'(lambda () (interactive)
                                                (message "do vertico-posframe-cleanup")
                                                (vertico-posframe-cleanup))
        :desc "Prpjectile refresh cache" "p" #'projectile-invalidate-cache
        :desc "Save all buffers" "s" #'save-all-buffers)))

;; (advice-add 'risky-local-variable-p :override #'ignore-risky-local-variable-p-1)
;; (defun ignore-risky-local-variable-p-1 (sym &optional _ignored)
;;   (advice-remove 'risky-local-variable-p #'ignore)
;;   nil)
;; (advice--p (advice--symbol-function 'risky-local-variable-p))
