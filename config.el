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
       '("doom"                  ; doom ui, doom font
         "evil"
         "posframe"              ; pop childframe
         "windows"               ; tab and windows
         "borg"                  ; secondary packages management, lib management
         "emojify"               ; native emojis in emacs
         "lookup"
         "rg"
         "habitica"
         "applescript"
         "web-dev"
         "lsp-bridge"
         "git"
         "org-protocol"          ; protocol, hammerspoon
         "notes"
         "better-default")))
  (dolist (name subconfigs)
    (require (intern (format "config-%s" name)))))

;; (use-package habitica)

(add-to-list '+lookup-provider-url-alist
             '("Emacs China" "https://emacs-china.org/search?expanded=true&q=%s"))

;; (add-to-list '+org-babel-native-async-langs 'restclient)

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

;; global replace
;; (global-set-key [remap +default/find-in-notes] #'consult-notes)
(global-set-key [remap isearch-forward] #'+default/search-buffer)   ; C-s
(global-set-key [remap list-buffers] #'consult-buffer-other-window) ; C-x C-b
(global-set-key (kbd "C-<backspace>") #'(lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-j") nil)                                ; prevent C-j
(global-set-key (kbd "C-q") nil)                                ; prevent C-q
(global-set-key (kbd "C-\\") nil)                               ; prevent C-q

(map! :v "C-f" #'indent-for-tab-command ;; = evil-indent
      (:leader
       :desc "Pop up scratch buffer" "x"
       #'(lambda () (interactive)
           ;; option 1 if has scratch winodw kill it
           ;; (let ((window (get-buffer-window "*doom:scratch*")))
           ;;   (if window (delete-window window)
           ;;     (call-interactively #'doom/open-scratch-buffer)))
           ;; option 2 kill scratch window only in it
           (if (string= (buffer-name (current-buffer)) "*doom:scratch*") (delete-window)
             (call-interactively #'doom/open-scratch-buffer)))
       "it" #'hl-todo-insert))

;; Support
(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; init later
(add-hook 'doom-first-buffer-hook
          (lambda ()
            (require 'config-utils)
            ;; SPC tab 1~9 for workspace
            ;; SPC 1~9 for tab
            ;; (sort-tab-turn-on) ;(require 'config-windows)
            ;; enable mode here
            (+global-word-wrap-mode) ;need wrod-wrap moduole
            ;; show 80 charater boundary
            ;; (set-face-foreground 'fill-column-indicator "gray40")
            ;; (global-display-fill-column-indicator-mode)
            ;; tracking
            ;; (global-wakatime-mode)
            ;; vimish fold mode
            ;; (vimish-fold-global-mode 1)
            ;; fix evil-collection error
            ;; (add-hook 'messages-buffer-mode-hook #'+word-wrap-mode)
            (require 'config-modeline)
            (require 'config-auto-save); auto save with evil
            ;; (require 'config-mind-wave)
            (after! config-lsp-bridge (require 'config-treesit))
            ;; (after! (or (featurep 'org) (require 'ob-async)))
            (require 'config-jinx)
            ))

;; (advice-add 'risky-local-variable-p :override #'ignore-risky-local-variable-p-1)
;; (defun ignore-risky-local-variable-p-1 (sym &optional _ignored)
;;   (advice-remove 'risky-local-variable-p #'ignore)
;;   nil)
;; (advice--p (advice--symbol-function 'risky-local-variable-p))


;;------------;;
;; playground ;;
;;------------;;

;; wrap +lookup/definition as xref backend
;; TODO: function in lookup 是不是已经可以直接做 xfre-backend 了？
(defun xfre-doom-lookup-definition (identifier)
  (interactive)
  (let ((file nil) (line nil) (column nil))
    (save-window-excursion
      (+lookup/definition identifier)
      (setq file (buffer-file-name)
            line (line-number-at-pos)
            column (current-column)))
    (list (xref-make identifier (xref-make-file-location file line column)))))
(defun doom-lookup-xref-backend () 'doom-lookup-xref-backend)
(cl-defmethod xref-backend-definitions ((_backend (eql doom-lookup-xref-backend)) symbol)
  (xfre-doom-lookup-definition symbol))
;; 不要加入 functions 因为 +lookup 调用 xref-backend
;; x (add-hook 'xref-backend-functions #'doom-lookup-xref-backend)

;; 源码阅读工具 command: citre-peek
(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  :config
  ;; map 一些 evil 的快捷方式
  (defun ++citre-peek-scroll-down ()
    "Scroll to the next line in a peek window."
    (interactive)
    (citre-peek--error-if-not-peeking)
    (citre-peek--line-forward 7))

  (defun ++citre-peek-scroll-up ()
    "Scroll to the previous line in a peek window."
    (interactive)
    (citre-peek--error-if-not-peeking)
    (citre-peek--line-forward -7))
  ;; fix evil keymap 无效
  (advice-add 'citre-peek :after #'evil-force-normal-state)
  (advice-add 'citre-peek-jump :after #'evil-force-normal-state)
  (dolist (state '(motion normal)) ;; expcept emacs state insert
    (evil-define-key state citre-peek-keymap (kbd "h") #'citre-peek-chain-backward)
    (evil-define-key state citre-peek-keymap (kbd "l") #'citre-peek-chain-forward)
    (evil-define-key state citre-peek-keymap (kbd "j") #'citre-peek-next-line)
    (evil-define-key state citre-peek-keymap (kbd "k") #'citre-peek-prev-line)
    (evil-define-key state citre-peek-keymap (kbd "J") #'citre-peek-jump)
    (evil-define-key state citre-peek-keymap (kbd "d") #'citre-peek-through)
    (evil-define-key state citre-peek-keymap (kbd "D") #'citre-peek-through-reference)
    (evil-define-key state citre-peek-keymap (kbd "p") #'citre-peek-prev-branch)
    (evil-define-key state citre-peek-keymap (kbd "n") #'citre-peek-next-branch)
    (evil-define-key state citre-peek-keymap (kbd "C-d") #'++citre-peek-scroll-down)
    (evil-define-key state citre-peek-keymap (kbd "C-u") #'++citre-peek-scroll-up))
  (map! :nm "g'" #'citre-peek)
  ;; set doom-lookup as citre last backend
  (defvar citre-doom-lookup-backend
    (citre-xref-backend-to-citre-backend
     'doom-lookup-xref-backend (lambda () t)
     :symbol-atpt-fn #'doom-thing-at-point-or-region))
  (citre-register-backend 'doom-lookup citre-doom-lookup-backend)
  (add-to-list 'citre-find-definition-backends 'doom-lookup t))

(defun ++switch-flycheck-list-errors ()
  (interactive)
  (flycheck-list-errors)
  (pop-to-buffer "*Flycheck errors*"))

;; ;; FIX: citre peek ui 和 flycheck error 都使用 postframe 时存在冲突
;; (defun posframe-poshandler-point-top-left-corner-up (info)
;;   "A posframe poshandler which position posframe at point's top left corner.
;; This poshandler will let posframe move one line up from the current line."
;;   (cons (car (window-inside-pixel-edges))
;;         (- (cdr (posframe-poshandler-point-top-left-corner info))
;;            (frame-char-height))))
;; (use-package flycheck
;;   :init
;;   (setq flycheck-posframe-position 'point-top-left-corner-up))
