;;; configs/config-lsp-bridge.el -*- lexical-binding: t; -*-

(defvar ++lsp-bridge/follow-hooks
  '(;; c/c++ go rust python ruby
    c-ts-mode-hook
    c++-ts-mode-hook
    go-ts-mode-hook
    python-ts-mode-hook
    sh-mode-hook
    emacs-lisp-mode-hook
    ;; org-mode-hook
    ruby-ts-mode-hook
    ;; vue-mode-hook
    ;; rjsx-mode-hook
    js-ts-mode-hook
    typescript-mode-hook
    rjsx-mode-hook
    ))

;;; modify from https://github.com/kongds/.doom.d/blob/main/configs/init-lsp-bridge.el

(defun ++lsp-bridge/action-follow-hook ()
  (unless (or (string-match "\*org-src-fontification:" (buffer-name))
              (string-match "\*Org Src" (buffer-name))
              (string-match "\*temp" (buffer-name))
              (string-match ".org-src-babel" (buffer-name))
              (equal (buffer-name) "*Capture*"))
    (setq-local corfu-auto nil)
    (lsp-bridge-mode)
    (setq-local +lookup-definition-functions (append '(lsp-bridge-find-def) +lookup-definition-functions)
                +lookup-implementations-functions (append '(lsp-bridge-find-impl) +lookup-implementations-functions)
                +lookup-references-functions (append '(lsp-bridge-find-references) +lookup-references-functions)))

  ;; doom company
  ;; (unless (equal major-mode 'org-mode)
  ;;   (yas-minor-mode -1))

  ;; use lsp bridge as diagnosis
  (when (member major-mode '(python-mode c-mode c++-mode sh-mode))
    (flycheck-mode -1)))

;; use lsp bridge for following modes
(dolist (hook ++lsp-bridge/follow-hooks)
  (add-hook hook #'++lsp-bridge/action-follow-hook))

;; (add-hook 'tex-mode-hook #'(lambda()
;;                              (setq-local corfu-auto t)
;;                              (setq-local corfu-auto-delay 0.1)
;;                              (setq-local
;;                               completion-at-point-functions
;;                               (list (cape-super-capf #'cape-dabbrev #'cape-tex #'cape-dict #'cape-file)))))

;; (after! lookup
;;   ;; fix rtags find defination
;;   ;; (point-marker) cannot work with different buffer
;;   (defun +lookup--run-handlers (handler identifier origin)
;;     (doom-log "Looking up '%s' with '%s'" identifier handler)
;;     (condition-case-unless-debug e
;;         (let ((wconf (current-window-configuration))
;;               (window (selected-window))
;;               (result (condition-case-unless-debug e
;;                           (+lookup--run-handler handler identifier)
;;                         (error
;;                          (doom-log "Lookup handler %S threw an error: %s" handler e)
;;                          'fail))))
;;           (cond ((eq result 'fail)
;;                  (set-window-configuration wconf)
;;                  nil)
;;                 ((or (get handler '+lookup-async)
;;                      (eq result 'deferred)))
;;                 ((or result
;;                      (null origin)
;;                      (/= (point-marker) origin))
;;                  (prog1 (with-current-buffer (window-buffer window) (point-marker))
;;                    (set-window-configuration wconf)))))
;;       ((error user-error)
;;        (message "Lookup handler %S: %s" handler e)
;;        nil))))

;; A client/server indexer for c/c++/objc[++]
;; (after! rtags
;;   ;; if rtags-imenu return No Symbols, use imenu
;;   (defun rtags-imenu-filter-output  (ret)
;;     (message "rtags-imenu-filter-output: %s" ret)
;;     (if (equal ret "RTags: No symbols")
;;         (command-execute 'imenu)  ret))
;;   (advice-add #'rtags-imenu :filter-return #'rtags-imenu-filter-output))

(use-package! lsp-bridge
  :after evil
  :commands lsp-bridge-mode
  :init
  ;; can't find enough prefix key
  ;; (setq acm-enable-quick-access t)
  ;; (setq acm-quick-access-modifier 'super)
  ;; (setq acm-quick-access-keys '("j" "l" "f" "s" "." "g" "d" "b" "x" ","))
  (setq acm-enable-tabnine nil)
  (setq acm-enable-yas t)
  (setq acm-enable-tempel t)
  (setq acm-enable-preview t)

  ;;(setq acm-candidate-match-function 'orderless-flex)
  (setq acm-candidate-match-function 'regexp-quote)
  (setq acm-enable-dabbrev nil)
  (setq lsp-bridge-python-lsp-server "pyright-background-analysis")
  (setq lsp-bridge-disable-backup nil)
  ;; (setq lsp-bridge-symbols-enable-which-func t)
  (setq lsp-bridge-enable-mode-line nil)
  (setq lsp-bridge-enable-signature-help nil)
  (setq lsp-bridge-python-ruff-lsp-server "pyright-background-analysis_ruff")
  (setq lsp-bridge-enable-hover-diagnostic t)

  (setq lsp-bridge-enable-log nil)
  (setq acm-enable-copilot t)
  (setq acm-enable-codeium nil)
  ;; 测试
  (setq acm-backend-copilot-network-proxy '(:host "127.0.0.1", :port 8877))
  ;; (setq lsp-bridge-org-babel-lang-list nil)
  ;; (setq lsp-bridge-enable-org-babel t)

  :config
  (add-to-list 'evil-emacs-state-modes 'lsp-bridge-ref-mode)


  ;; (add-hook 'lsp-bridge-mode-hook
  ;;           #'(lambda ()
  ;;               (delete `(lsp-bridge-mode (" [" lsp-bridge--mode-line-format "] ")) mode-line-misc-info)))
  (setq acm-completion-backend-merge-order '("copilot-candidates"

                                             "mode-first-part-candidates"
                                             "template-first-part-candidates"
                                             "tabnine-candidates"
                                             ;; "copilot-candidates"
                                             "codeium-candidates"
                                             "template-second-part-candidates"
                                             "mode-second-part-candidates"))


  (remove-hook 'python-mode-local-vars-hook #'lsp!) ;; disable lsp
  (remove-hook 'python-mode-local-vars-hook #'+python-init-anaconda-mode-maybe-h)
  (remove-hook 'python-mode-hook #'+python-use-correct-flycheck-executables-h)

  (defvar lsp-bridge-running-window nil)
  (defvar lsp-bridge-running-eob-timer nil)
  (defun lsp-bridge-restart-process-try-eob ()
    (message "wait eob...")
    (if (and (window-live-p lsp-bridge-running-window)
             (equal (buffer-name (window-buffer lsp-bridge-running-window)) "*lsp-bridge*"))
        (when (> (with-current-buffer "*lsp-bridge*" (point-max))  100)
          (let ((sw (selected-window)))
            (select-window lsp-bridge-running-window)
            (recenter (- (max 1 scroll-margin)))
            (select-window sw))
          (acm-cancel-timer lsp-bridge-running-eob-timer))
      (acm-cancel-timer lsp-bridge-running-eob-timer)))

  (defun lsp-bridge-restart-process-after (&rest _)
    (when (window-live-p lsp-bridge-running-window)
      (set-window-buffer lsp-bridge-running-window (get-buffer "*lsp-bridge*"))
      (setq lsp-bridge-running-eob-timer
            (run-at-time 1 t #'lsp-bridge-restart-process-try-eob)))
    (delete-frame acm-menu-frame)
    (delete-frame acm-doc-frame))

  (advice-add #'lsp-bridge-restart-process
              :after #'lsp-bridge-restart-process-after)

  (advice-add #'lsp-bridge-restart-process
              :before #'(lambda (&rest _)
                          (setq lsp-bridge-running-window nil)
                          (dolist (window (window-list))
                            (when (equal (buffer-name (window-buffer window)) "*lsp-bridge*")
                              (setq lsp-bridge-running-window window)))))

  ;; auto login copilot
  (setq lsp-bridge-copilot-inited nil)
  (add-hook 'lsp-bridge-mode-hook
            #'(lambda ()
                "login copilot if didn't"
                (when (and acm-enable-copilot (not lsp-bridge-copilot-inited))
                  (run-with-timer ;; give lsp-bridge some time to startup
                   3
                   nil #'(lambda ()
                           ;; ensuer copilot login
                           (message "auto login copilot")
                           (call-interactively #'lsp-bridge-copilot-login)
                           (run-with-timer 3 nil #'lsp-bridge-copilot-status)))
                  (setq lsp-bridge-copilot-inited t))))

  ;; (after! org
  ;;   (advice-add #'+org/return
  ;;               :before #'(lambda ()
  ;;                           (setq lsp-org-babel-save-current--point (point)))))


  ;; (map!
  ;;  (:leader
  ;;   :desc "workspace symbol"
  ;;   :n "s n" #'lsp-bridge-workspace-list-symbols))
  ;;
  )

(use-package! acm
  :after lsp-bridge
  :config
  (setq acm-menu-length 18)
  ;; bindings
  (define-key acm-mode-map (kbd "TAB") 'acm-select-next)
  (define-key acm-mode-map (kbd "S-TAB") 'acm-select-prev)
  (define-key acm-mode-map (kbd "C-n") 'acm-select-next)
  (define-key acm-mode-map (kbd "C-p") 'acm-select-prev)
  ;; fix: evil C-n C-p
  (define-key acm-mode-map [remap evil-complete-next] #'acm-select-next)
  (define-key acm-mode-map [remap evil-complete-previous] #'acm-select-prev)

  ;; beginning-of-defun in this function slow down the lsp-bridge
  (fset 'r-acm-in-string-p (symbol-function 'acm-in-string-p))
  (fset 'r-acm-in-comment-p (symbol-function 'acm-in-comment-p))
  (defun acm-in-string-p (&optional state)
    (if (eq major-mode 'python-mode)
        nil
      (r-acm-in-string-p)))
  (defun acm-in-comment-p (&optional state)
    (if (eq major-mode 'python-mode)
        nil
      (r-acm-in-comment-p))))

;; (use-package! lsp-mode
;;   :commands lsp-mode
;;   :custom (lsp-completion-provider :none) ;; we use Corfu!
;;   :init (setq lsp-completion-enable t)
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))
;;     (setq-local completion-at-point-functions (list  (cape-super-capf #'lsp-completion-at-point #'cape-dict #'cape-file))))
;;   :hook (lsp-completion-mode . my/lsp-mode-setup-completion))

;; A few more useful configurations...
(after! emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; (setq +lsp-company-backends nil)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package! tempel
  :config
  (setq tempel-path (expand-file-name "snippets/templates/mixin.eld" doom-user-dir)))
(use-package! tempel-collection :after tempel)

;;;mind-wave

(message "[config] Apply config-lsp-bridge")

(provide 'config-lsp-bridge)
