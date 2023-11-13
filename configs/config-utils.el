;;; config-utils.el ---                              -*- lexical-binding: t; -*-

(defun ++utils/vertico-find-file-other-window-in (&optional dir initial)
  "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
  (interactive)
  (require 'consult)
  (let* ((default-directory (or dir default-directory))
         (prompt-dir (consult--directory-prompt "Find" default-directory))
         (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
    (find-file-other-window
     (consult--read
      (split-string (cdr (apply #'doom-call-process cmd)) "\n" t)
      :prompt default-directory
      :sort nil
      :initial (if initial (shell-quote-argument initial))
      :add-history (thing-at-point 'filename)
      :category 'file
      :history '(:input +vertico/find-file-in--history)))))

(defun ++utils/clipboard-save-current-project-file-path ()
  (interactive)
  (let (;;(vault (project-name (project-current)))
        (file (file-relative-name
               buffer-file-name
               (project-root (project-current))))
        (select-enable-clipboard t))
    (kill-new file)))

(defun ++utils/clipboard-view-code-space-md-link ()
  (interactive)
  (let* (;;(vault (project-name (project-current)))
         (vault "notes")
         (project (completing-read "select folder: " (file-name-split buffer-file-name)))
         (file (file-relative-name buffer-file-name (project-root (project-current))))
         (base-name (file-name-base buffer-file-name))
         (extension (file-name-extension buffer-file-name))
         (code-file (format "%s.%s" base-name extension))
         (code-test (format "%s_test.%s" base-name extension))
         (select-enable-clipboard t))
    (kill-new (format "[☕️ %s: %s](org-protocol://view-code-space?vault=%s&filepath=%s&test=%s)"
                      project code-file vault file code-test))))

(defun ++utils/simple-rename-current-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (unless (string= name file)
      (if (vc-registered file)
          (vc-rename-file file name)
        (rename-file file name))
      (set-visited-file-name name t t))))

(defun ++utils/+workspace-new-or-switch-to (name)
  (let ((ws (+workspace-get name t)))
    (if ws (+workspace/switch-to name)
      (+workspace/new-named name))))

(defun ++utils/language-at-pos (&optional pos)
  "Message language at POS."
  (interactive "d")
  (let* ((pos (if pos pos (point)))
         (lang (cond
               ;; web-mode
               ((eq major-mode 'web-mode)
                (web-mode-language-at-pos pos))
               ;; mhtml-mode
               ((derived-mode-p 'mhtml-mode) "HTML")
               ((eq major-mode 'js-mode) "JS")
               ((eq major-mode 'css-mode) "CSS")
               ;; html-mode
               ((derived-mode-p 'html-mode)
                (let ((submode (get-text-property pos 'mhtml-submode)))
                  (if submode
                      (mhtml--submode-name submode)
                    "HTML")))
               ;TODO: rjsx-mode and other
               ((derived-mode-p 'prog-mode)
                (if (stringp mode-name) mode-name
                  (car mode-name)))
               (t
                (message "Don't know how to get language in %s" major-mode)
                nil))))
    (when lang
      (message "Language at point is %s" lang))
    lang))

;; (defun ++utils/async-org-babel-src-block ()
;;   (interactive)
;;   (let* ((begin (org-babel-where-is-src-block-head))
;;          (src-block (when begin
;;                       (search-forward-regexp "^#\+" nil t)
;;                       (end-of-line)
;;                       (buffer-substring-no-properties begin (point)))))
;;     (when begin
;;       (goto-char begin)
;;       (next-line)
;;       (org-babel-remove-result nil t)
;;       (org-babel-insert-result "Async Running...")

;;       (let ((async-child-init)))

;;       ;; TODO catch error
;;       ;; FIXME unsupport language
;;       (async-start
;;        ;; async run
;;        (lambda ()
;;           (setq org-confirm-babel-evaluate nil)
;;           ;; Initialize the new Emacs process with org-babel functions
;;           (setq exec-path exec-path)
;;           (setq load-path load-path)
;;           (package-initialize)
;;           ;; ,(async-inject-variables ob-async-inject-variables)
;;           (let ((default-directory default-directory))
;;             (with-temp-buffer
;;               (org-mode)
;;               (org-babel-do-load-languages org-babel-load-languages org-babel-load-languages)
;;               (insert src-block)
;;               (org-babel-execute-src-block)
;;               (goto-char (org-babel-where-is-src-block-result))
;;               (next-line)
;;               (beginning-of-line)
;;               (buffer-substring-no-properties (point) (point-max)))))
;;        ;; no error callback
;;        `(lambda (result)
;;           (with-current-buffer ,(current-buffer)
;;             (save-excursion
;;               (goto-char ,(point))
;;               (org-babel-remove-result nil t)

;;               (let ((result-pos (org-babel-where-is-src-block-result)))
;;                 (unless result-pos
;;                   (org-babel-insert-result "")
;;                   (setq result-pos (org-babel-where-is-src-block-result)))
;;                 (goto-char result-pos)
;;                 (next-line))

;;               (insert result))))))))

;;; keybinding
(map! :nvm ";" nil)
(map! :nm "'" #'++windows/ace-pinyin-dwim
      (:prefix ";"
       ;; jinx n, p, w, W
       :desc "next typo" :nvm "n" #'jinx-next
       :desc "previous typo" :nvm "N" #'jinx-previous
       :desc "word correcting" :nvm "w" #'jinx-correct
       :desc "Word confirm" :nvm "w" #'jinx-correct-word
       ;; evil mark
        :desc "set local mark" :nm "m" #'evil-set-marker
        :desc "goto local mark" :nm "j" #'evil-goto-mark
       (:prefix (";" . "code")
        :desc "lsp popup documentation"      :nvm ";" #'lsp-bridge-popup-documentation
        :desc "lsp scroll up in popup buf"   :nm "j"  #'lsp-bridge-popup-documentation-scroll-up
        :desc "lsp scroll down in popup buf" :nm "k"  #'lsp-bridge-popup-documentation-scroll-down )
       ;; :nvm "s" #'evil-snipe-repeat
       (:prefix ("b" . "buffer")
        :desc "save all buffers" :nm "s" #'save-all-buffers
        :desc "jump global mark" :nm "j" #'consult-global-mark)
       (:prefix ("c" . "cache")
        :desc "cleanup posframe cache" :nm "c"  #'(lambda () (interactive)
                                                    (message "do vertico-posframe-cleanup")
                                                    (vertico-posframe-cleanup))
        :desc "prpjectile refresh cache" :nm "p" #'projectile-invalidate-cache)))

(map! :map org-mode-map :desc "execute src block babel async" :nm ";e" #'org-babel-execute-src-block)
(map! :map emacs-lisp-mode-map :desc "eval last emacs sexp" :nm ";e" #'eros-eval-last-sexp)

(message "[config] Apply config-utils")
(provide 'config-utils)