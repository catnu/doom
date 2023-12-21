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
(defun mc/remove-fake-cursors ()
  "lazyload multiple-cursors"
  (require 'multiple-cursors))

(defun ++utils/replace-region-newline-to-space ()
  (interactive)
  (and (use-region-p)
       (let* ((begin (region-beginning))
              (end (region-end))
              (r-min (if (< begin end) begin end))
              (r-max (- (if (> begin end) begin end) 1))) ;; exclude last newline
         (deactivate-mark)
         (mc/remove-fake-cursors)
         (goto-char r-min)
         (while (search-forward-regexp "[^\n]\n[^\n]" r-max t)
           (replace-match
            (string-replace "\n" " " (string-replace "-\n" "" (match-string 0)))
            nil t)
           (backward-char)
           (mc/create-fake-cursor-at-point))
         (goto-char r-min))))

(use-package! symbol-overlay
  :config
  ;; override
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'symbol-overlay-jump-prev)
    (define-key map (kbd "N") #'symbol-overlay-jump-prev)
    (define-key map (kbd "n") #'symbol-overlay-jump-next)
    (define-key map (kbd "<") #'symbol-overlay-jump-first)
    (define-key map (kbd ">") #'symbol-overlay-jump-last)
    (define-key map (kbd "r") #'symbol-overlay-rename)
    (define-key map (kbd "R") #'symbol-overlay-query-replace)
    (define-key map (kbd "C-e") #'symbol-overlay-echo-mark)
    (define-key map (kbd "C-s") #'symbol-overlay-isearch-literally)
    (define-key map (kbd "C-h") #'symbol-overlay-map-help)
    (define-key map (kbd "C-g") #'symbol-overlay-remove-all)
    (setq symbol-overlay-map map)))

;;; keybinding
;; "(2-char) '(1-char) snipe across line
;; s S (2-char) f F (1-char) snipe inline
(map! :nvm ";" nil)
(map! :nm "'" #'++windows/ace-pinyin-dwim
      :nm "\"" #'ace-pinyin-jump-char-2
      (:prefix ";"
       :desc "translation word or region" :nvm "w" #'++lookup/bob-translation
       :desc "search online" :nvm "s" #'+lookup/online-select
       ;; jinx
       :desc "next typo" :nvm "n" #'jinx-next
       :desc "previous typo" :nvm "N" #'jinx-previous
       :desc "word correcting" :nvm ";" #'jinx-correct
       :desc "Word confirm" :nvm ":" #'jinx-correct-word
       ;; evil mark
        :desc "local mark to ?" :nm "l" #'evil-set-marker
        :desc "goto local mark ?" :nm "j" #'evil-goto-mark
       (:prefix ("h" . "highlight symbol")
        :desc "highlight at point" :nvm "h" #'symbol-overlay-put
        :desc "toggle show highlight scope or buffer" :nvm "t" #'symbol-overlay-toggle-in-scope
        :desc "echo mark" :nvm "e" #'symbol-overlay-echo-mark
        :desc "search highlight" :nvm "s" #'symbol-overlay-isearch-literally
        :desc "quit all highlight" :nvm "q" #'symbol-overlay-remove-all)
       (:prefix ("c" . "code")
        :desc "lsp popup documentation"      :nvm "c" #'lsp-bridge-popup-documentation
        :desc "lsp scroll up in popup buf"   :nm "j"  #'lsp-bridge-popup-documentation-scroll-up
        :desc "lsp scroll down in popup buf" :nm "k"  #'lsp-bridge-popup-documentation-scroll-down )
       (:prefix ("f" . "format")
        :desc "format indent" :nvm "f" #'indent-for-tab-command
        :desc "wrap remove" :nvm "w" #'++utils/replace-region-newline-to-space
        :desc "quite fake cursor" :nvm "q" #'(lambda () (interactive) (mc/remove-fake-cursors)))
       ;; :nvm "s" #'evil-snipe-repeat
       (:prefix ("m" . "make comment")
        :nm "t" #'hl-todo-insert
        :nm "s" #'comment-set-column
        :nm "m" #'comment-indent)
       (:prefix ("b" . "buffer")
        :desc "save all buffers" :nm "s" #'save-all-buffers
        :desc "jump global mark" :nm "j" #'consult-global-mark)
       (:prefix ("u" . "cache")
        :desc "verticon posframe cache cleanup " :nm "v"  #'(lambda () (interactive)
                                                    (message "do vertico-posframe-cleanup")
                                                    (vertico-posframe-cleanup))
        :desc "prpjectile refresh cache" :nm "p" #'projectile-invalidate-cache)))

(map! :leader :nm "ig" #'gitmoji-insert-emoji)

(map! :map org-mode-map :desc "execute src block babel async" :nm ";e" #'org-babel-execute-src-block)
(map! :map emacs-lisp-mode-map :desc "eval last emacs sexp" :nm ";e" #'eros-eval-last-sexp)

(message "[config] Apply config-utils")
(provide 'config-utils)