;;; config-utils.el ---                              -*- lexical-binding: t; -*-

;;;
(use-package! string-inflection)

(defun ++utils/word-upcase ()
  (interactive)
  (string-inflection-insert
   (upcase (string-inflection-get-current-word))))

(defun ++utils/word-downcase ()
  (interactive)
  (string-inflection-insert
   (downcase (string-inflection-get-current-word))))

(defun ++utils/vertico-posframe-cleanup ()
  (interactive)
  (message "do vertico-posframe-cleanup")
  (vertico-posframe-cleanup))

;;; util for magit
;; improve diff
(use-package! difftastic
  :commands magit-status ;; 被 magit 触发
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :config
  (define-key difftastic-mode-map "k" 'previous-line)
  (define-key difftastic-mode-map "j" 'next-line)
  (define-key difftastic-mode-map "h" 'backward-char)
  (define-key difftastic-mode-map "l" 'forward-char)
  ;; ">"     #'end-of-buffer
  ;; "<"     #'beginning-of-buffer
  (define-key difftastic-mode-map (kbd "C-d") 'scroll-up)
  (define-key difftastic-mode-map (kbd "C-u") 'scroll-down))

(after! magit-diff
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show " difftastic-magit-show)]))

;;; monkey patch
(use-package! url-http
  :config
  (defun url-http-create-request (&optional ref-url)
    "Create an HTTP request for `url-http-target-url', referred to by REF-URL."
    (let* ((extra-headers)
           (request nil)
           (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
           (using-proxy url-http-proxy)
           (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
                                                url-http-extra-headers))
                               (not using-proxy))
                           nil
                         (let ((url-basic-auth-storage
                                'url-http-proxy-basic-auth-storage))
                           (url-get-authentication url-http-proxy nil 'any nil))))
           (real-fname (url-filename url-http-target-url))
           (host (url-http--encode-string (url-host url-http-target-url)))
           (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
                     nil
                   (url-get-authentication (or
                                            (and (boundp 'proxy-info)
                                                 proxy-info)
                                            url-http-target-url) nil 'any nil))))
      (if (equal "" real-fname)
          (setq real-fname "/"))
      (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
      (if auth
          (setq auth (concat "Authorization: " auth "\r\n")))
      (if proxy-auth
          (setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

      ;; Protection against stupid values in the referrer
      (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
                                             (string= ref-url "")))
          (setq ref-url nil))

      ;; We do not want to expose the referrer if the user is paranoid.
      (if (or (memq url-privacy-level '(low high paranoid))
              (and (listp url-privacy-level)
                   (memq 'lastloc url-privacy-level)))
          (setq ref-url nil))

      ;; url-http-extra-headers contains an assoc-list of
      ;; header/value pairs that we need to put into the request.
      (setq extra-headers (mapconcat
                           (lambda (x)
                             (concat (car x) ": " (cdr x)))
                           url-http-extra-headers "\r\n"))
      (if (not (equal extra-headers ""))
          (setq extra-headers (concat extra-headers "\r\n")))

      ;; This was done with a call to `format'.  Concatenating parts has
      ;; the advantage of keeping the parts of each header together and
      ;; allows us to elide null lines directly, at the cost of making
      ;; the layout less clear.
      (setq request
            (concat
             ;; The request
             (or url-http-method "GET") " "
             (url-http--encode-string
              (if using-proxy (url-recreate-url url-http-target-url) real-fname))
             " HTTP/" url-http-version "\r\n"
             ;; Version of MIME we speak
             "MIME-Version: 1.0\r\n"
             ;; (maybe) Try to keep the connection open
             "Connection: " (if (or using-proxy
                                    (not url-http-attempt-keepalives))
                                "close" "keep-alive") "\r\n"
             ;; HTTP extensions we support
             (if url-extensions-header
                 (format
                  "Extension: %s\r\n" url-extensions-header))
             ;; Who we want to talk to
             (if (/= (url-port url-http-target-url)
                     (url-scheme-get-property
                      (url-type url-http-target-url) 'default-port))
                 (format
                  "Host: %s:%d\r\n" (puny-encode-domain host)
                  (url-port url-http-target-url))
               (format "Host: %s\r\n" (puny-encode-domain host)))
             ;; Who its from
             (if url-personal-mail-address
                 (concat
                  "From: " url-personal-mail-address "\r\n"))
             ;; Encodings we understand
             (if (or url-mime-encoding-string
                     ;; MS-Windows loads zlib dynamically, so recheck
                     ;; in case they made it available since
                     ;; initialization in url-vars.el.
                     (and (eq 'system-type 'windows-nt)
                          (fboundp 'zlib-available-p)
                          (zlib-available-p)
                          (setq url-mime-encoding-string "gzip")))
                 (concat
                  "Accept-encoding: " url-mime-encoding-string "\r\n"))
             (if url-mime-charset-string
                 (concat
                  "Accept-charset: "
                  (url-http--encode-string url-mime-charset-string)
                  "\r\n"))
             ;; Languages we understand
             (if url-mime-language-string
                 (concat
                  "Accept-language: " url-mime-language-string "\r\n"))
             ;; Types we understand
             "Accept: " (or url-mime-accept-string "*/*") "\r\n"
             ;; User agent
             (url-http-user-agent-string)
             ;; Proxy Authorization
             proxy-auth
             ;; Authorization
             auth
             ;; Cookies
             (when (url-use-cookies url-http-target-url)
               (url-http--encode-string
                (url-cookie-generate-header-lines
                 host real-fname
                 (equal "https" (url-type url-http-target-url)))))
             ;; If-modified-since
             (if (and (not no-cache)
                      (member url-http-method '("GET" nil)))
                 (let ((tm (url-is-cached url-http-target-url)))
                   (if tm
                       (concat "If-modified-since: "
                               (url-get-normalized-date tm) "\r\n"))))
             ;; Whence we came
             (if ref-url (concat
                          "Referer: " ref-url "\r\n"))
             extra-headers
             ;; Length of data
             (if url-http-data
                 (concat
                  "Content-length: " (number-to-string
                                      (length url-http-data))
                  "\r\n"))
             ;; End request
             "\r\n"
             ;; Any data
             url-http-data))
      ;; Bug#23750
      (setq request (url-http--encode-string request))
      (unless (= (string-bytes request)
                 (length request))
        (error "Multibyte text in HTTP request: %s" request))
      (url-http-debug "Request is: \n%s" request)
      request))
  )

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

(defvar ++utils/source-code-dir)

(defun ++utils/refer-source-code-md-link ()
  (interactive)
  (let ((relative-path (file-relative-name (buffer-file-name) ++utils/source-code-dir))
        (file-name (file-name-nondirectory (buffer-file-name)))
        (project (project-name (project-current)))
        (select-enable-clipboard t))
    (kill-new
     (if (use-region-p)
         (let ((lang (downcase (++utils/language-at-pos)))
               (selection (buffer-substring-no-properties (region-beginning) (region-end))))
           (format "[☕️ %s: %s](org-protocol://view-source-code?location=codes&filepath=%s&region=%s:%s)\n```%s\n%s```"
                   project file-name relative-path (region-beginning) (- (region-end) (region-beginning)) lang selection))
       (format "[☕️ %s: %s](org-protocol://view-source-code?location=codes&filepath=%s&pos=%s)"
              project file-name relative-path (point))))))

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

;; (global-set-key (kbd "C-;") #'consult-yank-from-kill-ring)
(global-set-key (kbd "C-'") #'embark-act)

(map! (:prefix ("C-;" . "my leader key")
       :desc "emacs yank history" :nm ";" #'consult-yank-from-kill-ring

       ;; evil mark
       ;; :desc "local mark to ?" :nm "l" #'evil-set-marker
       ;; :desc "goto local mark ?" :nm "j" #'evil-goto-mark
       :desc "switch global mark" :nm "'" #'consult-global-mark
       :desc "switch global buffer" :nm "." #'consult-buffer
       :desc "switch workspace buffer" :nm "," #'+vertico/switch-workspace-buffer

       (:prefix ("f" . "format symbol")
        ;; string inflection
        :desc "format CamelCase" "C" #'string-inflection-camelcase
        :desc "format camelCase" "c" #'string-inflection-lower-camelcase
        :desc "format snake_case" "s" #'string-inflection-underscore
        :desc "format SNAKE_CASE" "S" #'string-inflection-upcase ;; string-inflection-capital-underscore
        :desc "format kebab-case" "k" #'string-inflection-kebab-case
        :desc "format UPCASE" "d" #'++utils/word-downcase
        :desc "format downcase" "u" #'++utils/word-upcase)

       (:prefix ("h" . "highlight symbol")
        :desc "highlight at point" :nvm "h" #'symbol-overlay-put
        :desc "toggle show highlight scope or buffer" :nvm "t" #'symbol-overlay-toggle-in-scope
        :desc "echo mark" :nvm "e" #'symbol-overlay-echo-mark
        :desc "search highlight" :nvm "s" #'symbol-overlay-isearch-literally
        :desc "quit all highlight" :nvm "q" #'symbol-overlay-remove-all)

       (:prefix ("i" . "ai")
        ;; ### 对话模式
        ;; 1. 新建 `test.chat` 文件， 将自动进入 `mind-wave-chat-mode`
        ;; 2. 执行命令 `mind-wave-chat-ask`（按下 Ctrl + j）， 输入问题， 等待 ChatGPT 回答
        :desc "ask" :nvm "ai" #'mind-wave-chat-ask
        ;; 3. 执行命令 `mind-wave-chat-continue` (按下 Ctrl + u), 继续回答
        :desc "continue" :nvm "ac" #'mind-wave-chat-continue
        ;; 4. 执行命令 `mind-wave-chat-generate-title` (按下 Ctrl + i), 根据内容重新生成标题
        ;; 如果您想更换话题， 请新建一个新的 `*.chat` 文件， 然后继续向 ChatGPT 提问即可。
        ;; ### 多行输入
        ;; 多行输入有两种方式：
        ;; 1. 执行命令 `mind-wave-chat-ask-with-multiline`（按下 Ctrl + Shift + j）， 输入多行问题， 等待 ChatGPT 回答
        ;; 2. 执行命令 `mind-wave-chat-ask-insert-line` 插入 `----- User ------` 分隔符， 在 Buffer 继续输入多行内容， 最后执行 `mind-wave-chat-ask-send-buffer`
        ;; ### 文档模式
        ;; 选中内容（请注意， 不要选择太多， ChatGPT 的 API 有大小限制）

        ;; 1. 执行命令 `mind-wave-translate-to-english`， ChatGPT 获得翻译后会自动替换选中区域的内容。
        :desc "translate to English" :nvm "t" #'mind-wave-translate-to-english
        ;; 2. 执行命令 `mind-wave-proofreading-doc`， ChatGPT 会用润色后的文档自动替换选中区域的内容。
        :desc "proofreading for English" :nvm "p" #'mind-wave-proofreading-doc
        ;; 3. 执行命令 `mind-wave-explain-word`, ChatGPT 会自动解释当前句子中单词的意思， 并给出类似例句。
        :desc "word explaination" :nvm "w" #'mind-wave-explain-word
        ;; 4. 执行命令 `mind-wave-adjust-text`, ChatGPT 根据你的指令来调整文字或代码
        :desc "adjust" :nvm "j" #'mind-wave-adjust-text
        ;; 5. 执行命令 `mind-wave-check-typos`, ChatGPT 修复错别字
        ;; :desc "check typos" :nvm "c" #'mind-wave-check-typos
        ;; ### 代码模式
        ;; 光标移动到想要重构的函数

        ;; 代码相关命令会自动调整窗口布局， 你随时可以用 `mind-wave-restore-window-configuration` 恢复之前的窗口布局。
        ;; 1. 执行命令 `mind-wave-generate-code`, ChatGPT 会根据提示， 在当前 buffer 输出代码
        :desc "insert code" :nvm "i" #'++mind-wave/generate-code
        ;; 2. 执行命令 `mind-wave-refactory-code`, ChatGPT 会自动分屏， 在屏幕右边先后重构后的代码和重构建议
        :desc "refactor" :nvm "r" #'mind-wave-refactory-code
        ;; 3. 执行命令 `mind-wave-comment-code`, ChatGPT 会自动分屏， 在屏幕右边显示带注释的代码
        :desc "comment" :nvm "m" #'mind-wave-comment-code
        ;; 4. 执行命令 `mind-wave-explain-code`, ChatGPT 会自动分屏， 在屏幕右边显示代码的讲解
        :desc "codes explain" :nvm "c" #'mind-wave-explain-code
        :desc "codes explain" :nvm "C" #'++mind-wave/mind-wave-explain-code-with-input
        ;; 5. 执行命令 `mind-wave-explain-point`, ChatGPT 会自动分屏， 在屏幕右边显示光标处 API 的讲解
        :desc "explain api at point" :nvm ";" #'mind-wave-explain-point
        ;; 6. 执行命令 `mind-wave-generate-commit-name`, ChatGPT 会自动分析当前的 diff 内容， 并生成一个补丁名称

        ;; 7. 执行命令 `mind-wave-refactory-code-with-input`, ChatGPT 会自动分屏， 根据你的提示， 在屏幕右边先后重构后的代码和重构建议
        :desc "refactor with prompt" :nvm "R" #'mind-wave-refactory-code-with-input

        ;; ### 摘要模式
        ;; 1. 打开视频网站, 执行命令 `mind-wave-summary-video`, ChatGPT 会自动获取视频字幕， 并分析视频概要 (需要安装 `youtube_transcript_api`)
        ;; 2. 打开文本网站, 执行命令 `mind-wave-summary-web`, ChatGPT 会自动获取网页中的核心内容， 并分析网页概要 (需要安装 `nodejs-readability-cli`)
        )
      ))

      ;; (:leader
      ;; (:prefix (";" . "quick utils")
      ;;  :desc "translation word or region" :nvm "w" #'++lookup/bob-translation
      ;;  :desc "search online" :nvm "s" #'+lookup/online-select
      ;;  :desc "note refer here" :nvm "n" #'(lambda () (interactive)
      ;;                                 (++utils/refer-source-code-md-link)
      ;;                                 (shell-command "open -a /Applications/Obsidian.app"))
      ;;  ;; jinx
      ;;  (:prefix ("t" . "typo")
      ;;   :desc "next typo" :nvm "n" #'jinx-next
      ;;   :desc "previous typo" :nvm "N" #'jinx-previous
      ;;   :desc "word correcting" :nvm ";" #'jinx-correct
      ;;   :desc "Word confirm" :nvm ":" #'jinx-correct-word)
      ;;  (:prefix ("c" . "code")
      ;;   :desc "lsp popup documentation"      :nvm "c" #'lsp-bridge-popup-documentation
      ;;   :desc "lsp scroll up in popup buf"   :nm "j"  #'lsp-bridge-popup-documentation-scroll-up
      ;;   :desc "lsp scroll down in popup buf" :nm "k"  #'lsp-bridge-popup-documentation-scroll-down )
      ;;  (:prefix ("," . "edit")
      ;;   :desc "format indent" :nvm "i" #'indent-for-tab-command
      ;;   :desc "\\n remove" :nvm "w" #'++utils/replace-region-newline-to-space
      ;;   :desc "quite fake cursor" :nvm "q" #'(lambda () (interactive) (mc/remove-fake-cursors)))
      ;;  ;; :nvm "s" #'evil-snipe-repeat
      ;;  (:prefix ("m" . "make comment")
      ;;   :nm "t" #'hl-todo-insert
      ;;   :nm "s" #'comment-set-column
      ;;   :nm "d" #'comment-dwim
      ;;   :nm "m" #'comment-indent)
      ;;  (:prefix ("b" . "buffer")
      ;;   :desc "save all buffers" :nm "s" #'save-all-buffers

;; (map! :map org-mode-map :leader :desc "execute src block babel async" :nm ";e" #'org-babel-execute-src-block)
;; (map! :map emacs-lisp-mode-map :leader :desc "eval last emacs sexp" :nm ";e" #'eros-eval-last-sexp)

;; code playgroud
(use-package! go-playground
  :config
  (advice-add 'go-playground :after #'go-ts-mode))

(map! :localleader
      :map go-playground-mode-map
      "p" #'go-playground-exec)

(defun ++utils/go-playground ()
  (interactive)
  (let ((file "~/go/src/playground/main/main.go"))
    (find-file file)
    (go-mode)
    (go-playground-mode)))

(defun ++utils/tempel-format-region (start end)
  (interactive "r") ; 'r' means this command works on a region, 'interactive' makes it callable with M-x
  (let* ((region-text (buffer-substring-no-properties start end))
         (replaced-text (concat "\"" (replace-regexp-in-string "\n" "\" n\n\"" region-text) "\"")))
    (delete-region start end)
    (insert replaced-text)))

(map!
;;; keybinding
;; "(2-char) '(1-char) snipe across line
;; s S (2-char) f F (1-char) snipe inline
;; ' make line
 :nm ";" #'++windows/ace-pinyin-dwim
 :nm "\"" #'ace-pinyin-jump-char-2

 (:prefix "M-<escape>"
          "TAB" #'sort-tab-close-other-tabs
          "ESC" #'keyboard-escape-quit
          "v" #'++utils/vertico-posframe-cleanup
          "p" #'projectile-invalidate-cache
          ":" #'eval-expression)

 :leader
 :nm "ig" #'gitmoji-insert-emoji
 :nm "is" #'tempel-insert
 :nm "iS" #'consult-yasnippet)

(message "[config] Apply config-utils")
(provide 'config-utils)