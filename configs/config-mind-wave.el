;;; configs/config-mind-wave.el -*- lexical-binding: t; -*-
;; (setq mind-wave-async-text-model "gpt-4")
(require 'mind-wave)

(defun ++mind-wave/before-chat ()
  (unless (string-suffix-p ".chat" (buffer-name))
    (find-file-other-window (expand-file-name "test.chat" "~/tmp/chats"))))

(advice-add 'mind-wave-chat-ask :before #'++mind-wave/before-chat)
(advice-add 'mind-wave-chat-continue :before #'++mind-wave/before-chat)

(defun ++mind-wave/region-comment-text (begin end)
  (interactive)
  (let ((comment-begin
         (save-excursion
           (goto-char begin)
           (end-of-line)
           (if (comment-search-backward begin t)
               (progn
                 (forward-word)
                 (backward-word)
                 (point))
             nil))))
    (buffer-substring-no-properties
     (or comment-begin begin) end)))

(defun ++mind-wave/generate-code ()
  (interactive)
  (let* (
         (selection (if (region-active-p)
                        (string-trim (++mind-wave/region-comment-text (region-beginning) (region-end))) nil))
         (lang (++utils/language-at-pos))
         (lang (if lang (format " %s " lang) "pseudocode"))
         (prompt (format "只用高可读性的%s代码且不带任何解释和说明回答: %s"
                         lang
                         (or selection
                             (read-string "Prompt: "
                                          (if (string= "" lang) "within ?language" nil))))))
    (end-of-line)
    (insert "\n")
    (message "Ask: %s" prompt)
    (mind-wave-call-async "async_text"
                          (buffer-file-name)
                          (mind-wave--encode-string "")
                          (point)
                          (point)
                          mind-wave-code-role
                          prompt
                          "Generate..."
                          "Generate code done.")))

(map! (:prefix (";i" . "ai")
       ;; ### 对话模式
       ;; 1. 新建 `test.chat` 文件， 将自动进入 `mind-wave-chat-mode`
       ;; 2. 执行命令 `mind-wave-chat-ask`（按下 Ctrl + j）， 输入问题， 等待 ChatGPT 回答
       :desc "ask" :nvm "aa" #'mind-wave-chat-ask
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
       ;; 5. 执行命令 `mind-wave-explain-point`, ChatGPT 会自动分屏， 在屏幕右边显示光标处 API 的讲解
       :desc "explain api at point" :nvm ";" #'mind-wave-explain-point
       ;; 6. 执行命令 `mind-wave-generate-commit-name`, ChatGPT 会自动分析当前的 diff 内容， 并生成一个补丁名称
       ;; :desc "代码模式: generate commit name" :nvm "gc" #'mind-wave-generate-commit-name
       ;; 7. 执行命令 `mind-wave-refactory-code-with-input`, ChatGPT 会自动分屏， 根据你的提示， 在屏幕右边先后重构后的代码和重构建议
       :desc "refactor with prompt" :nvm "R" #'mind-wave-refactory-code-with-input

       ;; ### 摘要模式
       ;; 1. 打开视频网站, 执行命令 `mind-wave-summary-video`, ChatGPT 会自动获取视频字幕， 并分析视频概要 (需要安装 `youtube_transcript_api`)
       ;; 2. 打开文本网站, 执行命令 `mind-wave-summary-web`, ChatGPT 会自动获取网页中的核心内容， 并分析网页概要 (需要安装 `nodejs-readability-cli`)
       ))

(message "[config] Apply config-mind-wave")
(provide 'config-mind-wave)