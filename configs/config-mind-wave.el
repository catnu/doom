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
         ;; (prompt (format "只用高可读性的%s代码且不带任何解释和说明回答: %s"
         (prompt (format "Give me the pure %s code for %s. No explanations, comments, just the code without codeblock format."
                         lang
                         (or selection
                             (read-string "Prompt: "
                                          (if (string= "" lang) "within ?language" nil)))
                         )))
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

(defun ++mind-wave/mind-wave-explain-code-with-input ()
  (interactive)
  (message "Explaining...")
  (mind-wave-call-async "action_code"
                        (buffer-name)
                        (format "%s" major-mode)
                        (mind-wave--encode-string (nth 2 (mind-wave-get-region-or-function)))
                        mind-wave-code-role
                        (format "Please explain in detail the meaning of the following code, in %s, and answer %s, leave a blank line between each sentence." (mind-wave-output-lang)
                                (read-string "Ask for: "))
                        "explain"
                        "ChatGPT explaining..."
                        "ChatGPT explain finish."))

(message "[config] Apply config-mind-wave")
(provide 'config-mind-wave)