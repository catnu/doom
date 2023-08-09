;;; configs/config-lookup.el -*- lexical-binding: t; -*-

;;; more query
(add-to-list '+lookup-provider-url-alist
             '("Emacs China" "https://emacs-china.org/search?expanded=true&q=%s"))
;;; add Dash action
(add-to-list '+lookup-provider-url-alist
             '("Dash Docset" "lookup://:docset:/%s"))
(add-to-list '+lookup-provider-url-alist
             '("Dash" "lookup://:dash:/%s"))
(add-to-list '+lookup-provider-url-alist
             '("Google Translate" "lookup://:translate:/%s"))

(defun config-lookup/url-action (url)
  (pcase url
      ((rx "lookup://:translate:/")
       (call-interactively #'config-lookup/explanation-brief))
      ((rx "lookup://:docset:/")
       (funcall-interactively #'dash-at-point-with-docset))
      ((rx "lookup://:dash:/")
       (funcall-interactively #'dash-at-point))
      (_ (funcall +lookup-open-url-fn url))))

(defun config-lookup/search-online (query provider)
  "Look up QUERY in the browser using PROVIDER.
When called interactively, prompt for a query and, when called for the first
time, the provider from `+lookup-provider-url-alist'. In subsequent calls, reuse
the previous provider. With a non-nil prefix argument, always prompt for the
provider.

QUERY must be a string, and PROVIDER must be a key of
`+lookup-provider-url-alist'."
  (interactive
   (list (if (use-region-p) (doom-thing-at-point-or-region))
         (+lookup--online-provider current-prefix-arg)))
  (let ((backends (cdr (assoc provider +lookup-provider-url-alist))))
    (unless backends
      (user-error "No available online lookup backend for %S provider"
                  provider))
    (catch 'done
      (dolist (backend backends)
        (cl-check-type backend (or string function))
        (cond ((stringp backend)
               (config-lookup/url-action
                (if (string-match-p (rx "lookup://") backend)
                    backend
                  (format backend
                          (url-encode-url
                           (read-string (format "Search for (on %s): " provider)
                                        query))))))
              ((condition-case-unless-debug e
                   (and (fboundp backend)
                        (funcall backend query))
                 (error
                  (delq! major-mode +lookup--last-provider 'assq)
                  (signal (car e) (cdr e))))
               (throw 'done t)))))))

;; patch
(defun config-lookup/use-search-online ()
  (fset '+lookup/online #'config-lookup/search-online)
  (message "use config-lookup/search-online")
  (map! :leader :desc "Look up online" "sO" #'+lookup/online)
  (advice-remove '+lookup/online-select #'config-lookup/use-search-online))
(advice-add '+lookup/online-select :before #'config-lookup/use-search-online)

;; need https://github.com/soimort/translate-shell
(use-package translate-shell)
  ;; :config
  ;; <https://translate.google.com> is blocked in China for no apparent
  ;; reason. No one ever asked my option.
  ;; for mac you need proxychains-ng installed
  ;; (setq translate-shell-command "proxychains4 -q trans -t en+zh %s"
  ;;       translate-shell-brief-command "proxychains4 -q trans -brief -t zh+en %s"))

;; Dictionary
(defvar config-lookup/brief--buffer " *explanation-brief-buffer*")
(defvar config-lookup/brief-buffer--show nil)

(defun config-lookup/explanation-brief-pop ()
  (when (posframe-workable-p)
    (setq config-lookup/brief-buffer--show t)
    (posframe-show config-lookup/brief--buffer
                   ;; :poshandler 'posframe-poshandler-frame-top-right-corner
                   :poshandler #'(lambda (_info) '(-25 . 25))
                   ;; :background-color "green"
                   :foreground-color "orange"
                   :border-width 1
                   :border-color "orange"
                   :left-fringe 18
                   :right-fringe 18)))

(defun config-lookup/brief-buffer-pop-toggle () (interactive)
       (cond (config-lookup/brief-buffer--show
              (setq config-lookup/brief-buffer--show nil)
              (posframe-hide config-lookup/brief--buffer))
             (t (config-lookup/explanation-brief-pop))))

(defun config-lookup/explanation-brief (&optional word)
  "Show the explanation of WORD in the echo area."
  (interactive
   (list (translate-shell--read-string)))
  (message "google translating...")
  (let* ((word-sym (intern word))
         (result (if (assq word-sym translate-shell-brief-cache)
                     (assoc-default word-sym translate-shell-brief-cache)
                   (shell-command-to-string
                    (format translate-shell-brief-command (shell-quote-argument word))))))
    (message "google translated")
    (with-current-buffer (get-buffer-create config-lookup/brief--buffer)
      (erase-buffer)
      (insert "\n")
      (insert result)
      (insert "\n"))
    (config-lookup/explanation-brief-pop)))

(defun config-lookup/brief-buffer-habitica-character ()
  (interactive)
  (if (or (not habitica-uid) (not habitica-token))
      (call-interactively 'habitica-login))
  (with-current-buffer (get-buffer-create config-lookup/brief--buffer)
    (erase-buffer)
    (insert "\n")
    (habitica--parse-profile (assoc-default 'stats (habitica-api-get-profile)) nil)
    (insert "\n"))
  (config-lookup/explanation-brief-pop))

(map! :leader
      ;; :desc "Translation" "st" #'config-lookup/explanation-brief
      :desc "Bref buffer Helper" "sh" #'config-lookup/brief-buffer-pop-toggle
      :desc "Bref buffer Habitica" "sH" #'config-lookup/brief-buffer-habitica-character
      :desc "Look up online (w/ prompt)" "so" #'+lookup/online-select
      "sO" nil)

(message "[config] Apply config-lookup")
(provide 'config-lookup)