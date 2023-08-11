;;; configs/config-lookup.el -*- lexical-binding: t; -*-

;;; more query
(add-to-list '+lookup-provider-url-alist
             '("Emacs China" "https://emacs-china.org/search?expanded=true&q=%s"))
;;; add Dash action
(add-to-list '+lookup-provider-url-alist
             '("Dash Docset" "lookup://:docset:/%s"))
(add-to-list '+lookup-provider-url-alist
             '("Dash Snippets" "lookup://:d-snippet:/%s"))
(add-to-list '+lookup-provider-url-alist
             '("Dash" "lookup://:dash:/%s"))
(add-to-list '+lookup-provider-url-alist
             '("Google Translate" "lookup://:translate:/%s"))
;; handle url
(defun config-lookup/url-action (url)
  (pcase url
    ((rx "lookup://:translate:/" (let query (0+ anything)))
     (funcall-interactively #'config-lookup/explanation-brief
                            (if (eq query "")
                                (or (thing-at-point 'word)
                                    (translate-shell--read-string))
                              query)))
    ((rx "lookup://:dash:/")
     (call-interactively #'dash-at-point))
    ((rx "lookup://:docset:/")
     (funcall-interactively #'dash-at-point-with-docset))
    ((rx "lookup://:d-snippet:/")
     (funcall-interactively #'config/dash-at-point-only-snippets t))
    (_ (funcall +lookup-open-url-fn url))))

(use-package dash-at-point
  :config
  ;; (add-to-list 'dash-at-point-docsets "Snippets Only")
  ;;;###autoload
  (defun config/dash-at-point-only-snippets (&optional edit-search)
    (interactive "P")
    (let* ((thing (thing-at-point 'symbol))
           (search (if (or edit-search (null thing))
                       (read-from-minibuffer
                        (concat "Dash search (Snipptes Only): ") thing))))
      (dash-at-point-run-search search "Snippets Only"))))

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
                    (format backend (or query ""))
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
(defvar config-lookup/brief--buffer-alist '(" *explanation-brief-buffer*" " *habitica-brief-buffer"))
(defvar config-lookup/brief--buffer-show nil)

;; (defun config/explation-buf ()
;;   (car config-lookup/brief--buffer-alist))

;; (defun config/habitica-buf ()
;;   (cadr config-lookup/brief--buffer-alist))

(defun config-lookup/brief-pop (buf-or-name)
  (when (posframe-workable-p)
    (and config-lookup/brief--buffer-show
         (posframe-hide config-lookup/brief--buffer-show))
    (setq config-lookup/brief--buffer-show buf-or-name)
    (posframe-show buf-or-name
                   ;; :poshandler 'posframe-poshandler-frame-top-right-corner
                   :poshandler #'(lambda (_info) '(-25 . 25))
                   ;; :background-color "green"
                   :foreground-color "orange"
                   :border-width 1
                   :border-color "orange"
                   :max-height 37
                   :max-width 50
                   :left-fringe 18
                   :right-fringe 18)))

(defun config-lookup/brief-hide ()
  (posframe-hide config-lookup/brief--buffer-show)
  (setq config-lookup/brief--buffer-show nil))

(defun config-lookup/brief-buffer-pop-toggle ()
  (interactive)
  (pcase-let ((`(,explanation-buf ,habitica-buf) config-lookup/brief--buffer-alist))
    (cond
     ((null config-lookup/brief--buffer-show)
      (config-lookup/brief-pop
       (if (string= "Habitica" (completing-read "Show Which: " '("Translation" "Habitica" )))
           habitica-buf
         explanation-buf)))
     ((string= explanation-buf config-lookup/brief--buffer-show)
      (if (string= "Habitica" (completing-read "Show Which: " '("Close" "Habitica")))
          (config-lookup/brief-pop habitica-buf)
        (config-lookup/brief-hide)))
     ((string= habitica-buf config-lookup/brief--buffer-show)
      (let ((select (completing-read "Show Which: " '("Close" "Translation" "Habitica Profile" ))))
        (cond
         ((string= "Close" select) (config-lookup/brief-hide))
         ((string= "Translation" select) (config-lookup/brief-pop explanation-buf))
         (t (config-lookup/profile-button-pressed nil))))))))

(defun config-lookup/translate-shell-mix (word)
  (let ((result
         (concat
          (replace-regexp-in-string "\n" "; "
                                    (shell-command-to-string
                                     (format translate-shell-brief-command
                                             (shell-quote-argument word))))
          "\n\n" (shell-command-to-string
                (format translate-shell-command (shell-quote-argument word))))))
    (add-to-list 'translate-shell-brief-cache (cons (intern word) result))
    result))

(defun config-lookup/explanation-brief (&optional word)
  "Show the explanation of WORD in the echo area."
  (interactive
   (list (translate-shell--read-string)))
  (message "google translating...")
  (let* ((buf (car config-lookup/brief--buffer-alist))
         (word-sym (intern word))
         (result (if (assq word-sym translate-shell-brief-cache)
                     (assoc-default word-sym translate-shell-brief-cache)
                   (config-lookup/translate-shell-mix word))))
    (message "google translated")
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (insert "\n")
      (insert result)
      (insert "\n"))
    (config-lookup/brief-pop buf)))

(defun config-lookup/profile-button-pressed (_button)
  ;; (funcall +lookup-open-url-fn private/habitica-profile-url)
  (funcall +lookup-open-url-fn "https://habitica.com/"))

(define-button-type 'config-lookup/profile-button
  'action 'config-lookup/profile-button-pressed
  'follow-link t
  'help-echo "Click Button"
  'help-args "test")

(defun config-lookup/brief-buffer-habitica-character ()
  (interactive)
  (if (or (not habitica-uid) (not habitica-token))
      (call-interactively 'habitica-login))
  (let ((buf (cadr config-lookup/brief--buffer-alist)))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (insert "\n")
      (habitica--parse-profile (assoc-default 'stats (habitica-api-get-profile)) nil)
      (insert "** Profile")
      (make-button (- (point) 10) (point) :type 'config-lookup/profile-button)
      (insert "\n\n"))
    (config-lookup/brief-pop buf)))

(map! :leader
      ;; :desc "Translation" "st" #'config-lookup/explanation-brief
      :desc "Bref buffer Helper" "hh" #'config-lookup/brief-buffer-pop-toggle
      :desc "Bref buffer Habitica" "hH" #'config-lookup/brief-buffer-habitica-character
      :desc "Look up online (w/ prompt)" "so" #'+lookup/online-select
      "sO" nil)

(message "[config] Apply config-lookup")
(provide 'config-lookup)