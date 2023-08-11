;;; configs/config-lookup.el -*- lexical-binding: t; -*-

;; use translation habitica with minibrief
(require 'config-minibrief)

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
(defun ++lookup/url-action (url)
  (pcase url
    ((rx "lookup://:translate:/" (let query (0+ anything)))
     (funcall-interactively #'minibrief/shwo-translation
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

(defun ++lookup/search-online (query provider)
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
               (++lookup/url-action
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
(defun ++lookup/use-search-online ()
  (fset '+lookup/online #'++lookup/search-online)
  (message "use ++lookup/search-online")
  (map! :leader :desc "Look up online" "sO" #'+lookup/online)
  (advice-remove '+lookup/online-select #'++lookup/use-search-online))
(advice-add '+lookup/online-select :before #'++lookup/use-search-online)

(map! :leader
      ;; :desc "Translation" "st" #'minibrief/shwo-translation
      :desc "Bref buffer Helper" "hh" #'minibrief/pop-toggle
      :desc "Bref buffer Habitica" "hH" #'minibrief/show-habitica-character
      :desc "Look up online (w/ prompt)" "so" #'+lookup/online-select
      "sO" nil)

(message "[config] Apply config-lookup")
(provide 'config-lookup)