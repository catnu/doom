;;; configs/config-lookup.el -*- lexical-binding: t; -*-

(add-to-list '+lookup-provider-url-alist
             '("Dash Docset" "dash://:docset:/%s"))
(add-to-list '+lookup-provider-url-alist
             '("Dash" "dash://%s"))
(defun config-lookup/url-action (url)
  (pcase url
      ((rx "dash://:docset:/")
       (funcall-interactively #'dash-at-point-with-docset))
      ((rx "dash://")
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
                (if (string-match-p (rx "dash://") backend)
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
  (advice-remove '+lookup/online-select #'config-lookup/use-search-online))
(advice-add '+lookup/online-select :before #'config-lookup/use-search-online)

;; more query
(add-to-list '+lookup-provider-url-alist
             '("Emacs China" "https://emacs-china.org/search?expanded=true&q=%s"))

(message "[config] Apply config-lookup")
(provide 'config-lookup)