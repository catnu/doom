;;; configs/config-jinx.el -*- lexical-binding: t; -*-

(defvar-local jinx--mutex (make-mutex "jinx--mutex")
  "Mutex for thread safe access to jinx-mod dynamic module.")

(defun jinx--load-dicts-thread ()
  "Thread runner for `jinx--load-dicts' to load the actual dictionaries."
  (mapcar #'jinx--mod-dict (split-string jinx-languages)))

(use-package jinx
  :config
  (setq jinx-languages "en_US")
  (setq jinx-camel-modes '(prog-mode org-mode))
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  ;; fix https://github.com/minad/jinx/pull/91
  (jinx--load-module)
  (make-thread 'jinx--load-dicts-thread "jinx--load-dicts-thread"))

;; Alternative 2: Enable Jinx per mode
(dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook hook #'jinx-mode))

(message "[config] Apply config-jinx")
(provide 'config-jinx)

;; Local Variables:
;; jinx-local-words: "Mutex"
;; End: