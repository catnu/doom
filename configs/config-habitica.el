;;; configs/config-habitica.el -*- lexical-binding: t; -*-

(setq-default org-tag-persistent-alist
              '(("trivial")
                ("easy")
                ("medium")
                ("hard")
                ("habit")
                ("daily")
                ("todo")
                ("reward")))

;; (defun org-habitica-unload-feature ()
;;   "Unload org-habitica."
;;   (interactive)
;;   (global-unset-key (kbd "C-c u"))
;;   (global-unset-key (kbd "C-c C-x h"))
;;   ;; (remove-hook 'org-after-todo-state-change-hook 'org-habitica-sync-task)
;;   (unload-feature 'org-habitica)
;;   (message "feature org-habitica unloaded"))

;; (defun org-habitica-load-feature ()
;;   "Load org-habitica."
;;   (interactive)
;;   (require 'org-habitica)
;;   (global-set-key (kbd "C-c u") 'org-habitica-unload-feature)
;;   (global-set-key (kbd "C-c C-x h") 'org-habitica-sync-task)
;;   ;; (add-hook 'org-after-todo-state-change-hook 'org-habitica-sync-task)
;;   (org-habitica-check-access))
;; (global-set-key (kbd "C-c h") 'org-habitica-load-feature)

(use-package habitica
  :config
    (map! :leader
        (:prefix ("nh" . "habitica")
         :desc "Habitica tasks" "h" #'habitica-tasks)))

(use-package org-habitica :after habitica
  :config
  (map! :leader
        :desc "Save Habitica" "nhs" #'org-habitica-sync-task))

(message "[config] Apply config-habitica")
(provide 'config-habitica)
