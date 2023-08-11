;;; configs/config-windows.el -*- lexical-binding: t; -*-

(use-package! sort-tab
  :config
  (defun sort-tab-workspace-buffer-list ()
    (cl-concatenate
     'list
     (if (featurep 'eaf)
         (eaf--get-eaf-buffers))
     (mapcar #'get-buffer
             (consult--buffer-query
              :sort 'visibility
              :as #'buffer-name
              :predicate
              #'(lambda (buf)
                  ;; don't use +workspace here to avoid Recursive load of workspace.el
                  (when-let ((workspace (persp-get-by-name
                                         (safe-persp-name (get-current-persp)))))
                    (and (not (eq (string-match "◀" (buffer-name buf)) 0))   ;; remove telega buffers
                         (not (eq (string-match "192-" (buffer-name buf)) 0)) ;; remove 192, 172 iterm
                         (not (eq (string-match "172-" (buffer-name buf)) 0))
                         (persp-contain-buffer-p
                          buf workspace))))))))

  (defun sort-tab-get-buffer-list-workspace ()
    (when-let ((bufs (if (featurep 'consult) (sort-tab-workspace-buffer-list) nil)))
      (setq bufs (cl-remove-if #'sort-tab-buffer-need-hide-p bufs))
      (setq bufs (sort bufs #'sort-tab-buffer-freq-higher-p))
      bufs))

  (advice-add 'sort-tab-get-buffer-list
              :before-until #'sort-tab-get-buffer-list-workspace)

  (defun sort-tab-not-focus (&rest args)
    (if (eq (current-buffer) (sort-tab-get-buffer))
        (other-window 1)))

  (advice-add
   'sort-tab-turn-off
   :after #'(lambda()
              (advice-remove 'evil-window-up #'sort-tab-not-focus)
              (advice-remove 'evil-window-down #'sort-tab-not-focus)
              (advice-remove 'evil-window-left #'sort-tab-not-focus)
              (advice-remove 'evil-window-right #'sort-tab-not-focus)
              (advice-remove 'evil-window-next  #'sort-tab-not-focus)
              (advice-remove '+workspace-switch #'(lambda (&rest r)
                                                    (unless (get-buffer-window (sort-tab-get-buffer))
                                                      (sort-tab-create-window))))))

  (advice-add
   'sort-tab-turn-on
   :after #'(lambda()
              (map!
               (:leader
                :desc "1" :n "1" #'sort-tab-select-visible-tab
                :desc "2" :n "2" #'sort-tab-select-visible-tab
                :desc "3" :n "3" #'sort-tab-select-visible-tab
                :desc "4" :n "4" #'sort-tab-select-visible-tab
                :desc "5" :n "5" #'sort-tab-select-visible-tab
                :desc "6" :n "6" #'sort-tab-select-visible-tab
                :desc "7" :n "7" #'sort-tab-select-visible-tab
                :desc "8" :n "8" #'sort-tab-select-visible-tab
                :desc "9" :n "9" #'sort-tab-select-visible-tab
                ))
              (advice-add 'evil-window-up :after #'sort-tab-not-focus)
              (advice-add 'evil-window-down :after #'sort-tab-not-focus)
              (advice-add 'evil-window-left :after #'sort-tab-not-focus)
              (advice-add 'evil-window-right :after #'sort-tab-not-focus)
              (advice-add 'evil-window-next :after #'sort-tab-not-focus)
              (advice-add '+workspace-switch
                          :after #'(lambda (&rest r)
                                     (unless (get-buffer-window (sort-tab-get-buffer))
                                       (sort-tab-create-window))))))

  (setq sort-tab-align 'center)
  (setq sort-tab-name-max-length 20))

(use-package! ace-window
  :defer t
  :init
  ;; (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-leading-char-face '(:foreground "green" :bold t))
  ;; (setq aw-font-size 20)
  (custom-set-faces
   '(aw-leading-char-face
     ((((class color)) (:foreground "orange" :height 3.0))
      (((background dark)) (:foreground "gray100"))
      (((background light)) (:foreground "gray0"))
      (t (:foreground "gray100" :underline nil)))))
  (setq aw-scope 'global
        aw-background t)
  (add-to-list 'aw-ignored-buffers "*sort-tab*"))

(map! (:leader
       ;; SPC w
       ;; split sv
       :n "ws" #'+evil/window-split-and-follow
       :n "wS" #'evil-window-split
       :n "wv" #'+evil/window-vsplit-and-follow
       :n "wV" #'evil-window-vsplit
       ;; navigation hjkl
       ;; enlage/ballence fF
       :n "wf" #'doom/window-enlargen
       :n "wF" #'balance-windows
       ;; selection/exchange/deletion aowedD
       :n "wa" #'ace-window
       ;; :n "wo" #'other-window
       :n "we" #'evil-window-exchange
       :n "wd" #'ace-delete-other-windows
       :n "wD" #'ace-delete-window ))

(global-set-key [remap other-window] #'ace-window)

(message "[config] Apply config-windows")
(provide 'config-windows)
