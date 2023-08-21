;;; configs/config-minibrief.el -*- lexical-binding: t; -*-
;;; minibrief = posframe minibuffer at right top

(defvar ++minibrief/name-list '("Habitica" "Translation" "*Messages*"))
(defvar ++minibrief/show--name nil)

(defun ++minibrief/buffer-name (name)
  (if (string-match-p "^*" name) name (format " *Minibrief %s*" name)))

(defun ++minibrief/options (&optional list)
  (let ((list (or list (remove ++minibrief/show--name ++minibrief/name-list))))
    (if ++minibrief/show--name
        (append (or list (remove ++minibrief/show--name ++minibrief/name-list))
                (if (string= ++minibrief/show--name "Habitica")
                    '("Close" "Habitica Profile")
                  '("Close")))
      list)))

(defun ++minibrief/show (brief-name &optional color height width)
  (and ++minibrief/show--name (++minibrief/hide ++minibrief/show--name))
  (setq ++minibrief/show--name brief-name)
  (posframe-show (++minibrief/buffer-name brief-name)
                 ;; :poshandler 'posframe-poshandler-frame-top-right-corner
                 :poshandler #'(lambda (_info) '(-25 . 25))
                 ;; :background-color "green"
                 :foreground-color (or color "orange")
                 :border-width 1
                 :border-color (or color "orange")
                 :height height
                 :width width
                 :max-height 37
                 :max-width 50
                 :left-fringe 18
                 :right-fringe 18))

(defun ++minibrief/hide (brief-name &optional delete)
  ;; (when (string= brief-name "*Messages*")
  ;;   (advice-remove 'message #'++minibrief/message-tail)
  ;;   (setq delete t))
  (funcall (if delete #'posframe-delete #'posframe-hide) (++minibrief/buffer-name brief-name)))

(defun ++minibrief/message-tail (&rest _args)
  ;; ;; (with-current-buffer "*Messages*" (goto-char (point-max)))
  (let ((window (get-buffer-window "*Messages*" t)))
    (and window (with-current-buffer (window-buffer window)
                  (set-window-point window (point-max))))))

(defun ++minibrief/habitica-profile-button-pressed (&optional _button)
  ;; (funcall +lookup-open-url-fn private/habitica-profile-url)
  (funcall +lookup-open-url-fn "https://habitica.com/"))

(define-button-type '++minibrief/profile-button
  'action '++minibrief/habitica-profile-button-pressed
  'follow-link t
  'help-echo "Click Button"
  'help-args "test")

(defun ++minibrief/show-habitica-character ()
  (interactive)
  (if (or (not habitica-uid) (not habitica-token))
      (call-interactively 'habitica-login))
  (let ((buf (++minibrief/buffer-name "Habitica")))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (insert "\n")
      (habitica--parse-profile (assoc-default 'stats (habitica-api-get-profile)) nil)
      (insert "** Profile")
      (make-button (- (point) 10) (point) :type '++minibrief/profile-button)
      (insert "\n\n"))
    (++minibrief/show "Habitica")))

;; need https://github.com/soimort/translate-shell
(use-package translate-shell)
  ;; :config
  ;; <https://translate.google.com> is blocked in China for no apparent
  ;; reason. No one ever asked my option.
  ;; for mac you need proxychains-ng installed
  ;; (setq translate-shell-command "proxychains4 -q trans -t en+zh %s"
  ;;       translate-shell-brief-command "proxychains4 -q trans -brief -t zh+en %s"))

(defun ++minibrief/translate-shell-mix (word)
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

(defun ++minibrief/shwo-translation (&optional word)
  "Show the explanation of WORD in the echo area."
  (interactive
   (list (translate-shell--read-string)))
  (message "google translating...")
  (let* ((buf (++minibrief/buffer-name "Translation"))
         (word-sym (intern word))
         (result (if (assq word-sym translate-shell-brief-cache)
                     (assoc-default word-sym translate-shell-brief-cache)
                   (++minibrief/translate-shell-mix word))))
    (message "google translated")
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (insert "\n")
      (insert result)
      (insert "\n"))
    (++minibrief/show "Translation")))


(defun ++minibrief/pop-toggle ()
  (interactive)
  (let ((select (completing-read "Show: " (++minibrief/options))))
  (pcase select
    ("Close" (++minibrief/hide ++minibrief/show--name)
     (setq ++minibrief/show--name nil))
    ("*Messages*" (++minibrief/show select "#bbc2cf" 37 50)
     (++minibrief/message-tail)
     (advice-add 'message :after #'++minibrief/message-tail))
    ("Habitica Profile" (++minibrief/habitica-profile-button-pressed))
    ((guard (member select ++minibrief/name-list)) (++minibrief/show select)); todo memq
    (unknown (message (format "unknown minibrief name '%s'" unknown))))))

(message "[config] Apply config-minibrief")
(provide 'config-minibrief)