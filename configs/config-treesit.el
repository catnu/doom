;;; configs/config-treesit.el -*- lexical-binding: t; -*-

(use-package! treesit
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :config
  ;; M-x `treesit-install-language-grammar` to install language grammar.
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

  (defun ++config/ts-mode (code-mode ts-mode)
    (or (featurep ts-mode)
        (let ((code-mode-map (intern (concat (symbol-name code-mode) "-map")))
              (ts-mode-map (intern (concat (symbol-name ts-mode) "-map"))))
          ;; (add-to-list 'jinx-camel-modes ts-mode)
          (eval `(set-keymap-parent ,ts-mode-map ,code-mode-map)))))

  (add-hook 'c-mode-hook #'c-ts-mode 'append)
  (after! c-ts-mode (++config/ts-mode 'c-mode 'c-ts-mode))

  (add-hook 'c++-mode-hook #'c++-ts-mode 'append)
  (after! c-ts-mode (++config/ts-mode 'c++-mode 'c++-ts-mode))

  (add-hook 'go-mode-hook #'go-ts-mode 'append)
  (after! go-ts-mode (++config/ts-mode 'go-mode 'go-ts-mode))

  (add-hook 'ruby-mode-hook #'ruby-ts-mode 'append)
  (after! ruby-ts-mode (++config/ts-mode 'ruby-mode 'ruby-ts-mode))

  (add-hook 'cmake-mode-hook #'cmake-ts-mode 'append)
  (after! cmake-ts-mode (++config/ts-mode 'cmake-mode 'cmake-ts-mode))

  (add-hook 'conf-toml-hook #'toml-ts-mode 'append)
  (after! toml-ts-mode (++config/ts-mode 'conf-toml 'toml-ts-mode))

  (add-hook 'css-mode-hook #'css-ts-mode 'append)
  (after! css-ts-mode (++config/ts-mode 'css-mode 'css-ts-mode))

  (add-hook 'js-mode-hook #'js-ts-mode 'append)
  (after! js-ts-mode (++config/ts-mode 'js-mode 'js-ts-mode))

  (add-hook 'js-json-hook #'json-ts-mode 'append)
  (after! json-ts-mode (++config/ts-mode 'js-json 'json-ts-mode))

  (add-hook 'python-mode-hook #'python-ts-mode 'append)
  (after! python-ts-mode (++config/ts-mode 'python-mode 'python-ts-mode))

  (add-hook 'sh-mode-hook #'bash-ts-mode 'append)
  (after! bash-ts-mode (++config/ts-mode 'sh-mode 'bash-ts-mode))

  (add-hook 'typescript-mode-hook #'typescript-ts-mode 'append)
  (after! typescript-ts-mode (++config/ts-mode 'typescript-mode 'typescript-ts-mode))

  (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)) 'append))

(message "[config] Apply config-treesit")
(provide 'config-treesit)