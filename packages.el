;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; for epkg borg
;; (package! closql)
;; for consult vertico
(package! pinyinlib)

;; for config-lookup
(package! dash-at-point)
;; TODO add lsp-bridge backend
;; (package! symbols-outline)

;; for config-windows
(package! ace-window)
(package! sort-tab
  :recipe (:host github
           :repo "manateelazycat/sort-tab"))

;; for config-lsp-bridge
(package! markdown-mode)
(package! yasnippet)

;; for config-posframe
(when (modulep! :completion vertico +childframe)
  (package! posframe)
  (package! vertico-posframe))

;; for config-corfu
(package! corfu)
(package! kind-icon)
(package! cape)

;; for config-rg
(package! rg)
(package! deadgrep)

;; for config-evil
(package! ace-jump-mode)
(package! ace-pinyin)

;; for config-notes
;; (package! obsidian)

;; for org babel
(package! ob-html
  :recipe (:host github
           :repo "misohena/ob-html"))

;; spell check
(package! jinx)

;; symbol highlight
(package! symbol-overlay)

;; gitmoji
(package! gitmoji
  :recipe (:host github
           :repo "janusvm/emacs-gitmoji"
           :files ("*.el" "data")))

;; for leetcode-cn
(package! graphql)
(package! aio)
(package! log4e)

(package! go-playground)
(package! rust-playground)
(package! gotest)

(package! tempel)
(package! tempel-collection)

;; for utils
(package! difftastic)
(package! string-inflection)

;; codeium
;; (package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

(package! citre)