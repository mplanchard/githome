;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

(package! ace-window) ;; allow easy jumping between many windows
(package! anzu) ;; preview find and replace
(package! browse-at-remote) ;; hopefully a bit of a better git-link
(package! cargo-mode
  :recipe (:host github :repo "ayrat555/cargo-mode"))
(package! code-review) ;; better code review
(package! csv-mode)  ;; some support for CSVs
(package! dap-mode)  ;; debugger
(package! dhall-mode)  ;; support for dhall config language
(package! direnv) ;; automatically run .envrc commands for the emacs environment
(package! edit-server)  ;; allow editing text boxes in FF
(package! elpher) ;; gopher in emacs
(package! evil-terminal-cursor-changer) ;; when running in the terminal, don't just use a block cursor all the time
(package! evil-textobj-tree-sitter) ;; tree-sitter objects as vim targets
(package! exec-path-from-shell) ;; ensure the path generated from bashrc gets used in emacs
(package! emms) ;; play music
;; (package! exwm) ;; emacs window manager
;; (package! geiser) ;; all the schemes
;; (package! geiser-guile) ;; guile
(package! gh-notify ;; nicer github notifications
  :recipe (:host github :repo "anticomputer/gh-notify"))
(package! ghub)
(package! git-link) ;; generate github links to files, regions, etc.
(package! graphql-mode)
(package! hackernews)  ;; elitest tech news and commentary
(package! ijanet  ;; support for interactive janet lang
  :recipe (:host github :repo "serialdev/ijanet-mode"))
(package! janet-mode) ;; support for janet lang
(package! jest) ;; javascript testing
(package! json-par) ;; structural editing of JSON
(package! kaolin-themes) ;; some themes
(package! kubernetes) ;; kubernetes overview
(package! kubernetes-evil) ;; kubernetes evil support
(package! lsp-pyright) ;; add support for the pyright lsp
(package! magit-delta) ;; nice diff highlighting
(package! mixed-pitch) ;; allow mixed monospace and proportional fonts
(package! mmm-mode) ;; allow multiple major modes (e.g. for graphql templates)
(package! modus-themes) ;; for pre-emacs 28
(package! mu4e-views) ;; html emails in xwidgets view
(package! nginx-mode)
(package! ob-typescript)  ;; org-bable support for typescript
(package! org-remark) ;; keep highlights and margin notes on any document
(package! org-transclusion) ;; copy content by link and keep it up to date
(package! polymode)
(package! prettier-js)  ;; use prettier for formatting JS/TS
(package! pandoc-mode) ;; convert ALL the things
(package! protobuf-mode) ;; support for editing protos
(package! pyvenv) ;; oh python
(package! rmsbolt) ;; read the assembly
(package! shfmt) ;; format shell scripts
(package! skewer-mode) ;; interactively edit JS/HTML
(package! string-inflection)  ;; modify string cases
;; now handled by doom in init.el
;; (package! tree-sitter)  ;; better syntax highlighting and language analysis
;; (package! tree-sitter-langs)
(package! unfill) ;; opposite of fill paragraph
(package! vterm-extra  ;; support for interactive janet lang
  :recipe (:host github :repo "Sbozzolo/vterm-extra"))

;; more up-to-date versions of these fix memory issues that were causing
;; lag due to excessive GC for very large rust projects
;; (unpin! lsp-mode)
;; (unpin! lsp-treemacs)
;; (unpin! lsp-ui)
(unpin! magit)
(unpin! forge)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
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
;; our package manager can't deal with; see raxod502/straight.el#279)
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
