;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; native comp
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t))
;; comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; this package ensures that whatever PATH additions are a part of my standard
;; bash config wind up in emacs
(use-package! exec-path-from-shell
  ;; only use when in a nix-ish non-terminal app
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Matthew Planchard"
      user-mail-address "msplanchard@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font "Hack-11")
(setq doom-serif-font "DejaVu Serif-11")
(setq doom-variable-pitch-font "DejaVu Serif-11")
;; (setq doom-font "Fira Code-12")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename org-directory))
;; (setq +org-roam-open-buffer-on-find-file nil)
(setq org-agenda-files (list org-directory (file-name-concat org-directory "contacts")))
(setq org-journal-dir (file-name-concat org-directory "journal"))

(setq org-roam-capture-templates
      (list
       '("d" "default" plain "%?"
         :if-new (file+head "${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(setq +org-roam-open-buffer-on-find-file nil)

;; Speed up saving of large files in org-roam by batching operations into a
;; single sqlite txn. From org-roam#1752 on GH.
(advice-add 'org-roam-db-update-file :around
              (defun +org-roam-db-update-file (fn &rest args)
                  (emacsql-with-transaction (org-roam-db)
                    (apply fn args))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; Show time in the modeline
(setq display-time-mode t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; **********************************************************************
;; Experiments
;; **********************************************************************
;; Things I'm trying that may or may not pan out.
;; **********************************************************************

(after! lsp-mode
  (advice-remove #'lsp #'+lsp-dont-prompt-to-install-servers-maybe-a))

;; 15 is the original setting, but it seems like it's down to 0.5 via doom or
;; something, so try setting it back up to avoid GC pauses
(setq gcmh-idle-delay 10)

;; doom's `persp-mode' activation disables uniquify, b/c it says it breaks it.
;; It doesn't cause big enough problems for me to worry about it, so we override
;; the override. `pers-mode' is activated in the `doom-init-ui-hook', so we add
;; another hook at the end of the list of hooks to set our uniquify values.
(add-hook! 'doom-init-ui-hook
           :append ;; ensure it gets added to the end.
           #'(lambda () (require 'uniquify) (setq uniquify-buffer-name-style 'forward)))

(after! orderless
  (add-to-list 'orderless-matching-styles #'orderless-prefixes))

;; **********************************************************************
;; Settings
;; **********************************************************************

(setq enable-local-variables t)
;; (setq browse-url-browser-function 'browse-url-xdg-open)

(setq typescript-indent-level 2)
(setq js-indent-level 2)
(setq rustic-indent-offset 4)
(setq tab-width 4)

(setq ispell-dictionary "en_US")

(setq +format-on-save-enabled-modes
      '(not web-mode))

;; Autosave when losing focus
;; (add-to-list 'doom-switch-buffer-hook (lambda () (when buffer-file-name (save-buffer))))
;; (add-to-list 'doom-switch-window-hook (lambda () (when buffer-file-name (save-buffer))))
;; (add-to-list 'doom-switch-frame-hook (lambda () (when buffer-file-name (save-buffer))))

;; Search the GH directory for projects by default
(setq projectile-project-search-path
      '("~/s/gh"
        "~/s/gh/spectrust"
        "~/s/gl"
        "~s/gl/spectrust"
        "~/s/gh/mplanchard"))

(after! company
  (setq company-idle-delay 0.01))

(after! evil
  ;; ensure escape happens immediately, so subsequent normal commands are executed
  (setq evil-esc-delay 0)
  (setq evil-escape-delay 0.1) ;; delay between pressing j and k to escape
  (setq evil-ex-search-case 'smart))

;; LSP Settings and Performance Tuning
;; performance
;; (setq lsp-use-plists t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1 mb

(use-package! lsp
  :config
  ;; no need for this IMO
  (setq lsp-enable-on-type-formatting nil)
  ;; the headerline is cool
  (setq lsp-headerline-breadcrumb-enable t)
  ;; the icons don't look good
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  ;; enable annoying squiggles in the headerline
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  ;; prevent running lsp stuff for a second, for performance
  (setq lsp-idle-delay 1)

  ;; Show function signatures while writing functions and types for the thing at point
  (setq lsp-signature-auto-activate t)
  ;; I like the function signatures while writing functions, but don't like
  ;; the we way the docs make the little buffer at the bottom pop up distractingly.
  (setq lsp-signature-render-documentation nil)

  ;; I just use spc c a for this, don't need them on the modeline
  (setq lsp-modeline-code-actions-enable nil)

  ;; watching all the files in a project doesn't seem to be worth the performance
  ;; tradeoff when the project gets large
  (setq lsp-enable-file-watchers nil)

  ;; can't hit the lenses with the keyboard, don't care about them
  (setq lsp-lens-enable nil)

  ;; Enable proc-macro expansion
  (setq lsp-rust-analyzer-proc-macro-enable t)
  ;; Use build scripts in the analyzer context
  (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  ;; Build with --all-features
  (setq lsp-rust-all-features t)
  ;; Build with --test
  (setq lsp-rust-cfg-test t)
  ;; Inlay type hints are nice
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t))

(use-package! lsp-ui
  :config
  ;; Add a delay here for performance
  (setq lsp-ui-sideline-delay 0.75)
  ;; Show more context b/c rust errors are chonky
  (setq lsp-ui-sideline-diagnostic-max-lines 20)
  ;; This is distracting
  (setq lsp-ui-sideline-show-code-actions nil)
  ;; This shows function signatures and stuff, but I already get that below
  ;; the modeline, so this is just distracting. It does look cool though.
  (setq lsp-ui-sideline-show-hover nil)

  ;; Don't want auto-docs. I have it triggering on g h for hover or shift+k
  ;; for the poppup buffer
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  ;; More performance-related delays
  (setq lsp-ui-doc-delay 0.75)
  ;; hover where I'm at when I do trigger it
  (setq lsp-ui-doc-position 'at-point)

  ;; the default size is much too small. Make it both wider and taller.
  (setq lsp-ui-doc-max-width 150)
  (setq lsp-ui-doc-max-height 24)

  ;; Because why not??
  (setq lsp-ui-doc-include-signature t)

  ;; I like "go to definition" (g d) to show me a peek of the definnition, b/c
  ;; I very often want to just look at it, not travel to it. I can press Enter
  ;; from the peek if I want to travel.
  (setq lsp-ui-peek-always-show t))


;; Delay for perf
(setq flycheck-idle-change-delay 1.5)

;; Allow lots of flycheck errors
(setq flycheck-checker-error-threshold 1000)

;; headerline mode fails in ediff, so make sure it doesn't start.
(add-hook!
 '(ediff-prepare-buffer-hook magit-blob-mode-hook)
 #'(lambda () (lsp-headerline-breadcrumb-mode -1)))

;; treat camelCase words as two words for spellcheck
(setq ispell-extra-args (append '("--camel-case") ispell-extra-args))

;; Try to improve syntax hihglighting performance for really large files
(after! jit-lock
  ;; defer fontification while input is pending
  (setq jit-lock-defer-time 0)
  ;; When a buffer is idle for some time, go ahead and fontify areas outside
  ;; the view, to avoid work when scrolling
  (setq jit-lock-stealth-time 32))

(after! company
  ;; -------------------------------------------
  ;; Disable global completion for certain modes
  ;; -------------------------------------------
  ;;
  ;; ensure the first element is `not', so that the list is negated
  (unless (eq (car company-global-modes) 'not)
    ;; remove any existing not, just in case
    (setq company-global-modes (remove 'not company-global-modes))
    ;; set the first element to not
    (setcar company-global-modes 'not))
  ;; add modes in which to disable company-mode to the list, passing `t' for the
  ;; APPEND argument, which will ensure they are added to the end of the list, so
  ;; that they do not interfere with the negation.
  (add-to-list 'company-global-modes 'markdown-mode t)
  (add-to-list 'company-global-modes 'org-mode t)
  (add-to-list 'company-global-modes 'gfm-mode t)
  (add-to-list 'company-global-modes 'git-commit-mode t))
  ;; -------------------------------------------

;; don't try to smooth scroll on mac
(setq mac-mouse-wheel-smooth-scroll nil)

;; smooth scrolling!
(setq pixel-scroll-precision-mode t)

;; Turn on fill-column-indicator mode globally, except for certain modes.
(defun mp/disable-fill-column-indicator-mode ()
  (display-fill-column-indicator-mode 0))
(global-display-fill-column-indicator-mode)
(add-hook! '+doom-dashboard-mode-hook #'mp/disable-fill-column-indicator-mode)
(add-hook! 'vterm-mode-hook #'mp/disable-fill-column-indicator-mode)


;; doom hack
;; disable evil-snipe-override mode, which causes `d f SPC' and similar commands
;; to delete up to but not including the space
(add-hook! 'doom-first-input-hook ;; this is the mode that enables it
           :append ;; add to end of list so that ours executes last
           #'(lambda () (evil-snipe-override-mode -1)))

;; don't try to restart the server if it's already running
;; (unless
;;     (and
;;      (boundp 'server-process)
;;      (memq (process-status server-process) '(connect listen open run)))
;;   (server-start))
(use-package! org-protocol
  :config
  (let ((t1 `("P" "Protocol" entry (file "inbox.org")
              "* TODO %:description \nSource: [[%:link][%:description]] \nCaptured On: %U \n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"))
        (t2 `("L" "Protocol Link" entry (file "inbox.org")
              "* TODO %:description \nSource: [[%:link][%:description]] \nCaptured On: %U\n%?")))
    (unless (member t1 org-capture-templates) (add-to-list 'org-capture-templates t1))
    (unless (member t2 org-capture-templates) (add-to-list 'org-capture-templates t2))))

;; for debugging
;; (setq org-capture-templates nil)

;; org-mode settings
(after! org
  ;; set up associations for org-file-open
  ;; - set the system opener
  ;; -- for linux, use xdg-open
  (setf (alist-get 'system org-file-apps-gnu) "xdg-open %s")
  ;; -- for mac, use open
  (setf (alist-get 'system org-file-apps-macos) "open %s")
  ;; - for (x)html files, open w/the system opener
  (setf (alist-get "\\.x?html?\\'" org-file-apps nil nil #'equal) 'system)
  ;; don't add section numbers to headings on export
  (setq org-export-with-section-numbers nil)
  ;; keep quotes as I wrote them, don't automatically use smart quotes
  (setq org-export-with-smart-quotes nil)
  ;; require brackets to use sub/superscripts, so that a straight underline or
  ;; caret doesn't get interpreted as such
  (setq org-export-with-sub-superscripts '{})
  ;; don't automatically add a ToC to exports
  (setq org-export-with-toc nil)
  ;; Allow executing JS code blocks in org
  (use-package! ob-js)
  ;; Allow executing TS code blocks in org
  (use-package! ob-typescript
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((typescript . t))))
  ;; enable auto-fill in org-mode by default
  (add-hook! org-mode #'(lambda () (auto-fill-mode)))
  ;; prevent killing the agenda buffer, since it takes a while to load
  (add-hook! 'org-agenda-after-show-hook #'(lambda () (emacs-lock-mode 'kill))))

;; org-journal settings
(use-package! org-journal
  :config
  (setq
   ;; Just shove it straight into the org dir
   org-journal-dir org-directory
   ;; One file per day
   org-journal-file-type 'daily
   ;; Since we're doing a file per day, put the date in as the note title,
   ;; instead of the default top-level bullet
   org-journal-date-prefix "#+TITLE: "
   ;; The title is going to be like Journal: 2020-11-06, Friday
   org-journal-date-format "Journal: %Y-%m-%d, %A"
   ;; Since we're doing a file per day, each time entry is a top-level bullet,
   ;; instead of the default second-level bullet
   org-journal-time-prefix "* "
   ;; Make the file names just a bit nicer than the default all numeric %Y%m%d
   org-journal-file-format "journal-%Y-%m-%d.org"))

;; don't highlight bookmarks by default
(setq bookmark-fontify nil)
;; set file for annotations
(setq org-annotate-file-storage-file "~/org/annotations.org")
(setq org-annotate-file-add-search t)

(setq
 deft-directory org-directory
 deft-extensions '("org" "md")
 deft-recursive t)

(setq deft-directory org-directory
      deft-extensions '("org" "md" "txt")
      deft-recursive t)

(after! github-review
  :config
  (add-hook! 'github-review-mode-hook #'(lambda () (ws-butler-mode -1))))

(after! rmsbolt
  :config
  (if
      (eq (assq 'rustic-mode rmsbolt-languages) nil)
      (setq rmsbolt-languages
            (cons
             `(,'rustic-mode . ,(alist-get 'rust-mode rmsbolt-languages))
             rmsbolt-languages))))

(after! rustic
  :config
  ;; why would I ever not want this
  (setq rustic-format-on-save t))

;; SQL
(after!
  sql
  (setq sql-postgres-login-params (append sql-postgres-login-params '(port))))

;; direnv
(after!
  direnv
  (setq direnv-non-file-modes (append direnv-non-file-modes '(+doom-dashboard-mode))))

;; **********************************************************************
;; Packages
;; **********************************************************************

;; magit delta is soooo gooooood
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; Automatically load .envrc files whenever possible
(use-package! direnv :config (direnv-mode))
;; Use this explicitly because I like using it to jump around when I have lots
;; of windows open. Keybinding for ace-window is set up below.
(use-package! ace-window)

;; music player in in emacs
(after! emms
  (use-package! emms-setup)
  (use-package! emms-info-tinytag)
  (emms-all)
  (setq emms-info-functions '(emms-info-tinytag))
  (setq emms-player-list '(emms-player-vlc, emms-player-vlc-playlist))
  (setq emms-source-file-default-directory "~/Music")
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (emms-history-load))

(use-package! gh-notify)

;; visual debugger
(use-package! dap-cpptools
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode t)
  (use-package! dap-lldb)
  (use-package! dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb"
                                     :request "launch"
                                     :name "LLDB::Run"
                                     :gdbpath "rust-lldb"
                                     :target nil
                                     :cwd nil)))

(use-package! git-link
  :config
  (setq git-link-use-commit t))

(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! kubernetes-evil
  :config (evil-make-overriding-map kubernetes-mode-map 'normal)
  :after kubernetes)

;; Use mermaid-mode for mermaid files
(use-package! mermaid-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode)))

;; use dockerfile-mode for files with .dockerfile extension
(use-package! docker
  :config
  (add-to-list 'auto-mode-alist '("\\.[dD]ockerfile\\'" . dockerfile-mode)))

(use-package! edit-server
  :commands edit-server-start
  :init
  (setq edit-server-port 9293)
  (if after-init-time
      (edit-server-start)
    (add-hook 'after-init-hook
              #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t))))

(use-package! magit
  :config
  ;; Show local branches in magit status buffer
  (unless
      (member 'magit-insert-local-branches magit-status-sections-hook)
    (setq magit-status-sections-hook (append magit-status-sections-hook '(magit-insert-local-branches))))
  ;; Copy abbreviated revisions instead of the whole thing
  (setq magit-copy-revision-abbreviated t))

(use-package! tree-sitter-langs)

;; Better local syntax highlighting and language analysis
(use-package! tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; **********************************************************************
;; Keybindings
;; **********************************************************************

(map! :map dired-mode-map
      :nv "y y"
      #'my/dired-kill-full-path)

(map! (:map embark-file-map
       :desc "v-split"
       "V" (lambda (file) (+evil/window-vsplit-and-follow) (find-file file)))
      (:map embark-file-map
       :desc "h-split"
       "H" (lambda (file) (+evil/window-split-and-follow) (find-file file))))

(map! (:map org-mode-map
       :localleader
       :desc "org-insert-structure-template" "T" #'org-insert-structure-template)
      (:map org-mode-map
       :localleader
       :desc "emphasize"
       "!"
       #'org-emphasize))

(map! (:leader
       :prefix "w"
       :desc "go-to-window" :nv "g" #'ace-window)
      (:leader
       :prefix "w"
       :desc "swap-window" :nv "/" #'ace-swap-window))

(after! avy
  (map!
   (:prefix "g s"
    :desc "select and delete region"
    :nv "D"
    #'avy-kill-region)
   (:prefix "g s"
    :desc "select and delete line"
    :nv "d"
    #'avy-kill-whole-line)
   (:prefix "g s"
    :desc "select and copy region"
    :nv "Y"
    #'avy-kill-ring-save-region)
   (:prefix "g s"
    :desc "select and copy line"
    :nv "y"
    #'avy-kill-ring-save-whole-line)))

;; syntax aware text objects for evil motion
(after! evil-text-object-change-visual-type
  (map! (:textobj "f"
         (evil-textobj-tree-sitter-get-textobj "function.inner")
         (evil-textobj-tree-sitter-get-textobj "function.outer"))
        (:textobj "c"
         (evil-textobj-tree-sitter-get-textobj "comment.outer")
         (evil-textobj-tree-sitter-get-textobj "comment.outer"))
        (:textobj "C"
         (evil-textobj-tree-sitter-get-textobj "class.inner")
         (evil-textobj-tree-sitter-get-textobj "class.outer"))))

(map! :leader
      :desc "paste from kill ring" :nv "P" #'+default/yank-pop)

(map! :prefix "g"
      :desc "show-hover-doc" :nv "h" #'lsp-ui-doc-glance)

;; Replace rustic's cargo check, build, and run, which open in a minibuffer
;; popup, to open in dedicated buffers
(map! (:after rustic
       :map rustic-mode-map
       :localleader
       :prefix "b"
       :desc "cargo check"
       :nv "c"
       #'(lambda ()
           (interactive)
           (rustic-run-cargo-command "cargo check --tests" '(:buffer "*cargo-check*"))))
      (:after rustic
       :map rustic-mode-map
       :localleader
       :prefix "b"
       :desc "cargo build"
       :nv "b"
       #'(lambda ()
           (interactive)
           (rustic-run-cargo-command "cargo build" '(:buffer "*cargo-build*"))))
      (:after rustic
       :map rustic-mode-map
       :localleader
       :prefix "b"
       :desc "cargo run"
       :nv "r"
       #'(lambda ()
           (interactive)
           (rustic-run-cargo-command "cargo run" '(:buffer "*cargo-run*")))))

(map! (:after org
       :map org-mode-map
       :localleader
       :prefix "l"
       :desc "github-link" :nv "g" #'mp-insert-github-pr-link)
      (:after org
       :map org-mode-map
       :localleader
       :prefix "l"
       :desc "jira-link" :nv "j" #'mp-insert-jira-ticket-link)
      (:after org
       :prefix "g"
       :desc "open at point"
       :nv "o"
       #'org-open-at-point))

(map! (:leader
       :prefix "c"
       :desc "Open imenu buffer"
       :nv "O"
       #'lsp-ui-imenu)
      (:leader
       :prefix "c"
       :desc "Show buffer errors"
       :nv "X"
       #'flycheck-list-errors)
      (:leader
       :prefix "c"
       :desc "Find implementations"
       :nv "i"
       #'lsp-find-implementation)
      ;; find-references on SPC-c-D as opposed to the peek, which I already
      ;; have on g-D. This allows me to use embark-export on the results to
      ;; get a persistent buffer of refs
      (:leader
       :prefix "c"
       :desc "Find references"
       :nv "D"
       #'lsp-find-references))

(map!
 (:after vterm
  (:map vterm-mode-map
   :desc "send up in insert mode" :i "C-k" #'vterm-send-up)
  (:map vterm-mode-map
   :desc "send down in insert mode" :i "C-j" #'vterm-send-down))
 (:map shell-mode-map
  :desc "send up in insert mode" :i "C-k" #'comint-previous-input)
 (:map shell-mode-map
  :desc "send up in insert mode" :i "C-j" #'comint-next-input))

(after! magit
  (map!
   :map magit-status-mode-map
   :desc "jump to stashes"
   :prefix "g"
   :nv
   "z"
   #'magit-jump-to-stashes)
  (map!
   :map magit-status-mode-map
   :desc "jump to section"
   :prefix "g"
   :nv
   "."
   #'magit-status-jump))

;; The `+eval:region' and `+eval:buffer' commands that are bound to g r and g R
;; by default don't work in sql mode, so we replace them with the sql-specific
;; evaluation functions, which will send the region or buffer to an open sql shell
(map! (:map sql-mode-map
       :desc "evaluate region"
       :prefix "g"
       :nv "r"
       #'sql-send-region)
      (:map sql-mode-map
       :desc "evaluate buffer"
       :prefix "g"
       :nv "R"
       #'sql-send-buffer))

;; Same deal with janet-mode
(map! (:map janet-mode-map
       :desc "evaluate region"
       :prefix "g"
       :nv "r"
       #'ijanet-eval-region)
      (:map janet-mode-map
       :desc "evaluate buffer"
       :prefix "g"
       :nv "R"
       #'ijanet-eval-buffer))


(map! (:map code-review-mode-map
       :desc "Add or edit comment"
       "C-c C-c"
       #'code-review-comment-add-or-edit)
      (:map code-review-mode-map
       :desc "Add or edit comment"
       :localleader
       "c"
       #'code-review-comment-add-or-edit)
      (:map code-review-mode-map
       :desc "Show transient api"
       :localleader
       "m"
       #'code-review-transient-api)
      (:map code-review-mode-map
       :desc "Next comment"
       "] ]"
       #'code-review-comment-jump-next)
      (:map code-review-mode-map
       :desc "Previous comment"
       "[ ["
       #'code-review-comment-jump-previous)
      (:map code-review-comment-mode-map
       :desc "Show transient API"
       :localleader
       "m"
       #'code-review-transient-api))

;; This resolves a weird error when creating a new frame via (make-frame) that
;; I haven't been able to find any info on. Error message is
;; "The default fontset can't be used for a frame font".
(setq default-frame-alist (assq-delete-all 'font default-frame-alist))


;; Somehow recently this started overriding TAB in the magit status buffer.
;; Revisit later to see if it's fixed.
(after! evil
  (define-key evil-motion-state-map (kbd "<tab>") nil))

(after! markdown-mode
  (setq markdown-header-scaling t)
  ;; doom sets this to nil because of a potential error, described in
  ;; jrblevin/markdown-mode#578, with native comp and headings in markdown.
  ;; I don't see this error, and not setting this to nil makes the generated
  ;; toc by markdown-toc non-nested.
  (setq markdown-nested-imenu-heading-index t)
  (setq markdown-toc-header-toc-start "<!-- markdown-toc-start -->"))

;; Enable auto-fill mode for markdown
(add-hook! 'markdown-mode-hook #'(lambda () (auto-fill-mode)))
(add-hook! 'gfm-mode-hook #'(lambda () (auto-fill-mode)))

;; **********************************************************************
;; Javascript/Typescript
;; **********************************************************************

;; add node-modules to exec path
(add-hook 'js-mode-hook #'add-node-modules-path)
(add-hook 'js2-mode-hook #'add-node-modules-path)
(add-hook 'typescript-mode-hook #'add-node-modules-path)

;; autoformat with prettier on save
(add-hook 'js-mode-hook #'prettier-js-mode)
(add-hook 'js2-mode-hook #'prettier-js-mode)
(add-hook 'typescript-mode-hook #'prettier-js-mode)

;; fancy testing
(use-package! jest
  :after (js2-mode typescript-mode)
  :hook (js2-mode . jest-minor-mode) (typescript-mode . jest-minor-mode))

(defun mp-flycheck-update-js-lsp-checkers ()
  "Update JS checkers for LSP mode"
  (when (bound-and-true-p lsp-mode) (flycheck-add-next-checker 'lsp 'javascript-eslint)))

;; Run the eslint langserv in addition to the major one
(add-hook 'typescript-mode-hook #'mp-flycheck-update-js-lsp-checkers)
(add-hook 'js-mode-hook #'mp-flycheck-update-js-lsp-checkers)
(add-hook 'js2-mode-hook #'mp-flycheck-update-js-lsp-checkers)

;; **********************************************************************
;; Python
;; **********************************************************************

(setq flycheck-python-mypy-executable "mypy")

;; (use-package! python-black
;;   :after python)

;; Set python language server caching to max


(use-package! lsp-pyright
  :init
  ;; leave it to mypy
  (setq lsp-pyright-typechecking-mode "off")
  :hook
  (python-mode
   . (lambda ()
       (require 'lsp-pyright)
       (lsp-deferred))))


;; Try to find combinations of things that aren't slow
(add-hook
 'python-mode-hook
 (lambda ()
   (after! lsp
     (flycheck-add-next-checker 'lsp 'python-flake8))
   ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)
   ;; they're so slooooow, do them manually
   ;; (add-to-list 'flycheck-disabled-checkers 'python-mypy)
   ))

(add-hook
 'lsp-pyls-after-open-hook
 (lambda ()
   (setq lsp-python-ms-cache "Library")))

;; (add-hook! 'python-mode-hook #'python-black-on-save-mode)
;; Feel free to throw your own personal keybindings here
(map! :map python-mode-map
      :localleader
      :desc "Blacken Buffer" "b b" #'python-black-buffer)
(map! :map python-mode-map
      :localleader
      :desc "Blacken Region" "b r" #'python-black-region)
(map! :map python-mode-map
      :localleader
      :desc "Blacken Statement" "b s" #'python-black-statement)

;; **********************************************************************
;; Work-related
;; **********************************************************************

(setq mp/default-github-org "SpecTrust-Inc")
(setq mp/default-github-repo "spec-protect")

(setq sql-connection-alist
      '(("st-dev"
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-port 5432)
         (sql-user "postgres")
         (sql-database "spec_protected"))))

;; **********************************************************************
;; Custom Functions
;; **********************************************************************

(defun my/dired-kill-full-path ()
    "Copy the absolute path to a file in dired"
    (interactive)
    (dired-copy-filename-as-kill 0))

;; toggle from absolute to visual relative numbers
(defun my/toggle-relative-line-numbers ()
  (interactive)
  (if (eq display-line-numbers t)
      (setq display-line-numbers 'visual)
    (setq display-line-numbers t)))

(defun mp-parse-github-pr-target (target)
  "Parse the given GitHub PR TARGET into a URL

A TARGET is something that GitHub would automatically recognize as a GitHub
link when writing an issue or PR, such as myrepo#23 or someorg/myrepo#23.

If the TARGET does not contain an org name, the value of `mp/default-github-org'
will be used as the org name."
  ;; let*, as opposed to let, ensures that the variables are bound sequentially,
  ;; so that each bound variable is available in the context of the next
  ;; variable being bound
  (let* ((split-url (split-string target "[#/]" t "[[:space:]]+"))
         (split-len (length split-url)))
    (cl-multiple-value-bind
        (org repo id)
        (cond
         ((eq split-len 3) (list (pop split-url) (pop split-url) (pop split-url)))
         ((eq split-len 2) (list mp/default-github-org (pop split-url) (pop split-url)))
         ((eq split-len 1) (list mp/default-github-org mp/default-github-repo (pop split-url)))
         (t (error! "Couldn't parse target: %s" target)))
      (list
       (concat "https://github.com/" org "/" repo "/pull/" id)
       target))))

(defun mp-make-github-pr-link (target)
  "Construct an org link from the PR TARGET text

A TARGET is something that GitHub would automatically recognize as a GitHub
link when writing an issue or PR, such as myrepo#23 or someorg/myrepo#23.

Prompt for the TARGET when called interactively."
  (interactive "sGithub Target: ")
  (apply 'org-insert-link nil (mp-parse-github-pr-target target)))

(defun mp-insert-github-pr-link (start end)
  "Insert an org link for the selected PR target

A TARGET is something that GitHub would automatically recognize as a GitHub
link when writing an issue or PR, such as myrepo#23 or someorg/myrepo#23.

When a TARGET is selected, replace it with an org-mode link. If there is no
active selection, prompt for a TARGET and insert the org-mode link at the
cursor.
"
  (interactive "r")
  ;; check whether the region is actively selected
  (if (use-region-p)
      ;; If so, use the start and end to make the link
      (mp-make-github-pr-link (buffer-substring start end))
    ;; Otherwise, call the function interactively
    (call-interactively 'mp-make-github-pr-link)))


(defun mp-parse-jira-ticket-link (target)
  (let ((trimmed (car (split-string target nil t))))
    (list
     (concat "https://bestowinc.atlassian.net/browse/" trimmed)
     target)))


(defun mp-insert-org-jira-ticket-link (target)
  "Insert an org link for a Jira ticket from a ticket identifier."
  (interactive "sJira Target: ")
  (apply 'org-insert-link nil (mp-parse-jira-ticket-link target)))


(defun mp-insert-jira-ticket-link (start end)
  "Insert an org link for a Jira ticket, either interactively or from a region."
  (interactive "r")
  (if (use-region-p)
      (mp-insert-org-jira-ticket-link (buffer-substring start end))
    (call-interactively 'mp-insert-org-jira-ticket-link)))


(defun mp-get-relative-path ()
  "Get the path relative to the project root, or nil if not in a project."
  (let
      ((root-dir (projectile-project-root))
       (local-dir (or load-file-name buffer-file-name)))
    (cond
     ((not root-dir) nil)
     (t (concat "./" (substring local-dir (string-width root-dir) nil))))))


(defun mp-copy-relative-path ()
  "Copy the path to the current file, relative to the project root.

If not currently in a Projectile project, does not copy anything.
"
  (interactive)
  (kill-new (mp-get-relative-path)))

(defun mp/echo-async-run-shell-command (command)
  "Echo the shell command to the *Async Shell Command* buffer, then run it, opening the buffer."
  (async-shell-command (format "echo '%s' && %s" command command))
  (get-buffer "*Async Shell Command*"))

(defun my/convert (value-string unit-string)
  "Convert VALUE-STRING to the target UNIT-STRING.

This simple function is just here to make non-interactive unit conversion easier.
For interactive conversion, use `(calc-convert-units)'."
  (calc-eval
   (math-convert-units (calc-eval value-string 'raw) (calc-eval unit-string 'raw))))

(defun my/enhance (count) (interactive "p") (doom/increase-font-size count))

;; **********************************************************************
;; Externally Sourced Functions
;; **********************************************************************

;; Source: https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
(defun my/run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun my/run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'my/run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(defun my/aws-mfa (mfa-code)
  (interactive "sMFA Code: ")
  ;; For whatever reason, AWS won't log you in if you already have any
  ;; of these set.
  (setenv "AWS_ACCESS_KEY_ID")
  (setenv "AWs_SECRET_ACCESS_KEY")
  (setenv "AWS_SESSION_TOKEN")
  (let* ((iam-user
          ;; substring to remove the final newline
          (substring
           ;; get username, e.g. mplanchard
           (shell-command-to-string
            "aws sts get-caller-identity \
             --output json \
             | jq -r '.Arn' \
             | awk -F '/' '{print $2}'") 0 -1))
         (mfa-arn
          (substring
           ;; get the MFA device identifier
           (shell-command-to-string
            (format "aws iam list-mfa-devices \
                    --user-name %s \
                    --output json \
                    | jq -r '.MFADevices[0].SerialNumber'"
                    iam-user)) 0 -1))
         (credentials
          ;; get the credentials, which are space-separated, and
          ;; split them into a list
          (split-string
           (shell-command-to-string
            (format "aws sts get-session-token \
                     --serial-number %s \
                     --token %s \
                     --output text \
                     --duration-seconds 21600 \
                     | awk '{print $2, $4, $5}'"
                    mfa-arn
                    mfa-code))))
         ;; pull individual items out of the credentials list
         (access-key-id (nth 0 credentials))
         (secret-access-key (nth 1 credentials))
         (session-token (nth 2 credentials)))
    (unless (seq-every-p #'identity (list access-key-id secret-access-key session-token))
      (error "Problem getting AWS info"))
    (setenv "AWS_ACCESS_KEY_ID" access-key-id)
    (setenv "AWS_SECRET_ACCESS_KEY" secret-access-key)
    (setenv "AWS_SESSION_TOKEN" session-token)))

;; **********************************************************************
;; Org Tags
;; **********************************************************************

(setq org-tag-persistent-alist
      (quote
       ((:startgrouptag)
        ("software")
        (:grouptags)
        ("api")
        ("database")
        ("emacs")
        ("encryption")
        ("javascript")
        ("lisp")
        ("rust")
        ("software_architecture")
        ("software_culture")
        ("software_optimization")
        ("software_research")
        ("software_security")
        ("software_people")
        ("software_tools")
        ("unix")
        (:endgrouptag)

        (:startgrouptag) ("api")
        (:grouptags) ("rest") ("graphql")
        (:endgrouptag)

        (:startgrouptag) ("authorization")
        (:grouptags) ("rbac")
        (:endgrouptag)

        ("camping")
        ("coffee")

        (:startgrouptag) ("concurrency")
        (:grouptags)
        ("async_await")
        ("go_statement")
        ("multithread")
        ("multiprocess")
        (:endgrouptag)

        ("fun")

        (:startgrouptag) ("database")
        (:grouptags) ("postgres") ("sqlite") ("mysql")
        (:endgrouptag)

        (:startgrouptag) ("emacs")
        (:grouptags)
        ("elisp")
        ("emacs_config")
        ("emacs_packages")
        ("emacs_themes")
        ("mu4e")
        ("org_mode")
        (:endgrouptag)

        (:startgrouptag) ("encryption")
        (:grouptags) ("gpg") ("ssh")
        (:endgrouptag)

        (:startgrouptag) ("javascript")
        (:grouptags) ("react") ("typescript") ("vuejs")
        (:endgrouptag)

        (:startgrouptag) ("lisp")
        (:grouptags) ("clojure") ("elisp") ("racket") ("scheme")
        (:endgrouptag)

        (:startgrouptag) ("math")
        (:grouptags)
        ("statistics")
        (:endgrouptag)

        ("model") ;; a standard or ideal way to do something

        (:startgrouptag) ("org_mode")
        (:grouptags) ("org_roam")
        (:endgrouptag)

        (:startgrouptag) ("people_management")
        (:grouptags) ("relationships") ("team_dynamics")
        (:endgrouptag)

        ("possible_purchases")
        ("possible_gifts")

        (:startgrouptag) ("rust")
        (:grouptags)
        ("actix")
        ("warp")
        ("rust_analyzer")
        ("rust_async")
        ("rust_features")
        ("rust_web")
        (:endgrouptag)

        (:startgrouptag) ("rust_web")
        (:grouptags) ("actix") ("tokio_axum") ("rust_frontend")
        (:endgrouptag)

        (:startgrouptag) ("rust_async")
        (:grouptags) ("async_std") ("tokio")
        (:endgrouptag)

        (:startgrouptag) ("software_architecture")
        (:grouptags)
        ("complexity")
        ("concurrency")
        ("design_patterns")
        ("dynamic_linking")
        ("file_system_architecture")
        ("no_silver_bullet")
        ("software_patterns")
        (:endgrouptag)

        (:startgrouptag) ("software_patterns")
        (:grouptags)
        ("pubsub")
        ("type_driven_development")
        (:endgrouptag)

        (:startgrouptag) ("software_security")
        (:grouptags) ("authentication") ("authorization") ("credentials")
        (:endgrouptag)

        (:startgrouptag) ("type_driven_development")
        (:grouptags)
        ("parse_dont_validate")
        (:endgrouptag)

        (:startgrouptag) ("software_optimization")
        (:grouptags)
        ("cache_utilization")
        ("software_performance")
        ("software_speed")
        (:endgrouptag)

        (:startgrouptag) ("software_people")
        (:grouptags)
        ("andrew_gallant")
        ("brian_kernighan")
        ("dan_luu")
        ("fred_brooks")
        ("gary_bernhradt")
        ("paul_graham")
        ("rich_hickey")
        (:endgrouptag)

        (:startgrouptag) ("software_tools")
        (:grouptags)
        ("awk")
        ("git")
        ("graphical_applications")
        ("gui_frameworks")
        ("parsers")
        ("shell_applications")
        ("systemd")
        (:endgrouptag)

        (:startgrouptag) ("spectrust")
        (:grouptags)
        ("workflow_engine")
        ("integration_station")
        ("spec_proxy")
        ("hub_server")
        ("hub_client")
        (:endgrouptag)

        ("spotify")
        ("talk")
        ("quotes")

        (:startgrouptag) ("tokio")
        (:grouptags) ("tokio_axum")
        (:endgrouptag)

        (:startgrouptag) ("unix")
        (:grouptags) ("coreutils") ("virtual_memory")
        (:endgrouptag))))

;; **********************************************************************
;; Email
;; **********************************************************************

;; Find the mu4e directory relative to the mu directory
(let*
    ((user (getenv "USER"))
     (d0 (format "/etc/profiles/per-user/%s/share/emacs/site-lisp/mu4e" user))
     (d1 "/usr/local/share/emacs/site-lisp/mu4e") ;; local install
     (d2 "/usr/local/share/emacs/site-lisp/mu/mu4e") ;; macos maybe
     (d3 "/usr/share/emacs/site-lisp/mu4e") ;; install from pkg manager
     (mu4e-dir (cond
                ((file-directory-p d0) d0)
                ((file-directory-p d1) d1)
                ((file-directory-p d2) d2)
                ((file-directory-p d3) d3))))
  (add-to-list 'load-path mu4e-dir))

;; refresh the modeline display for unread emails every 5 minuts
(add-hook! 'after-init-hook
  #'(lambda ()
    (run-with-timer 0 300 #'mu4e-alert-enable-mode-line-display)))

(setq my/mu4e-interesting-mail-query "flag:unread AND NOT flag:trashed \
AND (maildir:/gmail/Inbox OR maildir:/spectrust/Inbox)")

(use-package! mu4e
  :config
  (setq
   ;; Don't pull in the entire thread from the archive when it gets a new message
   mu4e-headers-include-related nil
   ;; More space for the headers
   mu4e-headers-visible-lines 20
   ;; systemd mbsync job handles this for us
   mu4e-index-update-in-background nil
   ;; mu4e-maildir "~/.mail"  ;; deprecated, but keeping around for now
   ;; mu4e-root-maildir "~/.mail"
   ;; Simpler threading indicators
   ;; mu4e-headers-thread-child-prefix '("| " . "| ")
   ;; mu4e-headers-thread-last-child-prefix '("| " . "| ")
   ;; mu4e-headers-thread-orphan-prefix '("" . "")
   mu4e-headers-show-threads t
   mu4e-split-view 'vertical
   mu4e-headers-visible-columns 160
   ;; make indexing faster
   ; mu4e-index-cleanup nil
   ; mu4e-index-lazy-check t
   ;; update mail every 5 minutes
   ;; mu4e-split-view 'vertical
   ;; used to display an unread count
   mu4e-alert-interesting-mail-query my/mu4e-interesting-mail-query)

  (map! :map mu4e-headers-mode-map
        :desc "mark thread"
        :nv "T"
        #'mu4e-headers-mark-thread)
  (set-email-account! "gmail"
                      '((user-email-address . "msplanchard@gmail.com")
                        (smtpmail-smtp-user . "msplanchard")
                        (smtpmail-local-domain . "gmail.com")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-default-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-service . 587)
                        (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                        (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")))
  (set-email-account! "spectrust"
                      '((user-email-address . "matthew@spec-trust.com")
                        (smtpmail-smtp-user . "matthew@spec-trust.com")
                        (smtpmail-local-domain . "gmail.com")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-default-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-service . 587)
                        (mu4e-drafts-folder . "/spectrust/[Gmail]/Drafts")
                        (mu4e-refile-folder . "/spectrust/[Gmail]/All Mail")
                        (mu4e-sent-folder . "/spectrust/[Gmail]/Sent Mail")))

  (add-hook! 'mu4e-view-mode-hook #'mp/disable-fill-column-indicator-mode)
  (add-hook! 'mu4e-headers-mode-hook
    #'(lambda () (set-face-background 'mu4e-header-highlight-face "gray18")))

  (add-to-list 'mu4e-bookmarks
               '(:name "Gmail Inbox" :query "maildir:/gmail/Inbox" :key ?g))
  (add-to-list 'mu4e-bookmarks
               '(:name "SpecTrust Inbox" :query "maildir:/spectrust/Inbox" :key ?s))
  (add-to-list 'mu4e-bookmarks
               '(:name "Recent Unread" :query my/mu4e-interesting-mail-query :key ?U))
  (setq mu4e-headers-fields '((:human-date . 12)
                              ;; (:mailing-list . 15)
                              (:flags . 8)
                              (:from . 30)
                              (:subject . nil))))
