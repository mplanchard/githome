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

(setq doom-font (font-spec :family "Fira Code" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename org-directory))
;; (setq +org-roam-open-buffer-on-find-file nil)
;; (setq org-agenda-files (list org-directory (file-name-concat org-directory "presentations")))
(setq org-journal-dir (file-name-concat org-directory "journal"))

(setq org-roam-capture-templates
      (list
       '("d" "default" plain "%?"
         :if-new (file+head "${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(setq org-tag-alist (quote ((:startgrouptag)
                            ("software")
                            (:grouptags)
                            ("api")
                            ("database")
                            ("emacs")
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

                            ("coffee")

                            (:startgrouptag)
                            ("database") (:grouptags) ("postgres") ("sqlite") ("mysql")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("api") (:grouptags) ("rest") ("graphql")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("emacs")
                            (:grouptags)
                            ("emacs_config")
                            ("emacs_themes")
                            ("mu4e")
                            ("org_mode")
                            (:endgrouptag)

                            (:startgrouptag) ("org_mode") (:grouptags) ("org_roam") (:endgrouptag)

                            (:startgrouptag)
                            ("people_management")
                            (:grouptags)
                            ("relationships")
                            ("team_dynamics")
                            (:endgrouptag)

                            ("camping")
                            ("possible_purchases")
                            ("possible_gifts")
                            ("talk")

                            (:startgrouptag)
                            ("software_architecture")
                            (:grouptags)
                            ("complexity")
                            ("concurrency")
                            ("design_patterns")
                            ("dynamic_linking")
                            ("file_system_architecture")
                            ("no_silver_bullet")
                            ("software_patterns")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("concurrency")
                            (:grouptags)
                            ("async_await")
                            ("go_statement")
                            ("multithread")
                            ("multiprocess")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("software_patterns")
                            (:grouptags)
                            ("type_driven_development")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("software_security") (:grouptags) ("authentication") ("credentials")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("type_driven_development")
                            (:grouptags)
                            ("parse_dont_validate")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("javascript")
                            (:grouptags)
                            ("react")
                            ("typescript")
                            ("vuejs")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("software_optimization")
                            (:grouptags)
                            ("cache_utilization")
                            ("software_performance")
                            ("software_speed")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("software_people")
                            (:grouptags)
                            ("andrew_gallant")
                            ("dan_luu")
                            ("fred_brooks")
                            ("gary_bernhradt")
                            ("paul_graham")
                            ("rich_hickey")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("software_tools")
                            (:grouptags)
                            ("graphical_applications")
                            ("gui_frameworks")
                            ("parsers")
                            ("shell_applications")
                            (:endgrouptag)

                            ("spotify")
                            ("quotes")

                            (:startgrouptag)
                            ("lisp")
                            (:grouptags)
                            ("clojure")
                            ("elisp")
                            ("racket")
                            ("scheme")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("rust")
                            (:grouptags)
                            ("actix")
                            ("warp")
                            ("rust_analyzer")
                            ("rust_async")
                            ("rust_features")
                            ("rust_web")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("rust_web") (:grouptags) ("actix") ("tokio_axum")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("rust_async") (:grouptags) ("async_std") ("tokio")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("tokio") (:grouptags) ("tokio_axum")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("unix")
                            (:grouptags)
                            ("coreutils")
                            ("virtual_memory")
                            (:endgrouptag)

                            (:startgrouptag)
                            ("spectrust")
                            (:grouptags)
                            ("workflow_engine")
                            ("integration_station")
                            ("spec_proxy")
                            ("hub_server")
                            ("hub_client")
                            (:endgrouptag))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

;; 15 is the original setting, but it seems like it's down to 0.5 via doom or
;; something, so try setting it back up to avoid GC pauses
(setq gcmh-idle-delay 10)


;; **********************************************************************
;; Settings
;; **********************************************************************

(setq enable-local-variables t)
(setq browse-url-browser-function 'browse-url-xdg-open)

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
(setq projectile-project-search-path '("~/s/gh" "~/s/gh/spectrust" "~/s/gh/mplanchard"))

;; Make the ivy serach buffer larger
(setq ivy-height 25)

(after! company
  (setq company-idle-delay 0.01))

(after! evil
  (setq evil-esc-delay 0)
  (setq evil-escape-delay 0)
  (setq evil-escape-mode nil)
  (setq evil-ex-search-case 'smart))

;; LSP Settings and Performance Tuning

;; performance
;; (setq lsp-use-plists t)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1 mb

;; this was a performance problem with python, but not so much wit hrust
;; (setq lsp-enable-file-watchers nil)

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

;; Enable proc-macro expansion
(setq lsp-rust-analyzer-proc-macro-enable t)
;; Use build scripts in the analyzer context
(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
;; Build with --all-features
(setq lsp-rust-all-features t)
;; Build with --test
(setq lsp-rust-cfg-test t)

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
(setq lsp-ui-peek-always-show t)

;; Delay for perf
(setq flycheck-idle-change-delay 1.5)

;; headerline mode fails in ediff, so make sure it doesn't start.
(add-hook!
 '(ediff-prepare-buffer-hook magit-blob-mode-hook)
 (lambda () (lsp-headerline-breadcrumb-mode -1)))

;; Allow lots of flycheck errors
(after! flycheck
  (setq flycheck-checker-error-threshold 1000))

;; honestly I don't use this anymore (dired is better), but I like this theme
(after! neotree
  (setq neo-theme 'ascii))

;; treat camelCase words as two words for spellcheck
(after! ispell
  (setq ispell-extra-args (append '("--camel-case") ispell-extra-args)))

;; Try to improve syntax hihglighting performance for really large files
(after! jit-lock
  ;; defer fontification while input is pending
  (setq jit-lock-defer-time 0)
  ;; When a buffer is idle for some time, go ahead and fontify areas outside
  ;; the view, to avoid work when scrolling
  (setq jit-lock-stealth-time 32))

;; don't try to smooth scroll on mac
(setq mac-mouse-wheel-smooth-scroll nil)

;; Turn on fill-column-indicator mode globally, except for certain modes.
(defun mp/disable-fill-column-indicator-mode ()
  (display-fill-column-indicator-mode 0))
(global-display-fill-column-indicator-mode)
(add-hook! '+doom-dashboard-mode-hook #'mp/disable-fill-column-indicator-mode)
(add-hook! 'vterm-mode-hook #'mp/disable-fill-column-indicator-mode)


;; I use regular escape commands and don't need evil-escape
(setq evil-escape-inhibit t)

(server-start)
(require 'org-protocol)
(let ((t1 `("P" "Protocol" entry (file+headline ,(file-name-concat org-directory "inbox.org"))
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))
      (t2 `("L" "Protocol Link" entry (file ,(file-name-concat org-directory "inbox.org"))
        "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (unless (member t1 org-capture-templates) (add-to-list 'org-capture-templates t1))
  (unless (member t2 org-capture-templates) (add-to-list 'org-capture-templates t2)))

;; org-mode settings
(after! org
  ;; don't add section numbers to headings on export
  (setq org-export-with-section-numbers nil)
  ;; keep quotes as I wrote them, don't automatically use smart quotes
  (setq org-export-with-smart-quotes nil)
  ;; require brackets to use sub/superscripts, so that a straight underline or
  ;; caret doesn't get interpreted as such
  (setq org-export-with-sub-superscripts '{})
  ;; don't automatically add a ToC to exports
  (setq org-export-with-toc nil))

;; org-journal settings
(after! org-journal
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

;; disable autocomplete in org-mode. We can always turn it on, and I don't love
;; autocomplete when I'm just trying to write stuff.
(add-hook! 'org-mode-hook (lambda () (company-mode -1)))

(after! org
  :config
  ;; Allow executing JS code blocks in org
  (require 'ob-js)
  ;; prevent killing the agenda buffer
  (add-hook 'org-agenda-after-show-hook (lambda () (emacs-lock-mode 'kill))))

;; Allow executing TS code blocks in org
(after! ob-typescript
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t))))

;; org-roam
(setq
 deft-directory org-directory
 deft-extensions '("org" "md")
 deft-recursive t)

(setq deft-directory org-directory
      deft-extensions '("org" "md" "txt")
      deft-recursive t)

(after! github-review
  :config
  (add-hook! 'github-review-mode-hook (lambda () (ws-butler-mode -1))))

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

;; use guile for schema programming
(setq scheme-program-name "guile")

(after! yasnippet
  (unless (member "~/s/gnu/guix/etc/snippets" yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs "~/s/gnu/guix/etc/snippets")))

(if (file-exists-p "~/s/gnu/guix/etc/copyright.el")
    (load-file "~/s/gnu/guix/etc/copyright.el"))
(setq copyright-names-regexp "Matthew Planchard <msplanchard@gmail.com>")

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

(use-package! emms
  :config
  (require 'emms-setup)
  (require 'emms-info-tinytag)
  (emms-all)
  (setq emms-info-functions '(emms-info-tinytag))
  (setq emms-player-list '(emms-player-vlc, emms-player-vlc-playlist))
  (setq emms-source-file-default-directory "~/Music")
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (emms-history-load))

;; load and use one of the kaolin themes
(use-package! kaolin-themes
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))

(use-package! gh-notify)

(use-package! dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode t)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
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

(use-package! edit-server
  :commands edit-server-start
  :init
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

(use-package! tree-sitter-langs)

;; Better local syntax highlighting and language analysis
(use-package! tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; **********************************************************************
;; Keybindings
;; **********************************************************************

(map! :map org-mode-map
      :localleader
      :desc "org-insert-structure-template" "T" #'org-insert-structure-template)

(map! (:leader
       :prefix "w"
       :desc "go-to-window" :nv "g" #'ace-window)
      (:leader
       :prefix "w"
       :desc "swap-window" :nv "/" #'ace-swap-window))

(map! :leader
      :desc "paste from kill ring" :nv "P" #'+default/yank-pop)

(map! :prefix "g"
      :desc "show-hover-doc" :nv "h" #'lsp-ui-doc-glance)

;; Replace rustic's cargo check, which opens in a minibuffer popup, to one that
;; opens in a dedicated buffer
(map! :after rustic
      :map rustic-mode-map
      :localleader
      :prefix "b"
      :desc "cargo check"
      :nv "c"
      #'(lambda ()
          (interactive)
          (rustic-run-cargo-command "cargo check --tests" '(:buffer "*cargo-check*"))))

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
       #'lsp-ui-imenu))

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

;; This resolves a weird error when creating a new frame via (make-frame) that
;; I haven't been able to find any info on. Error message is
;; "The default fontset can't be used for a frame font".
(setq default-frame-alist (assq-delete-all 'font default-frame-alist))


;; Somehow recently this started overriding TAB in the magit status buffer.
;; Revisit later to see if it's fixed.
(after! evil
  (define-key evil-motion-state-map (kbd "<tab>") nil))

;; **********************************************************************
;; Email
;; **********************************************************************


;; Ensure we can find mu4e lisp files
(let*
    ((d1 "/usr/local/share/emacs/site-lisp/mu4e") ;; local install
     (d2 "/usr/local/share/emacs/site-lisp/mu/mu4e") ;; macos maybe
     (d3 "/usr/share/emacs/site-lisp/mu4e") ;; install from pkg manager
     (mu4e-dir (cond
                ((file-directory-p d1) d1)
                ((file-directory-p d2) d2)
                ((file-directory-p d3) d3))))
  (add-to-list 'load-path mu4e-dir))


(use-package! mu4e
  :config
  (setq
   ;; Don't pull in the entire thread from the archive when it gets a new message
   mu4e-headers-include-related nil
   ;; More space for the headers
   mu4e-headers-visible-lines 20
   mu4e-maildir "~/.mail"  ;; deprecated, but keeping around for now
   mu4e-root-maildir "~/.mail"
   ;; Simpler threading indicators
   mu4e-headers-thread-child-prefix '("| " . "| ")
   mu4e-headers-thread-last-child-prefix '("| " . "| ")
   mu4e-headers-thread-orphan-prefix '("" . "")
   mu4e-headers-show-threads t
   ;; make indexing faster
   ; mu4e-index-cleanup nil
   ; mu4e-index-lazy-check t
   ;; update mail every 5 minutes
   mu4e-update-interval 300
   ;; mu4e-split-view 'vertical
   ;; used to display an unread count
   mu4e-alert-interesting-mail-query
   "flag:unread AND NOT flag:trashed AND NOT maildir:'/gmail/[Gmail]/All Mail' AND NOT /spectrust/[Gmail]/All Mail")
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
  (add-to-list 'mu4e-bookmarks
               '(:name "Gmail Inbox" :query "maildir:/gmail/Inbox" :key ?g))
  (add-to-list 'mu4e-bookmarks
               '(:name "SpecTrust Inbox" :query "maildir:/spectrust/Inbox" :key ?s))
  ;; (setq mu4e-headers-fields '((:account . 8)
  ;;                             (:flags . 4)
  ;;                             (:mailing-list . 12)
  ;;                             (:from . 22)
  ;;                             (:human-date . 12)
  ;;                             (:subject . nil))))
  (setq mu4e-headers-fields '((:flags . 4)
                              (:from . 22)
                              (:subject . 64)
                              (:mailing-list . 12)
                              (:human-date . 12))))

(use-package! mu4e-views
  :config
  (setq mu4e-views-completion-method 'ivy)
  (setq mu4e-views-next-previous-message-behavior 'stick-to-current-window))


;; Send HTML messages by default.
(after! org-msg
  (setq org-msg-default-alternatives '(text html)))

(use-package! magit
  :config
  ;; Show local branches in magit status buffer
  (unless
      (member 'magit-insert-local-branches magit-status-sections-hook)
    (setq magit-status-sections-hook (append magit-status-sections-hook '(magit-insert-local-branches)))))


(defun mp-email-empty-trash ()
  "Empty the mu4e trash directory of anything older than 10 days old"
  (interactive)
  (async-shell-command
   (format
    "%s find maildir:/trash AND date:10d..1000d --fields=l | xargs rm -f"
    mu4e-mu-binary)))


(after! markdown-mode
  (setq markdown-marginalize-headers t)
  (setq markdown-header-scaling t)
  (setq markdown-toc-header-toc-start "<!-- markdown-toc start -->"))

;; Disable autocomplete in markdown mode and gfm mode. I don't love autocomplete
;; when I'm just trying to write stuff.
(add-hook! 'markdown-mode-hook (lambda () (company-mode -1)))
(add-hook! 'gfm-mode-hook (lambda () (company-mode -1)))

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

(use-package! python-black
  :after python)

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
;; Custom Functions
;; **********************************************************************

(setq mp/default-github-org "SpecTrust-Inc")
(setq mp/default-github-repo "spec-protect")

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


(defun mp-bestow-db (dbenv dbuser)
  (interactive "sEnvironment: \nsUser: ")
  ;; Open a tunnel to the DB
  (shell-command
   (format
    "source ~/.pyenv/take-two/bin/activate && cd ~/github/bestowinc/take-two/ && ~/github/bestowinc/take-two/go db tunnel %s &"
    dbenv))
  ;; ensure we have had time to establish the tunnel
  (message "Establishing a tunnel...")
  (sleep-for 10)
  ;; Decrypt the password and set up teh sql-connection-alist variable so that
  ;; it's set to connect to the DB
  (let*
      ((cmdstr
        (format
         "sops --decrypt --extract %s %s"
         (format "'[\"%s\"]'"
                 (cond
                  ((string-equal dbuser "enrollment-ro") "ENROLLMENT_READ_ONLY_PASSWORD")
                  ((string-equal dbuser "enrollment-rw") "ENROLLMENT_READ_WRITE_PASSWORD")
                  ((string-equal dbuser "enrollment-owner") "ENROLLMENT_OWNER_PASSWORD")
                  (t (throw 'no-user "No such user"))))
         (format
          "~/github/bestowinc/spellbook/.kubernetes/%s/encrypted/environment.yaml"
          dbenv)))
       (password (shell-command-to-string cmdstr))
       (sql-connection-alist
        (list (list
               (concat "bestow-db-" dbenv)
               '(sql-product 'postgres)
               '(sql-server "localhost")
               '(sql-port 5433)
               (list 'sql-user dbuser)
               (list 'sql-database
                     (format
                      "postgresql://%s:%s@localhost:5433/enrollment"
                      dbuser
                      password))))))
    (sql-connect (concat "bestow-db-" dbenv))))


(defun mp-echo-async-run-shell-command (command)
  "Echo the shell command to the *Async Shell Command* buffer, then run it, opening the buffer."
  (async-shell-command (format "echo '%s' && %s" command command))
  (get-buffer "*Async Shell Command*"))


(defun mp-githome (git_command)
  (interactive "sGit Command: ")
  (mp-echo-async-run-shell-command
   (format
    "git --git-dir $HOME/.dotfiles --work-tree=$HOME %s"
    git_command)))


(defun mp-run-system-setup ()
  "Run my system setup script."
  (interactive)
  (mp-echo-async-run-shell-command "bash ~/scripts/setup.sh"))


(defun mp-githome-pull ()
  "Pull any changes to the githome"
  (interactive)
  (mp-githome "pull"))

(defun mp-take-two-deploy-list (&optional target)
  "Get a list of commits to deploy from up to TARGET, or master/HEAD if not provided"
  (interactive "sTarget [default current staging SHA]: ")
  (let ((current
         ;; grab the current deployed
         (cdr (assoc 'commit_sha
                     (with-current-buffer (url-retrieve-synchronously "https://api.hellobestow.com/version")
                       (goto-char url-http-end-of-headers)
                       (json-read)))))
        (target
         (if (string-empty-p target)
             (cdr (assoc 'commit_sha
                         (with-current-buffer (url-retrieve-synchronously "https://api.stage.bestow.io/version")
                           (goto-char url-http-end-of-headers)
                           (json-read))))
           target)))
    ;; ensure we're up to date with origin
    (magit-git-fetch "origin" "master")
    ;; output git log to buffer (note we could use ~magit-log-other~ here, but
    ;; I cannot figure out how to copy the git logs in a way that includes the
    ;; author, even though it shows up in the log view. Since copying and pasting
    ;; the logs into slack and tagging people for approval is a critical part of
    ;; this workflow, we're sticking with running the shell command)
    (shell-command
     (format
      "git log --graph --pretty=format:'%%h - %%d %%s (%%cr) (%%an)' %s..%s --abbrev-commit"
      current
      target))))

(defun my/convert (value-string unit-string)
  "Convert VALUE-STRING to the target UNIT-STRING.

This simple function is just here to make non-interactive unit conversion easier.
For interactive conversion, use `(calc-convert-units)'."
  (calc-eval
   (math-convert-units (calc-eval value-string 'raw) (calc-eval unit-string 'raw))))

(defun my/enhance (count) (interactive "p") (doom/increase-font-size count))

;; (defun my/aws-login ()
;;   (interactive)
;;   )


;; (defun my/one-password-login ()
;;   (interactive))

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
