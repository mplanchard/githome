;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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

(setq doom-font (font-spec :family "Ubuntu Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "sans"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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
;; Settings
;; **********************************************************************

(setq typescript-indent-level 2)
(setq js-indent-level 2)
(setq rustic-indent-offset 4)
(setq tab-width 4)

(setq ispell-dictionary "en_US")

;; Autosave when losing focus
;; (add-to-list 'doom-switch-buffer-hook (lambda () (when buffer-file-name (save-buffer))))
;; (add-to-list 'doom-switch-window-hook (lambda () (when buffer-file-name (save-buffer))))
;; (add-to-list 'doom-switch-frame-hook (lambda () (when buffer-file-name (save-buffer))))

;; Search the GH directory for projects by default
(setq projectile-project-search-path '("~/github/"))

;; Make the ivy serach buffer larger
(setq ivy-height 25)

(after! company
  ;; start showing completion results asap
  (setq company-idle-delay 0.25))

(after! evil
  (setq evil-esc-delay 0)
  (setq evil-escape-delay 0)
  (setq evil-escape-mode nil)
  (setq evil-ex-search-case 'smart))

;; LSP Settings and Performance Tuning
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1 mb

(setq lsp-enable-file-watchers nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-idle-delay 1)
(setq lsp-signature-auto-activate nil)
(setq lsp-modeline-code-actions-enable nil)

(setq lsp-ui-sideline-delay 0.75)
(setq lsp-ui-doc-delay 0.75)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-max-width 150)
(setq lsp-ui-doc-max-height 16)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-sideline-diagnostic-max-lines 20)
(setq lsp-ui-sideline-show-code-actions nil)

(setq flycheck-idle-change-delay 1.5)

;; Allow lots of flycheck errors
(after! flycheck
  (setq flycheck-checker-error-threshold 1000))

(after! neotree
  (setq neo-theme 'ascii))

(after! ispell
  (setq ispell-extra-args (append '("--camel-case") ispell-extra-args)))

(after! jit-lock
  ;; defer fontification while input is pending
  (setq jit-lock-defer-time 0)
  ;; When a buffer is idle for some time, go ahead and fontify areas outside
  ;; the view, to avoid work when scrolling
  (setq jit-lock-stealth-time 32))

(setq mac-mouse-wheel-smooth-scroll nil)

;; Fill the 80th column to let me know I've gone too far
(setq global-hl-fill-column-mode t)

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

;; Allow executing JS code blocks in org
(require 'ob-js)

;; Allow executing TS code blocks in org
(after! ob-typescript
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t))))

;; org-roam
(after! org-roam
  (setq org-roam-directory (file-truename org-directory)))
(setq
 deft-directory org-directory
 deft-extensions '("org" "md")
 deft-recursive t)

(setq deft-directory org-directory
      deft-extensions '("org" "md" "txt")
      deft-recursive t)

;; Rust-related LSP settings
(setq rustic-format-on-save t)
(setq lsp-rust-analyzer-proc-macro-enable t)
(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
(setq lsp-rust-all-features t)
(setq lsp-rust-cfg-test t)

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


;; **********************************************************************
;; Packages
;; **********************************************************************

;; Automatically load .envrc files whenever possible
(use-package! direnv :config (direnv-mode))
;; Use this explicitly because I like using it to jump around when I have lots
;; of windows open. Keybinding for ace-window is set up below.
(use-package! ace-window)
;; this package ensures that whatever PATH additions are a part of my standard
;; bash config wind up in emacs
(use-package! exec-path-from-shell
  ;; only use when in a nix-ish non-terminal app
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

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
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

;; Use bar and block cursor in terminal emacs rather than just block
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

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
       :desc "ace-window" :nv "/" #'ace-window)
      ;; Sometimes these get mapped to evil-next-visual-line and evil-next-previous-line
      ;; in operator mode, which makes the behavior of ~dj~ and ~dk~ and friends
      ;; very odd. Manually set them to their original mappings.
      (:desc "down" :o "j" #'evil-next-line)
      (:desc "up" :o "k" #'evil-previous-line))

(map! :leader
      :desc "paste from kill ring" :nv "P" #'counsel-yank-pop)

(map! :prefix "g"
      :desc "show-hover-doc" :nv "h" #'lsp-ui-doc-glance)

(map! :after mu4e
      :map mu4e-headers-mode-map
      :desc "mark thread"
      :nv "T"
      #'mu4e-headers-mark-thread)

(map! :map ivy-minibuffer-map
      :desc "Search History" "C-r" #'counsel-minibuffer-history)

(map! :map ivy-occur-mode-map
      ;; normally this is f, but also the evil commands tend to override it.
      ;; make it RET instead and ensure it doesn't get overridden
      :desc "ivy-occur-press" :nv "RET" #'ivy-occur-press
      :desc "ivy-occur-press-and-switch" :prefix "g" :nv "o" #'ivy-occur-press-and-switch)

(map! :map ivy-occur-grep-mode-map
      ;; normally this is f, but also the evil commands tend to override it.
      ;; make it RET instead and ensure it doesn't get overridden
      :desc "ivy-occur-press" :nv "RET" #'ivy-occur-press
      :desc "ivy-occur-press-and-switch" :prefix "g" :nv "o" #'ivy-occur-press-and-switch)

(evil-make-overriding-map ivy-occur-mode-map 'normal)
(evil-make-overriding-map ivy-occur-grep-mode-map 'normal)

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

(map! (:after lsp-ui
       :leader
       :prefix "c"
       :desc "Show code outline"
       :nv "O"
       #'lsp-ui-imenu))

(map!
 (:after vterm
  (:map vterm-mode-map
   :desc "send up in insert mode" :i "C-k" #'vterm-send-up)
  (:map vterm-mode-map
   :desc "send down in insert mode" :i "C-j" #'vterm-send-down)))

;; Add a "rerun tests" command to the local test command options
(map!
 (:map rustic-mode-map
  :nv "?" #'rustic-popup
  (:localleader :prefix "t" :desc "rerun tests" :nv "r" #'rustic-cargo-test-rerun)))

;; Very weird to me that these aren't defined by default for the rustic popup
(map!
 (:map rustic-popup-mode-map
  :nv "b" #'rustic-cargo-build
  :nv "r" #'rustic-cargo-run
  :nv "c" #'rustic-cargo-clippy
  :nv "o" #'rustic-cargo-outdated
  :nv "e" #'rustic-cargo-clean
  :nv "t" #'rustic-cargo-test
  :nv "d" #'rustic-cargo-doc))


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

(if (string-equal system-type "darwin")
    (lambda ()
      ;; Add homebrew-installed mu's mu4e path to the load path
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
      ;; adding this by suggestion from https://magit.vc/manual/magit/MacOS-Performance.html
      (setq magit-git-executable "/usr/local/bin/git"))
  ;; Add snap-installed mu4e path to load path (TODO: make ubuntu specific)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))

(use-package! mu4e
  :init
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
   ;; make indexing faster
   mu4e-index-cleanup nil
   mu4e-index-lazy-check t
   ;; update mail every 5 minutes
   mu4e-update-interval 300)
  :config
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
  ;; (set-email-account! "work"
  ;;                     '((user-email-address . "matthew@bestow.com")
  ;;                       (smtpmail-smtp-user . "matthew@bestow.com")
  ;;                       (smtpmail-local-domain . "gmail.com")
  ;;                       (smtpmail-smtp-server . "smtp.gmail.com")
  ;;                       (smtpmail-default-smtp-server . "smtp.gmail.com")
  ;;                       (smtpmail-smtp-service . 587)
  ;;                       (mu4e-drafts-folder . "/work/[Gmail]/Drafts")
  ;;                       (mu4e-refile-folder . "/work/[Gmail]/All Mail")
  ;;                       (mu4e-sent-folder . "/work/[Gmail]/Sent Mail")))
  (add-to-list 'mu4e-bookmarks
               '(:name "Global Inbox"
                 :key ?i
                 :query "maildir:/work/Inbox OR maildir:/gmail/Inbox AND NOT flag:trashed")
               '(:name "Gmail and Sent"
                 :key ?g
                 :query "maildir:/gmail/Inbox OR \"maildir:/gmail/[Gmail]/Sent Mail\" AND NOT flag:trashed"))
  (add-hook 'mu4e-view-mode-hook #'visual-fill-column-mode)
  ;; (setq mu4e-headers-fields '((:account . 8)
  ;;                             (:flags . 4)
  ;;                             (:mailing-list . 12)
  ;;                             (:from . 22)
  ;;                             (:human-date . 12)
  ;;                             (:subject . nil))))
  (setq mu4e-headers-fields '((:flags . 4)
                              (:mailing-list . 12)
                              (:from . 22)
                              (:human-date . 12)
                              (:subject . nil))))

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
  (setq magit-status-sections-hook (append magit-status-sections-hook '(magit-insert-local-branches))))


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
   (flycheck-add-next-checker 'lsp 'python-flake8)
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

(setq mp-default-github-org "bestowinc")


(defun mp-parse-github-pr-target (target)
  "Parse the given GitHub PR TARGET into a URL

A TARGET is something that GitHub would automatically recognize as a GitHub
link when writing an issue or PR, such as myrepo#23 or someorg/myrepo#23.

If the TARGET does not contain an org name, the value of `mp-default-github-org'
will be used as the org name."
  ;; let*, as opposed to let, ensures that the variables are bound sequentially,
  ;; so that each bound variable is available in the context of the next
  ;; variable being bound
  (let* ((split-url (split-string target "[#/]" t "[[:space:]]+"))
         (split-len (length split-url)))
    (multiple-value-bind
        (org repo id)
        (if (eq split-len 3)
            (list (pop split-url) (pop split-url) (pop split-url))
          (list mp-default-github-org (pop split-url) (pop split-url)))
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

;; **********************************************************************
;; Externally Sourced Functions
;; **********************************************************************

