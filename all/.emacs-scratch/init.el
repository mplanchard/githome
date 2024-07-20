;; Allow use of the common lisp emulation library for CL functions
(require 'cl-lib)

;; --------------------------------------------------------------------------------
;; MY FUNCTIONS
;; --------------------------------------------------------------------------------
;; My own, my precious
;; --------------------------------------------------------------------------------

;; Return whether we're running on NixOS
(defun my/nixos-p ()
  (let ((sysinfo (shell-command-to-string "uname -v")))
    (not (eq (cl-search "NixOS" sysinfo) nil))))

;; A function to retrieve the build date of emacs, needed for elpaca on NixOS
(defun my/nixos/get-emacs-build-date ()
  (string-match "--prefix.*emacs.*\\([[:digit:]]\\{8\\}\\)" system-configuration-options)
  (let ((config-date (match-string 1 system-configuration-options)))
    (car (read-from-string config-date))))

;; Used for highlight-on-yank for evil mode
;; Cribbed from https://blog.meain.io/2020/emacs-highlight-yanked/
(defun my/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

;; --------------------------------------------------------------------------------
;; ELPACA INSTALL
;; --------------------------------------------------------------------------------

;; If we're running on NixOS, set elpaca-core-date.
;;
;; This value is necessary for elpaca to run, but something about the NixOS build
;; prevents it from collecting this value via its normal flow.
;;
;; See https://github.com/progfolio/elpaca/issues/222 for more info.
(if (my/nixos-p)
    (setq elpaca-core-date (list (my/nixos/get-emacs-build-date))))

;; Everything from here is copied directly from the elpaca readme.

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; --------------------------------------------------------------------------------
;; PACKAGE INSTALL/CONFIG
;; --------------------------------------------------------------------------------

;; Use elpaca's use-package macro, to translate use-package definitions into
;; async package installs.
(elpaca elpaca-use-package
	(elpaca-use-package-mode))

;; Display key prompt popups for keys that follow whichever prefix you just typed
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package helpful :ensure t
  :config
  (general-def
    [remap describe-function] #'helpful-callable
    [remap describe-variable] #'helpful-variable
    [remap describe-key] #'helpful-key
    [remap describe-command] #'helpful-command))

;; Modal editing
(use-package evil :ensure t :demand t
  :custom
  (evil-lookup-func #'helpful-at-point)
  :init
  (setq
   ;; use emacs' native redo for C-r
   evil-undo-system #'undo-redo
   ;; make C-u scroll rather than use as prefix arg
   evil-want-C-u-scroll t
   ;; no need to delay before interpreting esc sequences as such
   evil-esc-delay 0
   ;; don't treat "going right at end of line" or "left at beginning of line" as
   ;; errors (and thus to terminate macro recording/replay)
   evil-kbd-macro-suppress-motion-error t
   ;; Set to nil for evil-collection compatibility
   evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; Flash yanked text when yanking
  (advice-add 'evil-yank :around 'my/evil-yank-advice))

(use-package evil-collection :ensure t
  :after evil
  :custom
  ;; don't interfere w/my leader key
  (evil-collection-key-blacklist '("SPC"))
  :init
  (evil-collection-init))

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode 1))

;; Terminal
(use-package vterm :ensure t)
(use-package toggle-term :ensure t
  :config
  (setq
   ;; make it a little bigger
   toggle-term-size 30
   ;; set vterm as last-used so "toggle" will use it on startup
   toggle-term-last-used '("*vterm-popup*" . vterm)))

;; Pull PATH from default shell into emacs. Very useful in nix environments.
(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :init
  (when (daemonp) (exec-path-from-shell-initialize)))

;; Completion framework
(use-package vertico :ensure t
  :init
  (vertico-mode)
  :config
  (general-def vertico-map
    "C-j" #'vertico-next
    "C-k" #'vertico-previous
    "C-;" #'embark-act
    "C-." #'embark-dwim))
;; Annotations in completion minibuffers
(use-package marginalia :ensure t
  :init
  (marginalia-mode))
(use-package consult :ensure t)
(use-package embark :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (general-define-key
   :states '(global normal visual motion emacs insert)
   "C-;" #'embark-act
   "C-." #'embark-dwim))
(use-package embark-consult :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Use broader matching rather than the default tab completion 
(use-package orderless :ensure t
  :init
  ;; Default config recommended from vertico README
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; In-buffer completion-at-point (i.e. completion popup)
;; By the same author as vertico
(use-package corfu :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t))

;; Magit (VC commands) and forge (interaction with forges)
(use-package magit :ensure t)
(use-package forge :ensure t
  :after magit)
(use-package magit-delta :ensure t
  :hook (magit-mode . magit-delta-mode))
(use-package diff-hl :ensure t
  :config
  (global-diff-hl-mode)
  :hook
  (dired-mode . diff-hl-dired-mode))

(use-package lsp-mode :ensure t
  :config
  (setq
    gc-cons-threshold (* 100 1024 1024) ; 100 MB
    read-process-output-max (* 3 1024 1024) ; 3 MB
    lsp-headerline-breadcrumb-icons-enable nil
    lsp-headerline-breadcrumb-enable-diagnostics nil
    lsp-idle-delay 1
    lsp-lens-enable nil
    lsp-rust-all-features t
    lsp-rust-all-targets t
    lsp-rust-analyzer-check-all-targets t
    lsp-rust-analyzer-display-chaining-hints t
    lsp-rust-analyzer-display-closure-return-type-hints t
    lsp-rust-analyzer-display-parameter-hints t
    lsp-rust-clippy-preference "on"

    ;; Show function signatures while writing functions and types for the thing at point
    lsp-signature-auto-activate t
    ;; I like the function signatures while writing functions, but don't like
    ;; the we way the docs make the little buffer at the bottom pop up distractingly.
    lsp-signature-render-documentation nil)
  (general-def lsp-mode-map
    [remap evil-lookup] #'lsp-describe-thing-at-point)
  :hook
  (rustic-mode-hook . (lsp-inlay-hints-mode #'lsp-deferred)))

(use-package lsp-ui :ensure t
  :config
  (setq
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-peek-enable t
   lsp-ui-peek-show-directory t
   ;; always show a preview before jumping to definition
   lsp-ui-peek-always-show t

   lsp-ui-doc-header t
   lsp-ui-doc-position 'top
   lsp-ui-doc-alignment 'window
   lsp-ui-doc-include-signature t
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-include-signature t)
  (general-def lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
    [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package envrc :ensure t
  :hook
  (after-init . envrc-global-mode))

(use-package rustic :ensure t)

;; Flexible fancy keybinding, with leader key support
(use-package general
  :ensure t
  :init
  (general-define-key
   :states '(global normal visual motion emacs insert)
   :prefix-map 'my/leader-map
   :global-prefix "C-SPC"
   :non-normal-prefix "M-SPC"
   :prefix "SPC")
  (general-define-key
   :states '(normal visual motion)
   :prefix-map 'my/go-map
   :prefix "g")
  
  (general-create-definer my/go-key-def
    :keymaps 'my/go-map)
  (general-create-definer my/leader-key-def
    :keymaps 'my/leader-map)
  
  (defvar my/buffer-map (make-sparse-keymap))
  (general-create-definer my/buffer-key-def :keymaps 'my/buffer-map)

  (defvar my/file-map (make-sparse-keymap))
  (general-create-definer my/file-key-def :keymaps 'my/file-map)

  (defvar my/git-map (make-sparse-keymap))
  (general-create-definer my/git-key-def :keymaps 'my/git-map)

  (defvar my/help-map (make-sparse-keymap))
  (general-create-definer my/help-key-def :keymaps 'my/help-map)

  (defvar my/open-map (make-sparse-keymap))
  (general-create-definer my/open-key-def :keymaps 'my/open-map)

  (defvar my/quit-map (make-sparse-keymap))
  (general-create-definer my/quit-key-def :keymaps 'my/quit-map)

  (defvar my/window-map (make-sparse-keymap))
  (general-create-definer my/window-key-def :keymaps 'my/window-map)
  
  (my/buffer-key-def
   "b" #'consult-project-buffer
   "B" #'consult-buffer
   "d" #'kill-current-buffer
   "n" #'next-buffer
   "p" #'previous-buffer
   "r" #'revert-buffer
   "s" #'save-buffer)
  (my/help-key-def
   "f" #'describe-function
   "k" #'describe-key
   "i" #'info
   "m" #'describe-mode
   "v" #'describe-variable)
  (my/file-key-def
   "f" #'find-file
   "r" #'recentf)
  (my/git-key-def
    "." #'magit-file-dispatch
    "g" #'magit-status
    "n" #'diff-hl-next-hunk
    "p" #'diff-hl-previous-hunk
    "r" #'diff-hl-revert-hunk
    "s" #'diff-hl-stage-dwim
    "S" #'diff-hl-show-hunk
    "u" #'diff-hl-unstage)
  (my/open-key-def
   "t" #'toggle-term-toggle
   "T" #'vterm)
  (my/window-key-def
   "l" #'evil-window-right
   "h" #'evil-window-left
   "j" #'evil-window-down
   "k" #'evil-window-up
   "p" #'evil-window-prev
   "d" #'evil-window-delete
   "v" #'evil-window-vsplit)
  (my/leader-key-def
   "SPC" (cons "project-find-file" #'project-find-file)
   "/" (cons "search" #'consult-ripgrep)
   "b" (cons "buffer" my/buffer-map)
   "f" (cons "file" my/file-map)
   "g" (cons "file" my/git-map)
   "h" (cons "help" my/help-map)
   "o" (cons "open" my/open-map)
   "q" (cons "quit" my/quit-map)
   "w" (cons "window" my/window-map)
   "x" (cons "execute" #'execute-extended-command)
   ":" (cons "execute" #'execute-extended-command)
   "u" (cons "prefix" #'universal-argument))

  (my/go-key-def
    "c" #'comment-dwim
    "d" #'xref-find-definitions
    "D" #'xref-find-references))

;; Emacs settings
(use-package emacs :ensure nil
  :config
  (setq
   ;; I've got to get away from these confounded relatives, hanging on the bell all day
   ;; never giving me a moment's peace
   ring-bell-function #'ignore
   ;; keep it secret, keep it safe
   auth-sources '("~/.authinfo.gpg")
   ;; trigger completion-at-point with TAB
   tab-always-indent 'complete
   ;; hide commands from M-x that don't apply to the current mode
   read-extended-command-predicate #'command-completion-default-include-p
   ;; relative line numbers
   display-line-numbers 'relative
   ;; don't prompt, just follow symbolic links
   vc-follow-symlinks t
   ;; backup all files to a common directory
   backup-directory-alist '("." . (concat (or (getenv "XDG_RUNTIME_DIR") "~/.local") "/emacs-backups"))
   ;; add a newline at the end of files when visiting if they don't already have one
   require-final-newline 'visit
   ;; write to the target, not the symlink, when saving a file opened via symlink
   file-preserve-symlinks-on-save t
   ;; don't tell me every time auto-saving happens
   auto-save-no-message t
   ;; don't recenter every time I scroll offscreen, only if jumping a huge distance
   scroll-conservatively 50
   ;; show matching parenthesis when cursor is either inside or outside the other paren
   show-paren-when-point-inside-paren t
   ;; chemacs-aware user init directory
   my/user-init-dir (if (boundp 'chemacs-profile)
			(alist-get 'user-emacs-directory chemacs-profile)
		      (file-name-directory (user-init-file)))
   ;; set custom file to a file in the chemacs profile dir
   custom-file (file-name-concat my/user-init-dir "custom.el"))

  ;; theme selection
  (load-theme 'modus-vivendi t)
  ;; font settings
  (set-frame-font "ComicShannsMono Nerd Font Mono" nil t)
  ;; height is x10 of usual font size
  (set-face-attribute 'default nil :height 140)
  ;; turn off the toolbar
  (tool-bar-mode -1)
  ;; save minibuffer history
  (savehist-mode)
  ;; turn on line numbers by default
  (global-display-line-numbers-mode)
  ;; turn off scroll bars
  (scroll-bar-mode -1)
  ;; laod customizations
  (load custom-file))
