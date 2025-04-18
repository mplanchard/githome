;;; init.el --- MP's Emacs Config -*- lexical-binding: t; -*-

;; Commentary:

;; This is my from-scratch Emacs config.  I am coming from Doom, so some
;; aspects of Doom are mirrored here.  In general, the config attempts to:
;; - Use native Emacs constructs over third-party packages
;; - Create keybindings approximately equivalent to doom
;; - Prioritize Emacs startup time and general editing performance
;;
;; There has not been much if any effort expended in ensuring that this
;; config is generalizable to any use cases other than my own, so caveat
;; emptor if you are pulling from it.

(require 'cl-lib)

;;; Code:

;; --------------------------------------------------------------------------------
;; MY FUNCTIONS
;; --------------------------------------------------------------------------------
;; My own, my precious
;; --------------------------------------------------------------------------------

(defun my/nixos-p ()
  "Return whether we are running on NixOS."
  (let ((sysinfo (shell-command-to-string "uname -v")))
    (not (eq (cl-search "NixOS" sysinfo) nil))))

;; A function to retrieve the build date of emacs, needed for elpaca on NixOS
(defun my/nixos/get-emacs-build-date ()
  "Get the build date of Emacs on a nixos system.  Used for elpaca."
  (string-match "--prefix.*emacs.*\\([[:digit:]]\\{8\\}\\)" system-configuration-options)
  (let ((config-date (match-string 1 system-configuration-options)))
    (car (read-from-string config-date))))

;; Used for highlight-on-yank for evil mode
;; Cribbed from https://blog.meain.io/2020/emacs-highlight-yanked/
(defun my/evil-yank-advice (orig-fn beg end &rest args)
  "Pulse the region being yanked.

Call ORIG-FN with arguments BEG and END, along with any other ARGS.
Prior to calling, pulse the region between BEG and END."
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(defun my/copy-relative-file-path ()
  "Copy the path to the current file, relative to the project root if applicable."
  (interactive)
  (let ((project (project-current))
        (fname (buffer-file-name)))
    (let ((relname (if (and project fname)
                       (file-relative-name fname (project-root project))
                     (buffer-file-name))))
      (kill-new relname)
      (message relname))))

(defun my/delete-visited-file ()
  "Delete the file visited by the current buffer, asking for confirmation."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file
             (y-or-n-p (format "Delete file: %s? " file)))
        (if (vc-backend file)
            ;; if in a git repo, use vc-delete-file to also handle
            ;; marking as deleted in git & syncing
            (funcall-interactively #'vc-delete-file file)
          ;; otherwise just delete it and the visiting buffer
          (progn
            (funcall-interactively #'delete-file file)
            (kill-buffer)))
      ;; Give some indication that we did nothing
      (message "Buffer is not currently visiting a file"))))

;; Split windows and then balance them
(defun my/split-and-balance ()
  "Split window horizontally and rebalance the group."
  (interactive)
  (call-interactively #'split-window-vertically)
  (balance-windows (window-parent)))

;; Split windows vertically and then balance them
(defun my/vsplit-and-balance ()
  "Split window vertically and rebalance the group."
  (interactive)
  (call-interactively #'split-window-horizontally)
  (balance-windows (window-parent)))

;; split, then move focus to the new window
(defun my/split-balance-and-follow ()
  "Split window horizontally and follow."
  (interactive)
  (call-interactively #'my/split-and-balance)
  (select-window (next-window)))

(defun my/vsplit-balance-and-follow ()
  "Split window vertically and follow."
  (interactive)
  (call-interactively #'my/vsplit-and-balance)
  (select-window (next-window)))

(defun my/delete-other-vertical-windows ()
  "Maximize the current window vertically."
  (interactive)
  ;; There may be a less awkward way to do this, but this is the best
  ;; I've got so far, since "next sibling" doesn't wrap around (and
  ;; so doesn't work if not maximizing from the top child).

  ;; Get refs to the current window & its parent
  (let ((to-maximize (selected-window))
        (parent (window-parent)))
    ;; Check if we are part of a vertical group. If we aren't, there's
    ;; nothing to do.
    (when (window-combined-p to-maximize)
      ;; Select the top child of the vertical group. If it is not the
      ;; window we're wanting to maximize, delete it and re-select
      ;; the top child. Do this until we have deleted all windows
      ;; above the one we're trying to maximize.
      (select-window (window-top-child parent))
      (while (not (eq (selected-window) to-maximize))
        (delete-window)
        (select-window (window-top-child parent)))
      ;; Then delete any siblings following the window we're trying to
      ;; maximize.
      (while-let ((sibling (window-next-sibling)))
        (delete-window sibling)))))

(defun my/vterm-new-here ()
  "Create a new vterm in the current buffer."
  (interactive)
  ;; always open a new one rather than trying to find existing one
  (let ((current-prefix-arg '(nil)))
    (call-interactively #'vterm)))

(defun my/vterm-project (&optional arg)
  "Create a new vterm instance in the project root or switch to an existing one.

If not in a project, uses the current directory.

Passes `ARG' to `vterm':
- A nonnumeric prefix arg creates a new session.
- A numeric prefix args switches to a session with that number or creates it.
- A string prefix arg creates a new session with the specified buffer name."
  (interactive "P")
  ;; Avoid "defining as dynamic an already lexical var" error
  (defvar vterm-buffer-name)
  (let* ((default-directory (or (project-root (project-current)) default-directory))
         (vterm-buffer-name (format "%s-vterm" (or (project-name (project-current)) "general"))))
    (ignore vterm-buffer-name)
    (vterm arg)))

(defun my/vterm-project-new ()
  "Create a new vterm instance in the project root."
  (interactive)
  (my/vterm-project '(nil)))

(defun my/eglot-set-rust-analyzer-config ()
  "Set rust-analyzer config for eglot."
  (let ((rust-analyzer-config
         '("rust-analyzer"
           :initializationOptions
           (:cargo
            ;; use --all-features, build in different target dir to avoid
            ;; recompilation at the expense of disk space
            (:features "all" :targetDir t)
            :check
            ;; use clippy
            (:command "clippy" :extraArgs ["--benches" "--tests"])
            :inlayHints
            (:closureReturnTypeHints
             (:enable t))))))
    (add-to-list
     'eglot-server-programs
     `((rustic-mode rust-ts-mode rust-mode) . ,rust-analyzer-config))))

(defun my/evil-shift-right ()
  "Shift right, keeping selection."
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/evil-shift-left ()
  "Shift right, keeping selection."
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defvar my/popterm-shell-fn #'my/vterm-project
  "The function to run after opening the popterm window.")

(defvar my/popterm-split-position 'below
  "One of `'below', `'above', `'left', or `'right', relative to the frame.")

(defvar my/popterm-size -20
  "The size of the new popterm window.

If `my/popterm-split-position' is `'below' or `'above', this means the
number of lines to devote to the popterm window, if negative, or the
number of lines to keep reserved for the pre-split window, if positive.

If `my/popterm-split-position' is `'left'` or `'right', this means
the number of columns to devote to the popterm window, if negative, or
the number of columns to reserve for the pre-split window, if positive.")

(defun my/popterm-buffer-name ()
  "Get a buffer name for the popterm."
  (let* ((proj (project-current))
         (proj-name (when proj (project-name proj)))
         (term-buffer-name (if proj-name
                               (format " *%s-popterm*" proj-name)
                             " *popterm*")))
    term-buffer-name))

(defun my/popterm-show ()
  "Create a new popterm with the specified `POPTERM-FN'."
  (let* ((term-buffer-name (my/popterm-buffer-name))
         (term-buffer (get-buffer term-buffer-name))
         (term-window (split-window (frame-root-window)
                                    my/popterm-size
                                    my/popterm-split-position)))
    (select-window term-window)
    (if term-buffer
        (set-window-buffer term-window term-buffer)
      (call-interactively my/popterm-shell-fn)
      ;; For whatever reason this extra call seems necessasry to avoid
      ;; accidentally renaming the buffer we were in prior to creating
      ;; the new window.
      (select-window term-window)
      (rename-buffer term-buffer-name))
    ;; Don't allow other things to override the window
    ;; (set-window-dedicated-p (selected-window) t)
    ;; Prevent splitting of the term window
    (set-frame-parameter nil 'unsplittable t)))

(defun my/popterm-hide ()
  "Hide the popterm window if it is showing."
  (let* ((term-buffer-name (my/popterm-buffer-name))
         (term-window (get-buffer-window term-buffer-name)))
    (when term-window
      (select-window term-window)
      (bury-buffer)
      (delete-window))))

(defun my/popterm-toggle ()
  "Toggle a project terminal."
  (interactive)
  (let* ((term-buffer-name (my/popterm-buffer-name))
         (term-window (get-buffer-window term-buffer-name)))
    (if term-window
        (my/popterm-hide)
      (my/popterm-show))))

(defun my/restart-server ()
  "Restart the Emacs server, forcing it to be associated with the current process."
  (interactive)
  (when (server-running-p) (server-force-delete))
  (server-start))

;; --------------------------------------------------------------------------------
;; ELPACA INSTALL
;; --------------------------------------------------------------------------------

;; If we're running on NixOS, set elpaca-core-date.
;;
;; This value is necessary for elpaca to run, but something about the NixOS build
;; prevents it from collecting this value via its normal flow.
;;
;; See https://github.com/progfolio/elpaca/issues/222 for more info.
;; (if (my/nixos-p)
;;     (setq elpaca-core-date (list (my/nixos/get-emacs-build-date))))

;; Everything from here is copied directly from the elpaca readme.
(defvar elpaca-installer-version 0.8)
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
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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
;; CONSTANTS
;; --------------------------------------------------------------------------------

(setq my/leader-key "SPC")

;; --------------------------------------------------------------------------------
;; THIRD PARTY PACKAGE INSTALL/CONFIG
;; --------------------------------------------------------------------------------

;; Use elpaca's use-package macro, to translate use-package definitions into
;; async package installs.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; -------------------------------------------------------------------
;; Keybindings
;; -------------------------------------------------------------------

;; Display key prompt popups for keys that follow whichever prefix you just typed
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Flexible fancy keybinding, with leader key support
(use-package general
  :demand t
  :ensure t
  :init
  (general-define-key
   :states '(global normal visual motion emacs insert)
   :prefix-map 'my/leader-map
   :global-prefix (format "C-%s" my/leader-key)
   :non-normal-prefix (format "M-%s" my/leader-key)
   :prefix my/leader-key)
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

  (defvar my/error-map (make-sparse-keymap))
  (general-create-definer my/error-map-key-def :keymaps 'my/error-map)

  (defvar my/file-map (make-sparse-keymap))
  (general-create-definer my/file-key-def :keymaps 'my/file-map)

  (defvar my/git-map (make-sparse-keymap))
  (general-create-definer my/git-key-def :keymaps 'my/git-map)

  (defvar my/help-map (make-sparse-keymap))
  (general-create-definer my/help-key-def :keymaps 'my/help-map)

  (defvar my/open-map (make-sparse-keymap))
  (general-create-definer my/open-key-def :keymaps 'my/open-map)

  (defvar my/project-map (make-sparse-keymap))
  (general-create-definer my/project-key-def :keymaps 'my/project-map)

  (defvar my/search-map (make-sparse-keymap))
  (general-create-definer my/search-key-def :keymaps 'my/search-map)

  (defvar my/quit-map (make-sparse-keymap))
  (general-create-definer my/quit-key-def :keymaps 'my/quit-map)

  (defvar my/toggle-map (make-sparse-keymap))
  (general-create-definer my/toggle-key-def :keymaps 'my/toggle-map)

  (defvar my/window-map (make-sparse-keymap))
  (general-create-definer my/window-key-def :keymaps 'my/window-map)

  (defvar my/window-maximize-map (make-sparse-keymap))
  (general-create-definer my/window-maximize-key-def :keymaps 'my/window-maximize-map)

  (my/buffer-key-def
    "b" #'consult-project-buffer
    "B" #'consult-buffer
    ;; note: `kill-current-buffer' leaves the buffer around in the buffer
    ;; list sometimes for some reason, but this works reliably.
    "d" #'(lambda () (interactive) (kill-buffer (current-buffer)))
    "i" #'ibuffer
    "n" #'next-buffer
    "p" #'previous-buffer
    "r" #'revert-buffer
    "s" #'save-buffer
    "S" (cons "save all buffers" #'(lambda ()
            (interactive)
            (let ((current-prefix-arg '(nil)))
              (call-interactively #'save-some-buffers))))
    "x" #'scratch-buffer)
  (my/error-map-key-def
    "]" #'flymake-goto-next-error
    "[" #'flymake-goto-prev-error
    "x" #'consult-flymake
    "X" #'flymake-show-project-diagnostics)
  (my/file-key-def
    "D" (cons "delete file" #'my/delete-visited-file)
    "f" #'find-file
    "r" #'recentf
    "R" #'rename-visited-file)
  (my/git-key-def
    "." #'magit-file-dispatch
    "[" #'diff-hl-previous-hunk
    "]" #'diff-hl-next-hunk
    "g" #'magit-status
    "n" #'diff-hl-next-hunk
    "p" #'diff-hl-previous-hunk
    "r" #'diff-hl-revert-hunk
    "s" #'diff-hl-stage-dwim
    "S" #'diff-hl-show-hunk
    "u" #'diff-hl-unstage)
  (my/help-key-def
    "f" #'describe-function
    "k" #'describe-key
    "i" #'info
    "m" #'describe-mode
    "v" #'describe-variable)
  (my/open-key-def
    "t" (cons "toggle terminal" #'my/popterm-toggle)
    "T" (cons "terminal here" #'my/vterm-project))
  (my/project-key-def
    "b" #'consult-project-buffer
    "p" #'project-switch-project)
  (my/search-key-def
    "i" #'info-apropos)
  (my/toggle-key-def
    ;; word wrap, essentially
    "w" #'toggle-truncate-lines)
  (my/window-key-def
    "=" #'balance-windows
    "d" #'delete-window
    "l" #'windmove-right
    "h" #'windmove-left
    "j" #'windmove-down
    "k" #'windmove-up
    "m" (cons "maximize" my/window-maximize-map)
    "p" #'evil-window-prev
    "r" #'winner-redo
    "s" (cons "split "#'my/split-and-balance)
    "S" (cons "split and follow" #'my/split-balance-and-follow)
    "u" #'winner-undo
    "v" (cons "vsplit" #'my/vsplit-and-balance)
    "V" (cons "vsplit and follow" #'my/vsplit-balance-and-follow))
  (my/window-maximize-key-def
    "m" (cons "maximize window" #'delete-other-windows)
    "v" (cons "maximize vertically" #'my/delete-other-vertical-windows))
  (my/leader-key-def
    "SPC" (cons "project-find-file" #'project-find-file)
    "/" (cons "search" #'consult-ripgrep)
    "b" (cons "buffer" my/buffer-map)
    "e" (cons "errors" my/error-map)
    "f" (cons "file" my/file-map)
    "g" (cons "git" my/git-map)
    "h" (cons "help" help-map)
    "o" (cons "open" my/open-map)
    "p" (cons "project" my/project-map)
    "q" (cons "quit" my/quit-map)
    "s" (cons "project" my/search-map)
    "t" (cons "toggle" my/toggle-map)
    "w" (cons "window" my/window-map)
    "x" (cons "execute" #'execute-extended-command)
    ":" (cons "execute" #'execute-extended-command)
    "u" (cons "prefix" #'universal-argument))

  (my/go-key-def
    "c" #'comment-dwim
    "d" #'xref-find-definitions
    "r" #'xref-find-references
    "g" #'evil-goto-first-line
    "v" #'evil-visual-restore))

;; Modal editing
(use-package evil :ensure t
  :demand t
  :after general
  :custom
  ;; make lookup with K more consistently useful
  (evil-lookup-func #'helpful-at-point)
  :init
  (setq
   ;; use emacs' native redo for C-r
   evil-undo-system #'undo-redo
   ;; make C-u scroll rather than use as prefix arg
   evil-want-C-u-scroll t
   ;; no need to delay before interpreting esc sequences as such
   evil-esc-delay 0
   evil-respect-visual-line-mode t
   ;; don't treat "going right at end of line" or "left at beginning of line" as
   ;; errors (and thus to terminate macro recording/replay)
   evil-kbd-macro-suppress-motion-error t
   ;; Set to nil for evil-collection compatibility
   evil-want-keybinding nil)
  :config
  ;; where possible, use builtin stuff, but evil does provide some
  ;; things that require extra code to manage with emacs builtins
  (evil-mode 1)
  ;; Flash yanked text when yanking
  (advice-add 'evil-yank :around 'my/evil-yank-advice)
  (add-to-list 'evil-insert-state-modes 'git-commit-mode))

(use-package evil-collection :ensure t
  :after evil
  :custom
  ;; don't interfere w/my leader key
  (evil-collection-key-blacklist
   `(,my/leader-key
     "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  :init
  (evil-collection-init))

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode 1))

;; -------------------------------------------------------------------
;; Terminal and Adjacent
;; -------------------------------------------------------------------

(use-package eat :ensure t
  :config
  (evil-collection-eat-setup))

;; Terminal
(use-package vterm :ensure t
  :defer t
  :commands (vterm)
  :hook
  (vterm-mode . (lambda () display-line-numbers-mode -1))
  :custom
  (vterm-max-scrollback 100000)
  :config
  (general-def vterm-mode-map
    :states 'insert
    "C-j" #'(lambda () (interactive) (vterm-send-key "<down>"))
    "C-k" #'(lambda () (interactive) (vterm-send-key "<up>"))))

;; Pull PATH from default shell into emacs. Very useful in nix environments.
(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :init
  ;; only run when in graphical mode, essentially
  (when (daemonp) (exec-path-from-shell-initialize)))

;; -------------------------------------------------------------------
;; Completion, Search, Help
;; -------------------------------------------------------------------

(use-package ace-window :ensure t
  :after general
  :config
  (my/window-key-def
    "g" #'ace-window
    "/" #'ace-swap-window))

(use-package avy :ensure t
  :after (embark general)
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  (my/go-key-def
    "s /" #'avy-goto-char-timer
    "s k" #'evil-avy-goto-line-above
    "s j" #'evil-avy-goto-line-below
    "s r" #'avy-resume
    "s y y" #'avy-kill-ring-save-whole-line
    "s Y" #'avy-kill-ring-save-region))

;; Completion framework
(use-package vertico :ensure t
  :after general
  :init (vertico-mode)
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

;; Interactive search from a variety of sources, e.g. ripgrep
(use-package consult :ensure t
  :defer t
  :custom
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :commands (consult-project-buffer consult-buffer consult-project-buffer))

(use-package consult-todo :ensure t)

;; Right-click contextual interface via the keyboard, essentially
(use-package embark :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (general-define-key
   :states '(global normal visual motion emacs insert)
   "C-;" #'embark-act
   "C-." #'embark-dwim)
  (general-def 'embark-file-map
    "F" #'find-file-other-window))
(use-package embark-consult :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep :ensure t)

;; Use broader matching rather than the default tab completion
(use-package orderless :ensure t
  :init
  ;; Default config recommended from vertico README
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Company is much faster with eglot than corfu, for whatever reason.
;; See refs:
;; - https://github.com/joaotavora/eglot/discussions/993
;; - https://github.com/SystemCrafters/crafted-emacs/issues/369
;; - https://github.com/joaotavora/eglot/discussions/1127
(use-package company :ensure t
  :custom
  (company-idle-delay (lambda () (if (company-in-string-or-comment) 0.2 0)))
  :init
  (global-company-mode))

;; ;; In-buffer completion-at-point (i.e. completion popup)
;; ;; By the same author as vertico
;; (use-package corfu :ensure t
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-delay 0.1)
;;   (corfu-max-width 240)
;;   :init
;;   (global-corfu-mode)
;;   :config
;;   ;; use shift-tab to insert a separator for orderless style completion
;;   (general-def 'corfu-mode-map "<backtab>" #'corfu-insert-separator))

;; More fully-featured help information when running help commands
(use-package helpful :ensure t
  :config
  (general-def
    [remap describe-function] #'helpful-callable
    [remap describe-variable] #'helpful-variable
    [remap describe-key] #'helpful-key
    [remap describe-command] #'helpful-command))

;; -------------------------------------------------------------------
;; Version Control (git)
;; -------------------------------------------------------------------

(use-package git-timemachine :ensure t
  :after magit)
(use-package git-link :ensure t)

;; magit requires newer versions of some internal packages
(use-package transient :ensure t)

;; Magit (VC commands) and forge (interaction with forges)
(use-package magit :ensure t
  :after transient
  :commands (magit magit-status magit-file-dispatch)
  :custom (magit-wip-mode t)
  :hook
  (magit-mode . (lambda () (line-number-mode -1)))
  (magit-status-mode
   . (lambda ()
       ;; remove hooks that contribute to laggy inputs in magit status buffer
       (remove-hook 'post-command-hook 'magit-section-post-command-hook t)
       (remove-hook 'pre-command-hook 'magit-section-pre-command-hook t)))
  :init
  (setq forge-add-default-bindings nil)
  :config
  (setq
   ;; don't save things for me
   magit-save-repository-buffers nil
   magit-list-refs-sortby "-committerdate"
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; get section info with magit-describe-section
  (add-to-list 'magit-section-initial-visibility-alist '(unstaged . show))
  (add-to-list 'magit-section-initial-visibility-alist '(untracked . show))
  (add-to-list 'magit-section-initial-visibility-alist '(reviewer-pullreqs . show))
  (add-to-list 'magit-section-initial-visibility-alist '(assigned-pullreqs . show))

  (general-define-key
   ;; prevent magit from overriding my leader key
   :keymaps 'magit-mode-map
   :states '(normal visual motion)
   :prefix-map 'my/leader-map
   :prefix my/leader-key)

  ;; magit's map uses numbers for showing different indent levels,
  ;; even with evil-collection active. Override the heck out of that.
  (general-evil-define-key '(normal visual motion) 'magit-mode-map
    "0" #'evil-beginning-of-line
    "1" #'digit-argument
    "2" #'digit-argument
    "3" #'digit-argument
    "4" #'digit-argument
    "5" #'digit-argument
    "6" #'digit-argument
    "7" #'digit-argument
    "8" #'digit-argument
    "9" #'digit-argument)

  ;; no I don't want to stash while viewing a diff, what is wrong with you
  (general-evil-define-key '(normal visual motion) 'magit-diff-mode-map
    "z b" #'evil-scroll-line-to-bottom
    "z t" #'evil-scroll-line-to-top
    "z z" #'evil-scroll-line-to-center))

;; TODO deal with removal of insert-assigned-pullreqs &c:
;; https://github.com/magit/forge/issues/676
(use-package forge :ensure t
  :after magit
  :config
  ;; re-add support for assigned & review requests at top level of status
  ;; buffer without additional filtering. See https://github.com/magit/forge/issues/676
  (defun my/forge-insert-assigned-pullreqs ()
    "Insert assigned pullreqs to the magit status buffer."
    (forge-insert-topics 'assigned-pullreqs "Assigned pull requests"
      (lambda (repo)
        (and-let* ((me (ghub--username repo)))
          (forge--topics-spec :type 'pullreq :active t :assignee me)))))
  (defun my/forge-insert-review-requests ()
    "Insert requested reviews to the magit status buffer."
    (forge-insert-topics 'reviewer-pullreqs "Review requests"
      (lambda (repo)
        (and-let* ((me (ghub--username repo)))
          (forge--topics-spec :type 'pullreq :active t :reviewer me)))))
  (magit-add-section-hook 'magit-status-sections-hook
                          #'my/forge-insert-assigned-pullreqs
                          #'forge-insert-pullreqs)
  (magit-add-section-hook 'magit-status-sections-hook
                          #'my/forge-insert-review-requests
                          #'my/forge-insert-assigned-pullreqs))

(use-package magit-todos :ensure t)

;; better diff highlighting
(use-package magit-delta :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; gutter highlights for changed regions, plus operations on those hunks
(use-package diff-hl :ensure t
  :commands (global-diff-hl-mode diff-hl-mode diff-hl-dired-mode)
  :hook
  (dired-mode . diff-hl-dired-mode)
  (prog-mode . global-diff-hl-mode)
  (emacs-startup . global-diff-hl-mode))

;; -------------------------------------------------------------------
;; Programming
;; -------------------------------------------------------------------
;; Note that there are several features in here that are reliant on
;; tree-sitter being installed. I am installing that, including support
;; in emacs, via Nix. If not using nix, consider using treessit-auto
;; or a similar package to ease installation of tree-sitter grammars

(use-package prettier-js :ensure t)

(use-package rainbow-delimiters :ensure t)

(use-package janet-ts-mode
  :ensure (:type git :host github :repo "sogaiu/janet-ts-mode"))

(use-package flycheck :ensure t)

(use-package flycheck-janet
  :ensure (:type git :host github :repo "sogaiu/flycheck-janet"))

(use-package sideline :ensure t
  :hook ((flymake-mode . sideline-mode)
         (flycheck-mode . sideline-mode))
  :config
  (setq sideline-backends-right '(sideline-flycheck sideline-flymake)
        sideline-truncate t))

(use-package sideline-flycheck :ensure t
  :hook (flycheck-mode . sideline-flycheck-setup))
(use-package sideline-flymake :ensure t)

(use-package ajrepl
  :after janet-ts-mode
  :ensure (:type git :host github :repo "sogaiu/ajrepl"))

(use-package rustic :ensure t
  :defer t
  :mode ("\\.rs\\'" . rustic-mode)

  :hook
  ;; fix jumping to test errors and go-to-error in test output.
  ;; reported as bug here: https://github.com/brotzeit/rustic/issues/573
  (rustic-compilation-mode . (lambda () (display-line-numbers-mode -1)))
  (rustic-cargo-test-mode
   . (lambda () (add-to-list 'compilation-error-regexp-alist
                             `(,(rx "thread '"
                                    (one-or-more (not "'"))
                                    "' panicked at "
                                    (group (one-or-more not-newline))
                                    ":"
                                    (group (one-or-more digit))
                                    ":"
                                    (group (one-or-more digit)))
                               1 2 3))))
  (rustic-mode . (lambda () (set-fill-column 80)))

  :custom
  (rustic-lsp-client 'eglot)
  (rustic-lsp-setup-p nil)
  (rustic-compile-directory-method #'rustic-buffer-workspace)
  (rustic-default-clippy-arguments "--workspace --benches --tests --all-features --all-targets")
  (rustic-format-trigger 'on-compile)
  ;; derive the underlying rust-mode that backs rustic-mode from rust-ts-mode
  ;; note: currently seems to break "current test" determination
  ;; (rust-mode-treesitter-derive t)

  :config
  ;; rustic adds a weird configuration parameter to eglot-server-programs
  ;; when it loads, but I want to use my specific config. Because of
  ;; load order (rustic mode is generally going to get loaded after
  ;; eglot, since eglot is built in and rustic is deferred), this can
  ;; wind up at the front of the eglot config list, ahead of custom
  ;; config. So, delete it.
  (setq eglot-server-programs
        (cl-remove '(rustic-mode :language-id "rust") eglot-server-programs
                   :test 'equal :key 'car))

  (defun my/rustic-call-in-crate-ctx (cmd)
    "Call CMD in the crate context rather than the workspace context."
    (interactive)
    (let ((rustic-compile-directory-method #'rustic-buffer-crate))
      ;; we aren't usign the lexical variable locally, so it warns
      ;; us about it, but we do want it set in the context of the
      ;; interactive call.
      (ignore rustic-compile-directory-method)
      (call-interactively cmd)))

  (setq rustic-format-on-save nil)

  ;; rust keybindings
  ;; main map
  (defvar my/rust-map (make-sparse-keymap))
  (general-create-definer my/rust-key-def :keymaps 'my/rust-map)
  ;; map for test prefix
  (defvar my/rust-test-map (make-sparse-keymap))
  (general-create-definer my/rust-test-key-def :keymaps 'my/rust-test-map)
  ;; map for package-local prefix
  (defvar my/rust-package-map (make-sparse-keymap))
  (general-create-definer my/rust-package-key-def :keymaps 'my/rust-package-map)
  ;; map for package-local test prefix
  (defvar my/rust-package-test-map (make-sparse-keymap))
  (general-create-definer my/rust-package-test-key-def :keymaps 'my/rust-package-test-map)

  ;; bindings that operate in the scope of the current crate
  (my/rust-package-test-key-def
   "a" (cons
        "run all tests"
        #'(lambda () (interactive) (my/rustic-call-in-crate-ctx #'rustic-cargo-test-run)))
   "t" (cons
        "run current test"
        #'(lambda () (interactive) (my/rustic-call-in-crate-ctx #'rustic-cargo-current-test)))
   "r" (cons
        "rerun last test"
        #'(lambda () (interactive) (my/rustic-call-in-crate-ctx #'rustic-cargo-test-rerun))))

  ;; bindings that operate in the scope of the workspace
  (my/rust-package-key-def
    "t" my/rust-package-test-map
    "C" (cons
         "run clippy"
         #'(lambda () (interactive) (my/rustic-call-in-crate-ctx #'rustic-cargo-clippy))))
  (my/rust-test-key-def
   "a" #'rustic-cargo-test-run
   "t" #'rustic-cargo-current-test
   "r" #'rustic-cargo-test-rerun)
  (my/rust-key-def
   "c" #'rustic-cargo-check
   "C" #'rustic-cargo-clippy
   "p" (cons "package" my/rust-package-map)
   "t" (cons "test" my/rust-test-map))

  ;; add rust map to the m prefix on the leader key
  (general-define-key
   :keymaps '(rust-mode-map rust-ts-mode-map)
   :states '(normal visual motion)
   :prefix my/leader-key
   "m" my/rust-map)

  (general-define-key
   :keymaps 'prog-mode-map
   :states '(insert)
   ;; When pressing return, continue comments if writing comments
   "<return>" #'indent-new-comment-line)

  (general-evil-define-key '(insert) 'sql-interactive-mode-map
    ;; shift-return to insert a newline instead of submitting
    "<S-return>" #'newline
    "C-k" #'comint-previous-input
    "C-j" #'comint-next-input)) ;; end rustic

(use-package nix-mode :ensure t
  :mode "\\.nix\\'")

(use-package svelte-mode :ensure t)

(use-package flymake-eslint :ensure t)

(use-package shfmt :ensure t)

(use-package terraform-mode :ensure t)

(use-package paredit :ensure t)

(use-package makefile-executor :ensure t)

(use-package hl-todo :ensure t
  :hook
  (prog-mode . hl-todo-mode))

(use-package citre :ensure t
  :custom
  (citre-peek-auto-restore-after-jump nil)
  (citre-peek-fill-fringe nil)
  (citre-peek-use-dashes-as-horizontal-border t)

  :config
  ;; Define evil-friendly keybindings for interacting with the peek
  (general-evil-define-key '(normal motion visual) 'citre-peek-keymap
    ;; jump to definition, ensuring we push to the evil stack before jumping
    "RET" #'(lambda () (interactive)
                   (evil--jumps-push)
                   (call-interactively #'citre-peek-jump))
    "<return>" #'(lambda () (interactive)
                   (evil--jumps-push)
                   (call-interactively #'citre-peek-jump))
    ;; jump in other window, closing the peek before jumping
    "M-RET" #'(lambda () (interactive)
                (call-interactively #'citre-peek-abort)
                (evil--jumps-push)
                (call-interactively #'xref-find-definitions-other-window))
    "<escape>" #'citre-peek-abort
    "g f" #'citre-peek-through
    "g r" #'citre-peek-through-reference
    "C-k" #'citre-peek-prev-line
    "C-j" #'citre-peek-next-line
    "C-h" #'citre-peek-chain-backward
    "C-l" #'citre-peek-chain-forawrd
    "C-S-k" #'citre-peek-prev-branch
    "C-S-j" #'citre-peek-next-branch
    "C-p" #'citre-peek-prev-tag
    "C-n" #'citre-peek-next-tag)

  ;; fix an issue with evil where going into peek mode for some reason
  ;; requires a subsequent ESC for the citre keymap to become active
  (advice-add #'citre-peek
              :after (lambda () (evil-force-normal-state))
              '((name . "citre-force-normal-mode"))))

(use-package consult-eglot :ensure t)

(use-package breadcrumb :ensure t
  :defer t
  :hook
  (emacs-startup . breadcrumb-mode))

(defvar-local my/eglot-format-p t
  "Whether to use eglot for automatic formatting.")

;; upgrade internal package as dep of dev eglot
(use-package eldoc :ensure t
  :custom
  (eldoc-echo-area-use-multiline-p 10))
(use-package jsonrpc :ensure t)
(use-package eglot :ensure t
  :after (general evil citre)
  :custom
  (eglot-report-progress t)
  :hook
  (eglot-managed-mode
   . (lambda ()
       ;; local hook to do format-on-save w/eglot via LS
       (add-hook
        'before-save-hook
        #'(lambda () (when my/eglot-format-p (eglot-format-buffer)))
        0
        t)))
  :config
  (my/eglot-set-rust-analyzer-config)

  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode . ,(eglot-alternatives
                                        '(("typescript-language-server" "--stdio")
                                          ("deno" "lsp")))))

  (add-to-list 'eglot-server-programs
               `(sql-mode . ("postgrestools" "lsp-proxy")))

  (defvar my/eglot-map (make-sparse-keymap))
  (general-create-definer my/eglot-key-def :keymaps 'my/eglot-map)
  (defvar my/eglot-find-map (make-sparse-keymap))
  (general-create-definer my/eglot-find-def :keymaps 'my/eglot-find-map)

  (my/eglot-key-def
    "a" #'eglot-code-actions
    "A" #'eglot-code-action-quickfix
    "j" (cons "jump to symbol" #'consult-eglot-symbols)
    "r" #'eglot-rename
    "x" (cons "diagnostics" #'consult-flymake)
    "X" (cons "project diagnostics" #'flymake-show-project-diagnostics))

  (general-define-key
   :keymaps 'eglot-mode-map
   :states '(normal visual motion)
   :prefix my/leader-key
   "c" (cons "code" my/eglot-map))

  (general-def eglot-mode-map
    ;; replace general-purpose find-def and find-ref commmands with
    ;; LSP versimns
    [remap xref-find-definitions] #'citre-peek
    [remap xref-find-references] #'citre-peek-reference))

;; adds support for expand macro and other stuff
(use-package eglot-x
  :ensure (:type git :host github :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup)
  (setq eglot-x-enable-snippet-text-edit nil))

;; requires `emacs-lsp-booster` to be installed
(use-package eglot-booster
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;; syntactic folding for treesitter-derived modes
(use-package treesit-fold
  :after evil
  :ensure (:type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook
  (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp)))
  ;; won't need once https://github.com/brotzeit/rustic/issues/571 is fixed
  (rustic-mode . (lambda () (treesit-parser-create 'rust)))
  (rust-mode . (lambda () (treesit-parser-create 'rust)))
  :config
  (global-treesit-fold-mode))

(use-package treesit :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (add-to-list 'treesit-load-name-override-list '(terraform "libtree-sitter-hcl" "tree_sitter_hcl"))
  ;; tree-sitter-enabled programming language modes
  ;; note: rust-mode support is configured via the rustic package options
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.[Dd]ockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode)))

;; used for certain eglot-x functionality
(use-package yasnippet :ensure t
  :config
  (yas-global-mode 1))

;; -------------------------------------------------------------------
;; LLMs
;; -------------------------------------------------------------------

(use-package gptel :ensure t
  :config
  (setq
   gptel-model 'mistral-nemo:latest
   gptel-backend (gptel-make-ollama
                  "Ollama"
                  :host "localhost:11434"
                  :stream t
                  :models '(mistral-nemo:latest))))

;; -------------------------------------------------------------------
;; Envrc
;; -------------------------------------------------------------------

;; Ensure this is after any other things, since hooks are prepended,
;; so that this hook will get run before any other hooks for various
;; modes
(use-package envrc :ensure t
  :demand t
  :hook
  (emacs-startup . envrc-global-mode)
  (rustic-mode . envrc-global-mode)
  (lsp-before-initialize . envrc-global-mode))

;; -------------------------------------------------------------------
;; Org, Docs, Etc.
;; -------------------------------------------------------------------

(use-package pdf-tools :ensure t)

;; -------------------------------------------------------------------
;; Email
;; -------------------------------------------------------------------

;; installed via nix
(use-package mu4e :ensure nil
  :config
  (setq
   ;; set emacs default mail variables
   mail-user-agent 'mu4e-user-agent
   read-mail-command 'mu4e
   ;; settings that apply to gnus mode and mu4e
   message-send-mail-function 'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-local-domain "gmail.com"
   smtpmail-smtp-service 587
   ;; mu4e settings
   mu4e-use-fancy-chars t
   mu4e-maildir-shortcuts '((:maildir "/gmail/Inbox" :key ?g)
                            (:maildir "/spectrust/Inbox" :key ?s))
   mu4e-contexts `(,(make-mu4e-context
                     :name "Gmail"
                     :enter-func (lambda () (mu4e-message "Entering Gmail Context"))
                     :match-func (lambda (msg)
                                   (when msg
                                     (string-match-p "/gmail/.*" (mu4e-message-field msg :maildir))))
                     :vars `((user-mail-address . "msplanchard@gmail.com")
                             (user-full-name . "Matthew Planchard")
                             (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")
                             (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                             (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                             (mu4e-trash-folder . "/gmail/[Gmail]/Trash")))
                   ,(make-mu4e-context
                     :name "Spec"
                     :enter-func (lambda () (mu4e-message "Entering Spec Context"))
                     :match-func (lambda (msg)
                                   (when msg
                                     (string-match-p "/spectrust/.*" (mu4e-message-field msg :maildir))))
                     :vars `((user-mail-address . "matthew@spec-trust.com")
                             (user-full-name . "Matthew Planchard")
                             (mu4e-refile-folder . "/spectrust/[Gmail]/All Mail")
                             (mu4e-sent-folder . "/spectrust/[Gmail]/Sent Mail")
                             (mu4e-drafts-folder . "/spectrust/[Gmail]/Drafts")
                             (mu4e-trash-folder . "/spectrust/[Gmail]/Trash"))))))

(use-package org-msg :ensure t
  :config
  (setq
   org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
   org-msg-startup "hidestars indent inlineimages"
   org-msg-default-alternatives '((new		. (text html))
				                  (reply-to-html	. (text html))
				                  (reply-to-text	. (text)))
   org-msg-convert-citation t))

;; -------------------------------------------------------------------
;; Appearance
;; -------------------------------------------------------------------

(use-package doom-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package modus-themes :ensure t)
(use-package zenburn-theme :ensure t)

;; -------------------------------------------------------------------
;; Emacs Config
;; -------------------------------------------------------------------

(use-package ispell :ensure nil
  :custom
  ;; set a dictionary to use for added words and such
  (ispell-alternate-dictionary (expand-file-name "~/.config/dict/mp.dict"))
  :config
  (when (not (file-exists-p ispell-alternate-dictionary))
    (make-directory (file-name-directory ispell-alternate-dictionary) t)
    (with-temp-buffer
      ;; see http://aspell.net/man-html/Format-of-the-Personal-and-Replacement-Dictionaries.html
      (insert "personal_ws-1.1 en 0\n")
      (write-file ispell-alternate-dictionary))))

;; Run the server so that we can just use `emacsclient` as EDITOR
(use-package server :ensure nil
  :config
  (unless (server-running-p) (server-start)))

(use-package org :ensure nil
  :custom
  (org-startup-indented t))

;; Emacs settings
(use-package emacs :ensure nil
  :after (gruvbox)
  :custom
  ;; relative line numbers
  (display-line-numbers-type 'relative)
  ;; smaller fringe on left, no fringe on right
  (fringe-mode '(3 . 0))
  (global-display-fill-column-indicator-mode t)
  ;; don't show the startup screen
  (inhibit-startup-screen t)
  ;; show line numbers in modeline
  (line-number-mode t)
  ;; don't display the menu bar
  (menu-bar-mode nil)
  ;; use spaces instead of tabs when pressing tab
  (indent-tabs-mode nil)
  ;; smaller tabs
  (tab-width 4)
  ;; show tooltips in the echo area rather than as separate frames
  (tooltip-mode nil)
  ;; don't word wrap by default
  (truncate-lines t)
  ;; type 'y' or 'n' instaed of 'yes' or 'no'
  (use-short-answers t)
  ;; when going "off" the screen, wrap around to the other side
  (windmove-wrap-around t)
  ;; no sense paying for the bindings since I'm going to rebind anyway
  (winner-dont-bind-my-keys t)
  ;; tracks window history for undo/redo
  (winner-mode t)
  :mode
  ("\\.env\\'" . bash-ts-mode)
  :hook
  (bash-ts-mode . flymake-mode)
  (emacs-lisp-mode . flymake-mode)
  (prog-mode . flymake-mode)
  (prog-mode . electric-pair-mode)
  (before-save . delete-trailing-whitespace)
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
   ;; don't prompt, just follow symbolic links
   vc-follow-symlinks t
   ;; backup all files to a common directory
   my/emacs-backup-directory (concat (or (getenv "XDG_RUNTIME_DIR") "~/.local") "/emacs-backups")
   backup-directory-alist `(("." . ,my/emacs-backup-directory))
   ;; include lockfiles
   lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat my/emacs-backup-directory "/\\1" ) t))

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

  (load-theme 'gruvbox-dark-hard t)
  ;; font settings
  ;; top fonts: codenewroman, hasklug, comicshans
  (set-frame-font "CodeNewRoman Nerd Font Mono" nil t)
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

;; Don't inhibit startup, but go ahead and start the server next time we're idle
(run-with-idle-timer 1 nil #'(lambda () (unless (server-running-p) (server-start))))

;; Prevent warnings from the byte-compiler about free variables and
;; unresolved functions, since those are a natural part of an init file.
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:


(provide 'init)
;;; init.el ends here
