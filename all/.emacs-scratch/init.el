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

;; Modal editing
(use-package evil :ensure t :demand t
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
  (add-hook 'help-mode-hook #'evil-normalize-keymaps)
  (evil-mode 1)
  :config
  ;; Flash yanked text when yanking
  (advice-add 'evil-yank :around 'my/evil-yank-advice))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

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
  
  (general-create-definer my/leader-key-def
    :keymaps 'my/leader-map)
  
  (defvar my/file-map (make-sparse-keymap))
  (defvar my/buffer-map (make-sparse-keymap))
  (defvar my/window-map (make-sparse-keymap))
  (defvar my/help-map (make-sparse-keymap))
  
  (general-create-definer my/buffer-key-def
    :keymaps 'my/buffer-map)
  (general-create-definer my/file-key-def
    :keymaps 'my/file-map)
  (general-create-definer my/help-key-def
    :keymaps 'my/help-map)
  (general-create-definer my/window-key-def
    :keymaps 'my/window-map)
  
  (my/buffer-key-def
   "s" #'save-buffer
   "b" #'switch-to-buffer)
  (my/help-key-def
   "m" #'describe-mode
   "f" #'describe-function
   "v" #'describe-variable)
  (my/file-key-def
   "f" #'find-file
   "r" #'recentf)
  (my/window-key-def
   "l" #'evil-window-right
   "h" #'evil-window-left
   "j" #'evil-window-down
   "k" #'evil-window-up
   "p" #'evil-window-prev
   "d" #'evil-window-delete
   "v" #'evil-window-vsplit)
  
  (my/leader-key-def
   "b" (cons "buffer" my/buffer-map)
   "f" (cons "file" my/file-map)
   "h" (cons "help" my/help-map)
   "w" (cons "window" my/window-map)
   "x" #'execute-extended-command))

;; Pull PATH from default shell into emacs. Very useful in nix environments.
(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :init
  (when (daemonp) (exec-path-from-shell-initialize)))

;; Completion framework
(use-package vertico :ensure t
  :init
  (vertico-mode))

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
  (global-corfu-mode))

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
   vc-follow-symlinks t)
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
  (scroll-bar-mode -1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval progn
	   (define-derived-mode deno-mode typescript-mode
	     "Deno" "A major mode for Deno files"
	     (set (make-local-variable 'lsp-enabled-clients)
		  '(deno-ls))
	     (setq +format-with-lsp t)
	     (when (boundp 'prettier-js-mode) (prettier-js-mode -1)))
	   (add-to-list 'auto-mode-alist
			'("\\.deno\\.ts\\'" . deno-mode))
	   (when (fboundp 'lsp) (add-hook 'deno-mode-hook #'lsp)))
     (eval progn (setq sql-postgres-login-params nil)
	   (when (not (boundp 'sql-connection-alist))
	     (setq sql-connection-alist 'nil))
	   (dolist (db '("proxy-dev" "proxy-dev-test"))
	     (setf sql-connection-alist
		   (assoc-delete-all db sql-connection-alist)))
	   (with-temp-buffer
	     (insert-file-contents-literally
	      (concat
	       (let ((d (dir-locals-find-file ".")))
		 (if (stringp d) d (car d)))
	       "./env/local.env"))
	     (let
		 ((proxy-db-url
		   (progn
		     (search-forward "PROXY_DATABASE_URL=")
		     (buffer-substring-no-properties (point)
						     (line-end-position))))
		  (proxy-test-db-url
		   (progn
		     (goto-char (point-min))
		     (search-forward "PROXY_TEST_DATABASE_URL=")
		     (buffer-substring-no-properties (point)
						     (line-end-position)))))
	       (add-to-list 'sql-connection-alist
			    `("proxy-dev" (sql-product 'postgres)
			      (sql-database ,proxy-db-url)))
	       (add-to-list 'sql-connection-alist
			    `("proxy-dev-test" (sql-product 'postgres)
			      (sql-database ,proxy-test-db-url)))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
