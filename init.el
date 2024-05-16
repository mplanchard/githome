(require 'cl-lib)

;; --------------------------------------------------------------------------------
;; MY FUNCTIONS
;; --------------------------------------------------------------------------------
;; My own, my precious
;; --------------------------------------------------------------------------------

;; A function to retrieve the build date of emacs, needed for elpaca on NixOS
(defun my/nixos/get-emacs-build-date ()
  (string-match "--prefix.*emacs.*\\([[:digit:]]\\{8\\}\\)" system-configuration-options)
  (let ((config-date (match-string 1 system-configuration-options)))
    (car (read-from-string config-date))))

;; Return whether we're running on NixOS
(defun my/nixos-p ()
  (let ((sysinfo (shell-command-to-string "uname -v")))
    (not (eq (cl-search "NixOS" sysinfo) nil))))

;; --------------------------------------------------------------------------------
;; ELPACA INSTALL
;; --------------------------------------------------------------------------------

;; If we're running on NixOS, set elpaca-core-date
(if (my/nixos-p)
    (setq elpaca-core-date (list (my/nixos/get-emacs-build-date))))

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
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

;; Pull PATH from default shell into emacs. Very useful in nix environments.
(use-package exec-path-from-shell
  :ensure t
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
   ;; secrets!
   auth-sources '("~/.authinfo.gpg")
   ;; trigger completion-at-point with TAB
   tab-always-indent 'complete
   ;; hide commands from M-x that don't apply to the current mode
   read-extended-command-predicate #'command-completion-default-include-p)
  ;; theme selection
  (load-theme 'modus-vivendi t)
  ;; font settings
  (set-frame-font "ComicShannsMono Nerd Font Mono" nil t)
  ;; height is x10 of usual fon size
  (set-face-attribute 'default nil :height 120)
  ;; turn off the toolbar
  (tool-bar-mode -1)
  ;; save minibuffer history
  (savehist-mode))
