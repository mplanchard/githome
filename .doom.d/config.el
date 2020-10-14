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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

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

;; Search the GH directory for projects by default
(setq projectile-project-search-path '("~/github/"))

;; For LSP performance
(setq read-process-output-max (* 1024 1024)) ;; 1 mb
(after! lsp
        (setq lsp-ui-sideline-delay 0.75)
        (setq lsp-ui-doc-delay 0.75)
        (setq lsp-idle-delay 1)
        (setq lsp-ui-sideline-diagnostic-max-lines 5)
        (setq lsp-signature-auto-activate nil))


(after! neotree
        (setq neo-theme 'ascii)
  )

;; Fill the 80th column to let me know I've gone too far
(setq global-hl-fill-column-mode t)

;; org-mode settings
(after! org
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-smart-quotes nil)
  (setq org-export-with-sub-superscripts '{})
  (setq org-export-with-toc nil)
  )

;; Rust-related LSP settings
(setq rustic-format-on-save t)
(setq lsp-rust-all-features t)
(setq lsp-rust-cfg-test t)

;; **********************************************************************
;; Packages
;; **********************************************************************

(use-package! lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))
(use-package! direnv :config (direnv-mode))
(use-package! ace-window)
(use-package! exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
                (exec-path-from-shell-initialize)))

;; **********************************************************************
;; Keybindings
;; **********************************************************************

(map! :map org-mode-map
      :localleader
      :desc "org-insert-structure-template" "t" #'org-insert-structure-template)

(map! :leader
      :prefix "w"
      :desc "ace-window" :nv "/" #'ace-window)

(map! :prefix "g"
      :desc "show-hover-doc" :nv "h" #'lsp-ui-doc-glance)

(map! (:after org
       :map org-mode-map
       :localleader
       :prefix "l"
       :desc "github-link" :nv "g" #'mp-insert-github-link))

;; **********************************************************************
;; Python (why is it such a pain)
;; **********************************************************************

(setq flycheck-python-mypy-executable "mypy")

;; The mspyls server doesn't do type checking, so we add in mypy explicitly
;; to the checker chain so we don't have to run it manually.
;; see doom-emacs/issues#1530 for explanation
(defun mp-py-enable-linters
    ()
  (interactive nil)
  (flycheck-add-next-checker 'lsp  'python-flake8 'python-mypy))

(use-package! python-black
  :demand t
  :after python)
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

(defun mp-parse-github-target (text)
  (let ((split-url (split-string text "#" t "[[:space:]]+")))
    (list (concat
           "https://github.com/bestowinc/"
           (car split-url)
           "/pull/"
           (car (last split-url)))
          text)))

(defun mp-make-github-link (text)
  (interactive "sGithub Target: ")
  (apply 'org-insert-link nil (mp-parse-github-target text)))

(defun mp-insert-github-link (start end)
  (interactive "r")
  ;; check whether the region is actively selected
  (if (use-region-p)
      ;; If so, use the start and end to make the link
      (mp-make-github-link (buffer-substring start end))
      ;; Otherwise, call the function interactively
      (call-interactively 'mp-make-github-link)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21242b" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   (quote
    ("76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(safe-local-variable-values
   (quote
    ((lsp-python-ms-python-executable-cmd . "venv/bin/python"))))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 158 :width normal)))))
