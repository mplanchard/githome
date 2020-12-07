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
(setq doom-theme 'doom-dark+)  ;; note: overriden below by kaolin-themes pkg

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

(setq ispell-dictionary "en_US")

;; Search the GH directory for projects by default
(setq projectile-project-search-path '("~/github/"))

;; For LSP performance
(setq read-process-output-max (* 1024 1024)) ;; 1 mb
(after! lsp
  (setq lsp-ui-sideline-delay 0.75)
  (setq lsp-ui-doc-delay 0.75)
  (setq lsp-idle-delay 1)
  (setq lsp-ui-sideline-diagnostic-max-lines 20)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil))

;; Allow lots of flycheck errors
(after! flycheck
  (setq flycheck-checker-error-threshold 1000))

(after! neotree
  (setq neo-theme 'ascii))

(after! ispell
  (setq ispell-extra-args (append '("--camel-case") ispell-extra-args)))


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

;; deft notes
(setq
 deft-directory org-directory
 deft-extensions '("org" "md")
 deft-recursive t)

(setq deft-directory org-directory
      deft-extensions '("org" "md" "txt")
      deft-recursive t)

;; Rust-related LSP settings
(setq rustic-format-on-save t)
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
  (load-theme 'kaolin-temple t)
  (kaolin-treemacs-theme))


;; Use bar and block cursor in terminal emacs rather than just block
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

;; Use mermaid-mode for mermaid files
(use-package! mermaid-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode)))

;; **********************************************************************
;; Keybindings
;; **********************************************************************

(map! :map org-mode-map
      :localleader
      :desc "org-insert-structure-template" "t" #'org-insert-structure-template)

(map! :leader
      :prefix "w"
      :desc "ace-window" :nv "/" #'ace-window)

(map! :leader
      :desc "paste from kill ring" :nv "P" #'counsel-yank-pop)

(map! :prefix "g"
      :desc "show-hover-doc" :nv "h" #'lsp-ui-doc-glance)

(map! :map mu4e-headers-mode-map
      :desc "mark thread"
      :nv "T"
      #'mu4e-headers-mark-thread)

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


;; **********************************************************************
;; Email
;; **********************************************************************

(if (string-equal system-type "darwin")
    ;; Add homebrew-installed mu's mu4e path to the load path
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
  ;; Add snap-installed mu4e path to load path (TODO: make ubuntu specific)
  (add-to-list 'load-path "/snap/maildir-utils/current/share/emacs/site-lisp/mu4e"))

(setq
 ;; Don't pull in the entire thread from the archive when it gets a new message
 mu4e-headers-include-related nil
 ;; Set the maildir to ~/mail -- the default is /mail, which was fine, except that
 ;; the ubuntu snap for mu doesn't have access to private directories
 mu4e-maildir "~/mail"  ;; deprecated, but keeping around for now
 mu4e-root-maildir "~/mail")

;; Ensure we can load it
(require 'mu4e)

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
(set-email-account! "work"
                    '((user-email-address . "matthew@bestow.com")
                      (smtpmail-smtp-user . "matthew@bestow.com")
                      (smtpmail-local-domain . "gmail.com")
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (smtpmail-default-smtp-server . "smtp.gmail.com")
                      (smtpmail-smtp-service . 587)
                      (mu4e-drafts-folder . "/work/[Gmail]/Drafts")
                      (mu4e-refile-folder . "/work/[Gmail]/All Mail")
                      (mu4e-sent-folder . "/work/[Gmail]/Sent Mail")))

;; Send HTML messages by default.
(after! org-msg
  (setq org-msg-default-alternatives '(html)))

(defun mp-email-empty-trash ()
  "Empty the mu4e trash directory of anything older than 10 days old"
  (interactive)
  (async-shell-command
   (format
    "%s find maildir:/trash AND date:10d..1000d --fields=l | xargs rm -f"
    mu4e-mu-binary)))

;; Check mail every ten minutes:
;; first, cancel any running timers to avoid creating a multitude due to e.g.
;; refreshing doom emacs
(cancel-function-timers #'mu4e-update-mail-and-index)
;; then set up the email checker to run every 10 minutes
(run-with-timer 0 (* 60 10) #'mu4e-update-mail-and-index t)

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

;; Have pylint and flake8 run after any LSP checks
(add-hook
 'python-mode-hook
 (lambda ()
   (flycheck-add-next-checker 'lsp 'python-flake8)
   (flycheck-add-next-checker 'python-flake8 'python-pylint)))

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
  (shell-command (format "source ~/.pyenv/take-two/bin/activate && ~/github/bestowinc/take-two/go db tunnel %s &" dbenv))
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
  (interactive "sTarget [default origin/master]: ")
  (let ((current
         ;; grab the current deployed
         (cdr (assoc 'commit_sha
                     (with-current-buffer (url-retrieve-synchronously "https://api.hellobestow.com/version")
                       (goto-char url-http-end-of-headers)
                       (json-read)))))
        (target
         (if (string-empty-p target) "origin/master" target)))
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


;; mu thread folding from https://gist.github.com/felipeochoa/614308ac9d2c671a5830eb7847985202

(defun mu4e~headers-msg-unread-p (msg)
  "Check if MSG is unread."
  (let ((flags (mu4e-message-field msg :flags)))
    (and (member 'unread flags) (not (member 'trashed flags)))))

(defvar mu4e-headers-folding-slug-function
  (lambda (headers) (format " (%d)" (length headers)))
  "Function to call to generate the slug that will be appended to folded threads.
This function receives a single argument HEADERS, which is a list
of headers about to be folded.")

(defun mu4e~headers-folded-slug (headers)
  "Generate a string to append to the message line indicating the fold status.
HEADERS is a list with the messages being folded (including the root header)."
  (funcall mu4e-headers-folding-slug-function headers))

(defun mu4e~headers-fold-make-overlay (beg end headers)
  "Hides text between BEG and END using an overlay.
HEADERS is a list with the messages being folded (including the root header)."
  (let ((o (make-overlay beg end)))
    (overlay-put o 'mu4e-folded-thread t)
    (overlay-put o 'display (mu4e~headers-folded-slug headers))
    (overlay-put o 'evaporate t)
    (overlay-put o 'invisible t)))

(defun mu4e~headers-fold-find-overlay (loc)
  "Find and return the 'mu4e-folded-thread overlay at LOC, or return nil."
  (cl-dolist (o (overlays-in (1- loc) (1+ loc)))
    (when (overlay-get o 'mu4e-folded-thread)
      (cl-return o))))

(defun mu4e-headers-fold-all ()
  "Fold all the threads in the current view."
  (interactive)
  (let ((thread-id "") msgs fold-start fold-end)
    (mu4e-headers-for-each
     (lambda (msg)
       (end-of-line)
       (push msg msgs)
       (let ((this-thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
         (if (string= thread-id this-thread-id)
             (setq fold-end (point))
           (when (< 1 (length msgs))
             (mu4e~headers-fold-make-overlay fold-start fold-end (nreverse msgs)))
           (setq fold-start (point)
                 fold-end (point)
                 msgs nil
                 thread-id this-thread-id)))))
    (when (< 1 (length msgs))
      (mu4e~headers-fold-make-overlay fold-start fold-end (nreverse msgs)))))

(defun mu4e-headers-toggle-thread-folding (&optional subthread)
  "Toggle the folding state for the thread at point.
If SUBTHREAD is non-nil, only fold the current subthread."
  ;; Folding is accomplished using an overlay that starts at the end
  ;; of the parent line and ends at the end of the last descendant
  ;; line. If there's no overlay, it means it isn't folded
  (interactive "P")
  (if-let ((o (mu4e~headers-fold-find-overlay (point-at-eol))))
      (delete-overlay o)
    (let* ((msg (mu4e-message-at-point))
           (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (path-re (concat "^" (mu4e~headers-get-thread-info msg 'path)))
           msgs first-marked-point last-marked-point)
      (mu4e-headers-for-each
       (lambda (submsg)
         (when (and (string= thread-id (mu4e~headers-get-thread-info submsg 'thread-id))
                    (or (not subthread)
                        (string-match-p path-re (mu4e~headers-get-thread-info submsg 'path))))
           (push msg msgs)
           (setq last-marked-point (point-at-eol))
           (unless first-marked-point
             (setq first-marked-point last-marked-point)))))
      (when (< 1 (length msgs))
        (mu4e~headers-fold-make-overlay first-marked-point last-marked-point (nreverse msgs))))))


;; **********************************************************************
;; Auto-added custom stuff
;; **********************************************************************

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
