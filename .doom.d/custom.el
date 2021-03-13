(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1e1e1e" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   '("76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "7236acec527d58086ad2f1be6a904facc9ca8bf81ed1c19098012d596201b3f1" default))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(pos-tip-background-color "#303035")
 '(pos-tip-foreground-color "#d4d4d6")
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(safe-local-variable-values
   '((rustic-test-arguments . "--all")
     (lsp-enable-on-type-formatting . t)
     (python-pytest-executable . "docker-compose run --rm backend pytest")
     (lsp-enable-on-type-formatting . f)
     (lsp-python-ms-python-executable . "~/.pyenv/take-two/bin/python")
     (eval if
           (fboundp 'editorconfig-mode)
           (editorconfig-mode 1))
     (js-indent-level . 2)))
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
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 158 :width normal))))
 '(mu4e-thread-folding-child-face ((t (:extend t :background "gray18" :underline nil))) t)
 '(mu4e-thread-folding-root-unfolded-face ((t (:extend t :background "gray12" :overline nil :underline nil))) t)
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :weight normal)))))
(put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)
