; Emac's "customize" system is a way of declaring settings which can be edited via a UI. Some plugins use
; this, like elscreen. See here for more:
; http://ergoemacs.org/emacs/emacs_custom_system.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(org-agenda-files '("~/test.org"))
 '(package-selected-packages
   '(ace-jump-mode ag align-cljlet auto-complete autopair better-jumper browse-at-remote coffee-mode
                   color-theme-sanityinc-tomorrow color-theme-solarized column-marker dash-at-point
                   dash-functional diminish dired+ dired-details+ dogears edit-server elisp-autofmt
                   elisp-benchmarks evil evil-anzu evil-mc evil-multiedit evil-nerd-commenter
                   evil-surround eyebrowse f fill-column-indicator fiplr flx-ido framemove general
                   go-mode gradle-mode gumshoe hiwin ht ido-ubiquitous ido-vertical-mode indium
                   inf-clojure js-comint less-css-mode lua-mode magit markdown-mode maxframe
                   mu4e-maildirs-extension multi-term mustache-mode notmuch outline-magic parinfer
                   powerline projectile queue rainbow-delimiters ruby-electric rust-mode scss-mode
                   seq slamhound smartparens smex spell-fu spinner swift-mode tempel tree-mode
                   typescript-mode undo-fu wcheck-mode web-mode yaml-mode yascroll yasnippet
                   zoom-frm))
 '(spell-fu-idle-delay 1.5))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-error-highlight-face ((t (:inherit nil :underline "brown4"))))
 '(markdown-list-item1-face ((t (:foreground "#ECE27C"))))
 '(markdown-list-item2-face ((t (:foreground "#85C7FF"))))
 '(markdown-list-item3-face ((t (:foreground "#E4BDA7"))))
 '(markdown-list-item4-face ((t (:foreground "#87D952"))))
 '(markdown-list-item5-face ((t (:foreground "#C3D9C9"))))
 '(mu4e-cited-1-face ((t (:inherit font-lock-builtin-face :weight normal))))
 '(mu4e-cited-2-face ((t (:inherit font-lock-type-face :weight normal))))
 '(mu4e-cited-3-face ((t (:inherit font-lock-variable-name-face :weight normal))))
 '(mu4e-cited-4-face ((t (:inherit font-lock-keyword-face :weight normal))))
 '(mu4e-cited-5-face ((t (:inherit font-lock-comment-face :weight normal))))
 '(mu4e-cited-6-face ((t (:inherit font-lock-comment-delimiter-face :weight normal))))
 '(mu4e-cited-7-face ((t (:inherit font-lock-preprocessor-face :weight normal))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#3A8CD7"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#44D7BC"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#C9D736"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#D79841"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#D7604A"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#D777A9"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#5D7CD7")))))
