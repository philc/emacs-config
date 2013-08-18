; Emac's "customize" system is a way of declaring settings which can be edited via a UI. Some plugins use
; this, like elscreen. See here for more:
; http://ergoemacs.org/emacs/emacs_custom_system.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-display-screen-number nil)
 '(elscreen-display-tab 20)
 '(elscreen-tab-display-kill-screen nil)
 '(org-agenda-files (quote ("~/test.org"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-control-face ((t (:background "#202424" :foreground "white" :underline t))))
 '(elscreen-tab-current-screen-face ((t (:background "#505454" :foreground "white"))))
 '(elscreen-tab-other-screen-face ((t (:background "black" :foreground "#bbbbbb" :underline t)))))
