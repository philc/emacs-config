;;
;; Terminal (multi-term mode)
;; This setup is a work-in-progress. I'm still exploring whether this is a viable terminal replacement. It's
;; very klunky out of the box.
;;
(require 'multi-term)
(setq multi-term-buffer-name "term"
      multi-term-program "/bin/zsh"
      multi-term-program-switches "--login")
(defun init-term-settings ()
  ;; Normally we maintain a margin of N lines between the cursor and the edge of the window, but in term mode,
  ;; the cursor should always be at the bottom of the window.
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 0)
  (setq-local scroll-step 1))

;; The default Evil state for terminals should be ready to accept commands.
(evil-set-initial-state 'term-mode 'emacs)

(add-hook 'term-mode-hook 'init-term-settings)
(add-hook 'term-mode-hook (lambda() (setq-local yas-dont-activate t)))

(evil-define-key 'normal term-raw-map "i" 'evil-emacs-state)
(evil-define-key 'normal term-raw-map (kbd "RET")
  (lambda ()
    (interactive)
    (term-send-input)))

;; What you want to do here is add mode-specific advice. Override evil-insert-state and in it,
;; call evil-eamcs-state if not evil-emacs-state-p
(evil-define-key 'normal term-raw-map "i"
  (lambda ()
    (interactive)
    (evil-emacs-state)))
(evil-define-key 'normal term-raw-map "a"
  (lambda ()
    (interactive)
    (evil-emacs-state)))
(evil-define-key 'emacs term-raw-map (kbd "C-y")
  (lambda ()
    (interactive)
    (term-send-raw-string "\e")))

(evil-define-key 'emacs term-raw-map (kbd "C-p") '(lambda () (interactive) (term-send-raw-string "\C-p")))
(evil-define-key 'emacs term-raw-map (kbd "C-n") '(lambda () (interactive) (term-send-raw-string "\C-n")))

;; Define the blue used by the emacs term to be readable for dark backgrounds.
(defface term-color-blue
  '((t :foreground "#1F93F3" :background "#1F93F3"))
  "Face used to render blue color code."
  :group 'term)
