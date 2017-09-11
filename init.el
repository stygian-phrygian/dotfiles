; set up package repositories
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
; enable packages
(setq package-enable-at-startup nil)
(package-initialize)

; install "use-package" package
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
      (package-install 'use-package))
(eval-when-compile
    (require 'use-package))


;; vim keybindings for emacs
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

; better M-x completion
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

; ; syntax checker for a variety of languages
; (use-package flycheck
;   :ensure t
;   :config
;   (progn
;     (add-hook 'after-init-hook 'global-flycheck-mode)
;     ;; turn off flycheck for emacs lisp
;     (with-eval-after-load 'flycheck
;       (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

(use-package clojure-mode
  :ensure t)

(use-package clojure-mode-extra-font-locking
  :ensure t)

; rainbow colored parentheses to ease viewing
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'slime-mode-hook #'rainbow-delimiters-mode)))

; awesome common lisp environment
(use-package slime
  :ensure t
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))

; golang
(use-package go-mode
  :ensure t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)))

; git integration
(use-package magit
  :ensure t)

; theme
(use-package dracula-theme
  :ensure t
  :load-path "themes"
  :config
  (load-theme 'dracula t))

;(use-package darkokai-theme
;  :ensure t
;  :config (load-theme 'darkokai t))

;(use-package solarized-theme
;  :ensure t
;  :config
;  (load-theme 'solarized-dark t))


; relative line numbers
(use-package nlinum-relative
  :ensure t
  :config
  (progn
    (require 'nlinum-relative)
    ; set-up evil
    (nlinum-relative-setup-evil)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)
    ; change update delay
    (setq nlinum-relative-redisplay-delay 0)
    ; change current line symbol
    (setq nlinum-relative-current-symbol "")
    ; set line number format
    (setq nlinum-format "%4d  \u2502 ")
    ; change line number background and foreground colors
    (set-face-attribute 'linum nil
      :foreground (face-foreground 'default)
      :background (face-background 'default))
    ))

(use-package company
  :diminish company-mode
  :ensure t
  :init (global-company-mode t)
  :config
  (progn
    ; bigger popup window
    (setq company-tooltip-limit 20)
    ; decrease delay before autocompletion popup shows
    (setq company-idle-delay .3)
    ; remove annoying blinking
    (setq company-echo-delay 0)
    ; minimum prefix
    (setq company-minimum-prefix-length 2)
    ; start autocompletion only after typing
    (setq company-begin-commands '(self-insert-command))

    ;; change company completion colors
    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
            `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
    
    ;; install go completion
    (use-package company-go
      :ensure t
      :config
      (progn
        (require 'company-go)
        ;; add gocode hook
        ;; go get -u github.com/nsf/gocode # must install that first
        (add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))))))

; turn off tool bar
(tool-bar-mode -1)
; turn off menu bar
(menu-bar-mode -1)
; turn off scroll bar
(scroll-bar-mode -1)
; inhibit-startup-screen
(setq inhibit-startup-screen t)
; empty scratch buffer
(setq initial-scratch-message nil)
; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
; 4 spaces is a tab
(setq tab-width 4)
; disable visual bell graphic
(setq visible-bell nil)
; disable audio bell
(setq ring-bell-function 'ignore)


;;; init ends here
