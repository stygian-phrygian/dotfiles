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


; vim keybindings for emacs
(use-package evil
  :ensure t
  :config
  (progn
    ; turn on evil-mode everywhere
    (evil-mode 1)
    ; bind <esc> to sensible quit functionality
    ; found here:
    ; https://stackoverflow.com/a/10166400/8475035
    (defun minibuffer-keyboard-quit ()
        "Abort recursive edit. In Delete Selection mode,
         if the mark is active, just deactivate it;
         then it takes a second \\[keyboard-quit] to abort the minibuffer."
        (interactive)
        (if (and delete-selection-mode transient-mark-mode mark-active)
            (setq deactivate-mark  t)
          (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
          (abort-recursive-edit)))
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    ; set up comment toggling
    (use-package evil-commentary
      :ensure t
      :config (evil-commentary-mode))

    ; set-up evil-leader
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      :config
      (progn
        ; leader on always
        (setq evil-leader/in-all-states t)
        (setq evil-leader/leader "<SPC>")
        ; global leader bindings
        (evil-leader/set-key
          "l" 'avy-goto-line
          "w" 'avy-goto-word-0
          "x" 'smex)
        ;; mode specific leader bindings
        ;(evil-leader/set-key-for-mode 'go-mode
        ;  "b" 'TODO-go-compile-file)
        
        ))))

; better M-x completion
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

; jump anywhere with only a few characters
; vim-easymotion (ish) clone for emacs
; key bindings are set in the evil-leader config 
(use-package avy
  :ensure t
  :config
  ; grey out background when displaying jump characters
  (setq avy-background t))

; ; syntax checker for a variety of languages
; (use-package flycheck
;   :ensure t
;   :config
;   (progn
;     (add-hook 'after-init-hook 'global-flycheck-mode)
;     ; turn off flycheck for emacs lisp
;     (with-eval-after-load 'flycheck
;       (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

(use-package clojure-mode
  :ensure t)

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package cider
  :ensure t)
  ;:config (setq cider-auto-select-error-buffer nil))

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

; ; git integration
; (use-package magit
;   :ensure t)

; themes------
; (use-package dracula-theme
;   :ensure t
;   :load-path "themes"
;   :config
;   (load-theme 'dracula t))

; (use-package darkokai-theme
;   :ensure t
;   :config (load-theme 'darkokai t))

; (use-package gruvbox-theme
;   :ensure t
;   :config (load-theme 'gruvbox-dark-hard t))

(use-package cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk t))

; (use-package hc-zenburn-theme
;   :ensure t
;   :config
;   (load-theme 'hc-zenburn t))

; ;; whitespace
; (use-package whitespace
;   :ensure t
;   :commands (whitespace-mode)
;   :config
;   (setq whitespace-style '(face tabs spaces newline empty
;                            trailing tab-mark newline-mark)))

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

    ; change company completion colors
    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
            `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
    
    ; install go completion
    (use-package company-go
      :ensure t
      :config
      (progn
        (require 'company-go)
        ; add gocode hook
        ; go get -u github.com/nsf/gocode # must install that first
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
; display column number
(setq column-number-mode t)
; inhibit-startup-screen
(setq inhibit-startup-screen t)
; better word wrapping
(setq visual-line-mode t)
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
; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
; bind C-PageUp and C-PageDown to cycle through
; buffers previous and next respectively
; (without switching to a separate window)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)
; turn on default window movement bindings
; which are: Shift-<Arrow-Up|Down|Left|Right>
; NB. this might interfere with org-mode
; if so run this: 
; (setq org-replace-disputed-keys t)
(windmove-default-keybindings)
; inhibit indention of *prior* line when RET is pressed
; this fixes the semicolon indention bug
; hack found here:
; https://emacs.stackexchange.com/questions/20896/change-the-behaviour-of-ret-with-electric-indent-to-only-indent-the-new-line
; alternative solution:
; https://emacs.stackexchange.com/questions/9563/return-heavily-indents-previous-line
(setq-default electric-indent-inhibit t)

;;; init ends here
