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
    ; want vim-esque undo
    (setq evil-want-fine-undo t)
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
          "i" 'avy-goto-subword-1 ;"i" stands for "in between"
                                  ; works on camelCase & etc
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

; make windows resize elegantly
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

; rainbow colored parentheses to ease viewing
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'slime-mode-hook #'rainbow-delimiters-mode)))

; minibuffer documentation
(use-package eldoc
  :ensure t
  :config
  (progn
    ; go eldoc
    (use-package go-eldoc
        :ensure t
        :defer t
        :init (add-hook 'go-mode-hook #'go-eldoc-setup))))

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
  :defer t
  :ensure t)

(use-package clojure-mode-extra-font-locking
  :defer t
  :ensure t)

(use-package cider
  :defer t
  :ensure t)
  ;:config (setq cider-auto-select-error-buffer nil))

;; (use-package parinfer
;;   :ensure t
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults
;;             pretty-parens
;;             evil
;;             smart-tab
;;             smart-yank))
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

; awesome common lisp environment
(use-package slime
  :ensure t
  :defer t
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))

; golang
; install gocode (for completion),
; godef (for jumping into methods), and
; goimports (for automatically importing packages)
;
; go get golang.org/x/tools/cmd/goimports
; go get -u github.com/nsf/gocode
; go get -u github.com/rogpeppe/godef
; completion (with gocode) is configured in the "company" package below
(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'go-mode-hook
              (lambda ()
                ; create compile command for go files
                (setq compile-command "go build -v && go test -v && go vet")
                (define-key (current-local-map) "\C-c\C-c" 'compile)
                ; use goimports instead of gofmt
                (setq gofmt-command "goimports")
                (add-hook 'before-save-hook 'gofmt-before-save)))))



; rust
(use-package rust-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq rust-format-on-save t)))

; python
(use-package elpy
  :ensure t
  :defer t
  :init (elpy-enable)
  :config
  (progn
    ; use ipython for the repl (make sure ipython is installed)
    ; pip install ipython[all]
    (elpy-use-ipython)
    ; add elpy hook for python-mode
    (add-hook 'python-mode-hook 'elpy-mode)
    ; open the Python shell in a buffer after sending code to it
    (add-hook 'inferior-python-mode-hook 'python-shell-switch-to-shell)
    ; Use IPython as the default shell, with a workaround to accommodate IPython 5
    ; https://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5  (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "--simple-prompt -i")
    ; turn on elpy's completion (uses jedi, run: pip install jedi)
    ; tell elpy to use company autocomplete backend
    (setq elpy-rpc-backend "jedi")
    (use-package company-jedi
      :ensure t
      :config
      (add-hook 'python-mode-hook
                (lambda ()
                  (add-to-list 'company-backends 'company-jedi))))
    ; Enable pyvenv, which manages Python virtual environments
    (pyvenv-mode 1)
    ; Tell Python debugger (pdb) to use the current virtual environment
    ; https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
    (setq gud-pdb-command-name "python -m pdb ")))

; python auto-formatter
; enable autopep8 formatting on save, make sure autopep8 is installed
; pip install --upgrade autopep8
(use-package py-autopep8
  :ensure t
  :commands (py-autopep8-enable-on-save py-autopep8-buffer)
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

; ; git integration
; (use-package magit
;   :ensure t)

; themes------
(use-package solarized-theme :ensure t :defer t)
(use-package abyss-theme :ensure t :defer t)
(use-package dracula-theme :ensure t :defer t)
(use-package darkokai-theme :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package avk-emacs-themes :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package ample-zen-theme :ensure t :defer t)
(use-package hc-zenburn-theme :ensure t :defer t)
(use-package anti-zenburn-theme :ensure t :defer t)
(use-package darkburn-theme :ensure t :defer t)
(use-package cyberpunk-theme :ensure t :defer t)
(use-package birds-of-paradise-plus-theme :ensure t :defer t)
(use-package tao-theme :ensure t :defer t)
(use-package warm-night-theme :ensure t :defer t)
(use-package quasi-monochrome-theme :ensure t :defer t)
(use-package colonoscopy-theme :ensure t :defer t)
(use-package purple-haze-theme :ensure t :defer t)
(use-package bubbleberry-theme :ensure t :defer t)
(use-package spacegray-theme :ensure t :defer t)
(use-package arjen-grey-theme :ensure t :defer t)
(use-package planet-theme :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
;
; other nice themes (that aren't on ELPA)
; late-night-theme
; julie-theme
;
; load a default theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'bubbleberry t)


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

    ; install rust completion (with racer)
    ; https://github.com/racer-rust/emacs-racer
    ; make sure racer is installed first by running:
    ; rustup component add rust-src
    ; cargo install racer
    (use-package racer
      :ensure t
      :config
      (progn
        (add-hook 'rust-mode-hook #'racer-mode)
        (add-hook 'racer-mode-hook #'company-mode)
        (require 'rust-mode)
        (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
        (setq company-tooltip-align-annotations t)))
    
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
; don't create autosave files (the #filename.txt# kind)
(setq create-lockfiles nil)
; don't create back-up files (the filename.txt~ kind)
(setq make-backup-files nil)
; enable clipboard integration (NB only works outside of terminal emacs)
(setq x-select-enable-clipboard t)
; highlight matching parens
(show-paren-mode 1)
; Shut up compile saves
(setq compilation-ask-about-save nil)
; Don't save *anything* for compilation
(setq compilation-save-buffers-predicate '(lambda () nil))
;;; init ends here
(custom-set-variables
