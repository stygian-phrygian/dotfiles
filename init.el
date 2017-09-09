;; set up package repositories
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; enable packages
(setq package-enable-at-startup nil)
(package-initialize)

;; install "use-package" package
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

;; better M-x completion
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

;; syntax checker for a variety of languages
(use-package flycheck
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    ;; turn off flycheck for emacs lisp
    (with-eval-after-load 'flycheck
      (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

(use-package clojure-mode
  :ensure t)

(use-package clojure-mode-extra-font-locking
  :ensure t)

;; rainbow colored parentheses to ease viewing
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'slime-mode-hook #'rainbow-delimiters-mode)))

;; awesome common lisp environment
(use-package slime
  :ensure t
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))

;; golang
(use-package go-mode
  :ensure t)

;; git integration
(use-package magit
  :ensure t)

;; theme
(use-package dracula-theme
  :ensure t
  :load-path "themes"
  :config
  (load-theme 'dracula t))

;; relative line numbers
(use-package nlinum-relative
  :ensure t
  :config
  (progn
    (require 'nlinum-relative)
    ;; set-up evil
    (nlinum-relative-setup-evil)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)
    ;; change update delay
    (setq nlinum-relative-redisplay-delay 0)
    ;; change current line symbol
    (setq nlinum-relative-current-symbol "")
    ;; set line number format
    (setq nlinum-format "%4d  \u2502 ")
    ;; change line number bacground and foreground colors
    (set-face-foreground 'linum "grey")
    (set-face-background 'linum "black")))

;; turn off tool bar
(tool-bar-mode -1)
(setq
 ;; inhibit-startup-screen
 inhibit-startup-screen t
 ;; empty scratch buffer
 initial-scratch-message nil)
      

;;; init ends here
