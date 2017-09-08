
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


(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; More configuration goes here
  )

(use-package slime
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package dracula-theme
  :ensure t
  :load-path "themes"
  :config
  (load-theme 'dracula t)
  )

(use-package nlinum-relative
  :ensure t
  :config
  ;; something else you want
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
    (set-face-background 'linum "black")
  )
)
