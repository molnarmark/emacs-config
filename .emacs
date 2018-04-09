;; Emacs configuration

(require 'package)

;; Adding MELPA
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Highlight Indent Guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (progn
    (setq highlight-indent-guides-method 'character)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)    
  )
)

(use-package alect-themes
  :ensure t
)

;; Load the theme
(load-theme 'alect-black t)

;; Auto Complete
(use-package auto-complete
  :ensure t
  :config
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
  ))

;; Expand Region
(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-v") 'er/expand-region)
  ))

;; Setting the  default font
(set-face-attribute 'default nil
  :family "Consolas"
  :height 140
  :weight 'normal
  :width 'normal
 )

;; Turning off unnecessary menu bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers are a great thing to have
(global-linum-mode t)

;; Counsel
(use-package counsel
  :ensure t
)

;; NeoTree
(use-package neotree
  :ensure t
  :config
  (progn
    (global-set-key [f8] 'neotree-toggle)
  )
)

;; Swiper
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )
)

;; Drag Stuff
(use-package drag-stuff
  :ensure t
  :config
  (progn
    (drag-stuff-global-mode 1)
    (drag-stuff-define-keys)
  ))

;; Web Mode
(use-package web-mode
  :ensure t
  )

;; PHP Mode
(use-package php-mode
  :ensure t
  )

;; Smart parens
(use-package smartparens
  :ensure t
  )

(smartparens-global-mode t)

;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-mode t)
  ))

;; Key bindings
(global-set-key "\C-cd" 'kill-whole-line) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile markdown-mode auto-complete neotree drag-stuff highlight-indent-guides use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
