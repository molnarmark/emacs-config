;; Emacs configuration

(require 'package)

;; Disabling the splash screen & welcome message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Maximizing Emacs
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Adding MELPA
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Ace Window
(use-package ace-window
  :ensure t
  )

;; Highlight Indent Guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (progn
    (setq highlight-indent-guides-method 'character)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)    
  )
)

;; For the theme
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
    (projectile-global-mode)
    )
 (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (progn
    (counsel-projectile-mode)
    ))

;; Which-key
(use-package which-key
  :ensure t
  :config
  (progn
    which-key-mode
   ))

;; Git Gutter
(use-package git-gutter
  :ensure t
  :config
  (progn
    (global-git-gutter-mode +1)
  ))

;; Magit
(use-package magit
  :ensure t
  :init
  (progn
  (bind-key "C-x g" 'magit-status)
  ))

;; Dumb Jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

(display-time-mode)
(global-hl-line-mode t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(winner-mode 1)

;; Key bindings
(global-set-key "\C-cd" 'kill-whole-line) 
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))

;; Custom defuns
(defun only-current-buffer () 
  (interactive)                                                                   
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-dispatch-arguments nil)
 '(package-selected-packages
   (quote
    (emacs-powerline beacon dumb-jump magit which-key Which-key counsel-projectile git-gutter projectile markdown-mode auto-complete neotree drag-stuff highlight-indent-guides use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
