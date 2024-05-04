;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Run this manually and restart to update
;; (package-refresh-contents)

;; Download packages
(setq package-list '(evil doom-themes magit evil-collection counsel))

(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hide bars
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Highlight trailing whitespace in red
(setq-default show-trailing-whitespace t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; Theme
(load-theme 'doom-one t)

;; Enable Evil
;; This is optional since it's already set to t by default.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
;; Turns off normal mode <tab> which is tag jumping by default
(setq evil-want-C-i-jump nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
;; Evil leader keybinds
(evil-set-leader nil (kbd ","))
(evil-define-key 'normal 'global (kbd "<leader>x") 'evil-window-delete)
(evil-define-key 'normal 'global (kbd "<leader>f") 'counsel-git)
(evil-define-key 'normal 'global (kbd "<leader>m") 'counsel-recentf)
(evil-define-key 'normal 'global (kbd "<leader>b") 'ivy-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader>gs") 'magit)
(evil-mode 1)

;; ivy, counsel mode ON
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel solarized-theme magit evil-collection doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
