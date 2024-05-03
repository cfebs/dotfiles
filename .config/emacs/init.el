;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Run this manually and restart to update
;; (package-refresh-contents)

;; Download packages
(setq package-list '(evil doom-themes magit evil-collection))

(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Theme
(load-theme 'doom-one t)

;; Hide bars
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Enable Evil
(require 'evil)
(evil-mode 1)
(evil-collection-init '(calendar dired calc ediff magit))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-collection magit evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
