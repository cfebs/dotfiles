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
(evil-mode 1)

;; Highlight trailing whitespace in red
(setq-default show-trailing-whitespace t)
