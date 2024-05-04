;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Run this manually and restart to update
;; (package-refresh-contents)

;; Download packages
(setq package-list
	  '(evil
		doom-themes
		magit
		evil-collection
		counsel
		vimrc-mode
		ws-butler
		))

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
;; Line numbers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default indicate-empty-lines t)
;; Delete trailing whitespace on save
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'fundamental-mode)
              (delete-trailing-whitespace))))
;; Tabs
(setq-default tab-width 4)

;; backups
(let ((backup-dir "~/tmp/emacs/backups")
	  (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
	(when (not (file-directory-p dir))
	  (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
		auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
		auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
		tramp-backup-directory-alist `((".*" . ,backup-dir))
		tramp-auto-save-directory auto-saves-dir))

;; some options for version control
;;version-control t		; Use version numbers on backups,
;;kept-new-versions 5	; keep some new versions
;;kept-old-versions 2	; and some old ones, too
(setq backup-by-copying t	 	; Don't delink hardlinks
	  delete-old-versions t)	; Clean up the backups

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
;; Can run recentf save periodically
;; (run-at-time nil (* 5 60) 'recentf-save-list)

;; Theme
(load-theme 'doom-solarized-dark t)

;; Enable Evil
;; This is optional since it's already set to t by default.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
;; Turns off normal mode <tab> which is tag jumping by default
(setq evil-want-C-i-jump nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
;; make tab work as expected in insert mode
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
;; Evil leader keybinds
(evil-set-leader nil (kbd ","))
(evil-define-key 'normal 'global (kbd "<leader>x")		'evil-window-delete)
(evil-define-key 'normal 'global (kbd "<leader>f")		'counsel-git)
(evil-define-key 'normal 'global (kbd "<leader>m")		'counsel-recentf)
(evil-define-key 'normal 'global (kbd "<leader>b")		'ivy-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader>t")		'counsel-fzf)
(evil-define-key 'normal 'global (kbd "<leader>gs")		'magit)
(evil-mode 1)

;; ivy, counsel mode ON
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; vimrc highlighting
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; TODO learn wtf this is, appears from time to time
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ws-butler vimrc-mode counsel solarized-theme magit evil-collection doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
