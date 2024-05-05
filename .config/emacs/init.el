;; Set a place for custom values (but won't be loaded)
(setq custom-file (concat user-emacs-directory "/custom.el"))

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
;; Enable evil early
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(evil-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hide bars
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Highlight trailing whitespace in red
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

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
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files '("~/Sync/org"))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Plugin stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (display-graphic-p)
	(progn
		(set-fringe-mode 10)
		(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
		(scroll-bar-mode -1)
  ))

;; Evil binds
(evil-set-leader nil (kbd ","))
(evil-define-key 'normal 'global (kbd "<leader>x")	'evil-window-delete)
(evil-define-key 'normal 'global (kbd "<leader>f")	'counsel-git)
(evil-define-key 'normal 'global (kbd "<leader>m")	'counsel-recentf)
(evil-define-key 'normal 'global (kbd "<leader>b")	'ivy-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader>t")	'counsel-fzf)
(evil-define-key 'normal 'global (kbd "<leader>gs")	'magit)

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
;; Can run recentf save periodically
;; (run-at-time nil (* 5 60) 'recentf-save-list)

;; Theme
(load-theme 'doom-solarized-dark t)

;; ivy, counsel mode ON
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")

;; vimrc highlighting
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
