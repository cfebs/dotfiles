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
		vimrc-mode
		ws-butler
		vterm
		orderless
		vertico
		consult
		marginalia
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

;; tree sitter stuff
;; MANUAL: run this to install all grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (php "https://github.com/tree-sitter/tree-sitter-php")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; override major modes to treelist versions
(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (php-mode . php-ts-mode)
   (tsx-mode . tsx-ts-mode)
   (toml-mode . toml-ts-mode)
   (python-mode . python-ts-mode)))

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
(evil-define-key 'normal 'global (kbd "<leader>f")	'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader>m")	'consult-recent-file)
(evil-define-key 'normal 'global (kbd "<leader>b")	'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>t")	'find-file)
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
;;(ivy-mode 1)
;;(setq ivy-use-virtual-buffers t)
;;(setq enable-recursive-minibuffers t)
;;(setq ivy-count-format "(%d/%d) ")

(vertico-mode 1)
(marginalia-mode 1)
(setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))

;; vimrc highlighting
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
