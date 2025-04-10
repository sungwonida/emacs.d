;; Customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Path for the local packages
(setq local-packages-path (expand-file-name "local/packages" user-emacs-directory))
(add-to-list 'load-path local-packages-path)
(let ((default-directory local-packages-path))
  (normal-top-level-add-subdirs-to-load-path))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; helm
(use-package helm
  :config
  (helm-mode)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-l" . helm-locate)
  ("C-x C-b" . helm-mini)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-c C-r" . helm-recentf)
  ("M-y" . helm-show-kill-ring)
  ("M-s o" . helm-occur)
  ("M-g M-o M-k" . helm-google-suggest)
  :custom
  (helm-autoresize-mode t)
  (helm-move-to-line-cycle-in-source t)
  (helm-ff-search-library-in-sexp t)
  (helm-scroll-amount 8)
  (helm-ff-file-name-history-use-recentf t)
  (helm-show-completion-display-function #'helm-show-completion-default-display-function))

;; Make the helm child frame be located in the center
(defun my-helm-display-frame-center (buffer &optional resume)
  "Display `helm-buffer' in a separate frame centered in the parent frame."
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((parent (selected-frame))
           (frame-pos (frame-position parent))
           (parent-left (car frame-pos))
           (parent-top (cdr frame-pos))
           ;; Calculate width and height for the new frame.
           (width (* (/ (frame-width parent) 3) 2))
           (height (/ (frame-height parent) 3))
           tab-bar-mode
           ;; Define default-frame-alist for the new Helm frame.
           (default-frame-alist
            `((parent . ,parent)
              (width . ,width)
              (height . ,height)
              (undecorated . ,helm-use-undecorated-frame-option)
              (left-fringe . 0)
              (right-fringe . 0)
              (tool-bar-lines . 0)
              (line-spacing . 0)
              (desktop-dont-save . t)
              (no-special-glyphs . t)
              (inhibit-double-buffering . t)
              ;; Center the new frame relative to the parent.
              (left . ,(+ parent-left (/ (* (frame-char-width parent)
                                            (- (frame-width parent) width)) 2)))
              (top . ,(+ parent-top (/ (* (frame-char-height parent)
                                          (- (frame-height parent) height)) 2)))
              ;; Frame title and other properties.
              (title . "Helm")
              (vertical-scroll-bars . nil)
              (menu-bar-lines . 0)
              (fullscreen . nil)
              ;; Ensure visibility.
              (visible . t))))
      ;; ;; Set internal border color to match the default face foreground.
      ;; (set-face-background 'internal-border
      ;;                      (face-foreground 'default nil 'default))
      ;; Display the Helm buffer in a popup frame.
      ;; Use `helm-display-buffer-popup-frame` correctly.
      (helm-display-buffer-popup-frame buffer default-frame-alist))))

;; Set this function as the display function for Helm buffers.
(setq helm-display-function 'my-helm-display-frame-center)

;; helm-git-grep
(use-package helm-git-grep
  :bind ("C-c g g" . helm-git-grep-at-point))

;; helm-ag
(use-package helm-ag
  :custom
  (helm-ag-insert-at-point 'symbol))

;; helm-descbinds
(use-package helm-descbinds
  :config (helm-descbinds-mode))

;; diminish
(use-package diminish)

;; theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; face
;;  Monaco for Linux
;;      1) http://www.gringod.com/wp-upload/software/Fonts/Monaco_Linux.ttf
;;      2) Copy the downloaded font into ~/.fonts/
;;      3) Run sudo fc-cache -fv
;;  Monaco for Windows
;;      1) http://www.gringod.com/wp-upload/software/Fonts/Monaco_5.1.ttf
;;      2) Copy the downloaded font into %SystemRoot%/Fonts/
(setq default-frame-alist
            '((width . 180)
              (height . 50)))
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (setq scroll-step 1)

(set-language-environment '"Korean")
(prefer-coding-system 'utf-8)
(cond
 ((eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "Monaco-14"))
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))
 ((eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "Monaco-12")))
 ((eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(font . "Monaco-9"))
  (unicode-fonts-setup)))

(use-package all-the-icons
  :if (display-graphic-p))

;; anzu
(use-package anzu
  :config
  (global-anzu-mode +1)
  (setq anzu-search-threshold nil))

;; ediff
(use-package ediff
  :custom (ediff-split-window-function 'split-window-horizontally))

;; tern
(use-package tern
  :hook (js-mode . (lambda () (tern-mode t))))

;; tern-auto-complete
(use-package tern-auto-complete
  :config (tern-ac-setup))

;; octave
;;  Need a language server
(use-package octave
  :config
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
  (defun my-octave-mode-hook ()
    (define-key octave-mode-map (kbd "M-s M-l") 'octave-send-line)
    (define-key octave-mode-map (kbd "M-s M-b") 'octave-send-block)
    (define-key octave-mode-map (kbd "M-s M-f") 'octave-send-defun)
    (define-key octave-mode-map (kbd "M-s M-a") 'octave-send-buffer)
    (define-key octave-mode-map (kbd "M-s M-r") 'octave-send-region))
  :hook
  (octave-mode . my-octave-mode-hook))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (c++-mode . lsp)
         (c-mode . lsp))
  :commands lsp
  :config
  (setq lsp-enable-snippet nil
        lsp-prefer-capf t
        lsp-headerline-breadcrumb-enable t)
  (setq lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-flake8-max-line-length 79
        lsp-pylsp-plugins-pylint-enabled t
        lsp-pylsp-plugins-pylint-args ["--disable=C0111"] ; Example argument
        lsp-pylsp-plugins-black-enabled t
        lsp-pylsp-plugins-jedi-completion-enabled t
        lsp-pylsp-plugins-jedi-definition-enabled t
        lsp-pylsp-plugins-jedi-hover-enabled t
        lsp-pylsp-plugins-jedi-references-enabled t
        lsp-pylsp-plugins-jedi-signature-help-enabled t
        lsp-pylsp-plugins-jedi-symbols-enabled t))

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (defun my-lsp-ui-mode-hook ()
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
  (my-lsp-ui-mode-hook)
  :custom
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t))

;; company
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  (prog-mode . (lambda ()
                 (company-mode t)
                 (define-key company-mode-map [backtab] 'company-complete)
                 (define-key company-active-map [tab] 'company-complete-selection)))
  (python-mode-hook . (lambda ()
                        (when (file-remote-p default-directory)
                          (company-mode -1))))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-backends '(company-capf)))

;; flycheck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package company-lsp
  :commands company-lsp)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package general
  :ensure t
  :config
  (general-define-key
   "C-c l" '(:keymap lsp-command-map :which-key "LSP")))

;; company-box
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-is-never-other-window t))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :config
  (setq lsp-treemacs-sync-mode 1)
  (global-set-key (kbd "M-9") 'treemacs)
  (global-set-key (kbd "C-x t t") 'treemacs)
  (global-set-key (kbd "C-x t b") 'treemacs-bookmark)
  (global-set-key (kbd "C-x t C-t") 'treemacs-find-file)
  (global-set-key (kbd "C-x t 1") 'treemacs-delete-other-windows)
  (global-set-key (kbd "C-c l e") 'lsp-treemacs-errors-list)
  (global-set-key (kbd "C-c l s") 'lsp-treemacs-symbols)
  (global-set-key (kbd "C-c l c") 'lsp-treemacs-call-hierarchy)
  (global-set-key (kbd "C-c l r") 'lsp-treemacs-references))

;; magit
(use-package magit
  :diminish
  magit-auto-revert-mode
  :config
  (setq magit-refresh-verbose t)
  (setq magit-refresh-status-buffer nil)
  (setq auto-revert-buffer-list-filter
        'magit-auto-revert-repository-buffer-p)
  :bind
  ("C-c v s" . magit-status)
  ("C-c v y" . magit-show-refs-popup)
  ("C-c v l" . magit-log-head))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (or (not (eq system-type 'windows-nt))
	    (memq window-system '(mac ns)))
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH"))
    (exec-path-from-shell-initialize)))

;; Auto-save, File Lock and Backup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Mark the region in Windows
(cond
 ((string-equal system-type "windows-nt")
  (global-set-key [C-kanji] 'set-mark-command)))

;; Suppress the startup page
(setq inhibit-startup-message t)

;; Google Translate
(use-package google-translate
  :config
  (defun my-google-translate-query-translate-ko-to-en ()
    (interactive)
    (let ((google-translate-default-source-language "ko")
          (google-translate-default-target-language "en"))
      (google-translate-query-translate)))
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ko")
  :bind
  ("C-M-g" . google-translate-at-point)
  ("M-g t" . google-translate-query-translate)
  ("M-g k t" . my-google-translate-query-translate-ko-to-en))

;; Quick Insert-Date
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %p %l:%M")))

;; Easier transition between the windows
;;	 M-up, M-down, M-left, and M-right keys.
(windmove-default-keybindings 'meta)

;; spawning the windows
(fset 'spawn-window-right
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 51 M-left] 0 "%d")) arg)))
(fset 'spawn-window-left
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 51 M-right] 0 "%d")) arg)))
(fset 'spawn-window-down
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 50 M-down] 0 "%d")) arg)))
(fset 'spawn-window-up
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 50 M-up] 0 "%d")) arg)))
(bind-key* "C-<right>" 'spawn-window-left)
(bind-key* "C-<left>" 'spawn-window-right)
(bind-key* "C-<down>" 'spawn-window-down)
(bind-key* "C-<up>" 'spawn-window-up)

;; dired-mode
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

(add-hook 'dired-mode-hook
          (function (lambda ()
     		      (load "dired-x")
                      ;; Set dired-x buffer-local variables here.  For example:
                      (setq dired-omit-files-p t)
     		      (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
     		      (setq dired-omit-extensions '("~"))
                      )))

(defun my-dired-mode-hook ()
  (define-key dired-mode-map [backspace] 'dired-up-directory))
(add-hook 'dired-mode-hook 'my-dired-mode-hook)

;; Development generic
(add-hook 'prog-mode-hook '(lambda () (setq tab-width 4)))
(add-hook 'before-save-hook 'prog-delete-trailing-whitespace)

(defun prog-delete-trailing-whitespace ()
  (interactive)
  (when (derived-mode-p 'prog-mode 'emacs-lisp-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'prog-delete-trailing-whitespace)

;;; yasnippet
(use-package yasnippet
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
              ("C-c p s s " . helm-ag)
              ("C-c p s r " . helm-ag-project-root)))

;; CMake
(setq cmake-tab-width 4)

;; C
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(defun my-c-mode-common-hook ()
  (hs-minor-mode t)
  (define-key c-mode-base-map (kbd "M-o") 'ff-get-other-file)
  (define-key c-mode-base-map (kbd "M-m") 'helm-semantic-or-imenu)
  (local-set-key (kbd "C-c u") 'hs-toggle-hiding)
  (local-set-key (kbd "C-c <down>") 'hs-hide-all)
  (local-set-key (kbd "C-c <up>") 'hs-show-all))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun highlight-if-0/1 ()
  "Modify the face of text in between #if 0 ... #endif."
  (interactive)
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list
        '((#("1" 0 1
             (fontified nil))
           nil
           (foreground-color . "dim gray")
           both nil)
          (#("0" 0 1
             (fontified nil))
           (foreground-color . "dim gray")
           nil
           both nil)))
  (cpp-highlight-buffer t))

(defun c-cpp-highlight-if-0/1 ()
  (when (derived-mode-p 'c-mode 'c++-mode)
    (highlight-if-0/1)))
(add-hook 'c-mode-common-hook 'c-cpp-highlight-if-0/1)
(add-hook 'before-save-hook 'c-cpp-highlight-if-0/1)

;; Python
(defun my-python-mode-hook ()
  (define-key python-mode-map (kbd "M-m") 'helm-semantic-or-imenu))
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook (lambda () (setq forward-sexp-function nil)))
(define-key lisp-mode-shared-map (kbd "M-m") 'helm-semantic-or-imenu)

;; smartparens
(use-package smartparens
  :config
  (smartparens-global-mode t)
  :diminish
  (smartparens-mode "()"))

;; Recent files
;; (require 'recentf)
;; (setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-keep
	  '(lambda (file)
		 (cond
          ((file-remote-p file nil t)
           (file-readable-p file))
          ((file-readable-p file)))))

;; redo
(use-package redo+
  :bind ("C-." . redo))

;; smart-compile
(use-package smart-compile)

;; tdd
;;		Turn on/off the mode manually because it runs recompile automatically
;;		after saving any buffer no matters it's prog-mode or not.
;; (use-package tdd
;;   :straight nil
;;   :load-path local-packages-path
;;   :init (defun selective-tdd-after-save ()
;; 		  (when (derived-mode-p 'c-mode 'c++-mode 'cmake-mode) (tdd-after-save)))
;;   :config (custom-set-variables '(tdd-test-function (smart-compile)))
;;   :hook (after-save . selective-tdd-after-save)
;;   :bind ([f12] . tdd-mode))

;; Semantic Refactor
(use-package srefactor
  :init
  (defun my-srefactor-mode-hook ()
	(define-key c-mode-map (kbd "M-s M-RET") 'srefactor-refactor-at-point)
	(define-key c++-mode-map (kbd "M-s M-RET") 'srefactor-refactor-at-point))
  :hook
  (c-mode . my-srefactor-mode-hook)
  (c++-mode . my-srefactor-mode-hook))

;; Pandoc
(if (or (eq system-type 'gnu/linux) (eq system-type 'darwin))
    (setq markdown-command "pandoc"))

;; Publishing the live buffer
;;		http://stackoverflow.com/questions/36183071/how-can-i-real-time-preview-markdown-in-emacs]]
;;		1) M-x httpd-start
;;		2) M-x impatient-mode
;;		3) localhost:8080/imp
(use-package simple-httpd)
(use-package impatient-mode)
;;		4) M-x imp-set-user-filter
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;; Replace the region with yank buffer
(delete-selection-mode 1)

;; conda
(use-package conda
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t)
  (custom-set-variables
   '(conda-anaconda-home (concat (file-name-as-directory (getenv "HOME")) "miniconda3")))
  :hook
  (find-file . (lambda ()
                 (when (bound-and-true-p conda-project-env-path)
                   (conda-env-activate-for-buffer)))))

;; highlight-symbol
(use-package highlight-symbol
  :init
  (defun my-highlight-symbol-hook ()
	(highlight-symbol-mode)
	(define-key prog-mode-map [(shift control s)] 'highlight-symbol-next)
	(define-key prog-mode-map [(shift control r)] 'highlight-symbol-prev)
	(define-key prog-mode-map (kbd "C-c h h") 'highlight-symbol)
	(define-key prog-mode-map (kbd "C-c h r") 'highlight-symbol-remove-all)
	(define-key prog-mode-map (kbd "C-c h q") 'highlight-symbol-query-replace))
  :hook
  (prog-mode . my-highlight-symbol-hook)
  :custom
  (highlight-symbol-on-navigation-p t))

;; Saving Emacs Sessions
;; (desktop-save-mode 1)

;; cmake-mode
(use-package cmake-mode)

;; docker
(use-package docker
  :bind ("C-c d c" . docker-containers))

;; tramp
(setq tramp-verbose 1)
;; use the customizations in ~/.ssh/config
(customize-set-variable 'tramp-use-ssh-controlmaster-options "~/.ssh/config")

;; markdown
(use-package markdown-mode)

;; winner-mode
(winner-mode)

;; vterm
(use-package vterm
  :bind
  (:map vterm-mode-map ("C-v" . vterm-yank))
  (:map vterm-mode-map ("C-k" . vterm-send-C-k))
  (:map vterm-mode-map ("C-y" . vterm-send-C-y))
  (:map vterm-mode-map ("C-g" . vterm-send-C-g)))

;; eshell
(use-package eshell
  :hook (eshell-mode . (lambda ()
    		             (define-key eshell-mode-map (kbd "C-c C-h") 'helm-eshell-history))))

;; cuda-mode
(use-package cuda-mode)

;; astyle
(use-package astyle
  :when (executable-find "astyle")
  :hook (c-mode-common . astyle-on-save-mode))

;; quelpa
(use-package quelpa-use-package)

;; topsy
(use-package topsy
  :after quelpa-use-package
  :quelpa (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook
  (magit-section-mode . topsy-mode))

;; mini-frame
(use-package mini-frame
  :custom
  (mini-frame-show-parameters
   '((top . 10)
     (width . 0.7)
     (left . 0.5))))

;; Smooth mouse-wheel scrolling
(defun enable-smooth-mouse-wheel-scrolling ()
  "Enable smooth mouse-wheel scrolling."
  (interactive)
  (setq pixel-scroll-precision-mode 1) ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1)) ;; keyboard scroll one line at a time

(defun disable-smooth-mouse-wheel-scrolling ()
  "Disable smooth mouse-wheel scrolling."
  (interactive)
  (setq pixel-scroll-precision-mode nil)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift)
                                     . hscroll)
                                    ((meta))
                                    ((control meta)
                                     . global-text-scale)
                                    ((control)
                                     . text-scale)))
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  (setq scroll-step 0))

;; ztree
(use-package ztree)

;; large-file-warning-threshold
(setq large-file-warning-threshold nil)


;;; init.el ends here
