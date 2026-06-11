;; Customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; --- Local packages ---------------------------------------------------------
(defconst local-packages-dir
  (expand-file-name "local/packages" user-emacs-directory))

;; Add DIR itself if it has loose .el files, and every subdir that has .el files.
(defun my/add-el-dirs-to-load-path (dir)
  "Recursively add to `load-path` any subdir of DIR that contains .el files."
  (when (file-directory-p dir)
    (when (directory-files dir nil "\\.el\\'")
      (add-to-list 'load-path dir))
    (dolist (entry (directory-files dir t "\\`[^.]")) ; skip . and ..
      (when (file-directory-p entry)
        (my/add-el-dirs-to-load-path entry)))))

;; Do it once at startup
(my/add-el-dirs-to-load-path local-packages-dir)
;; ---------------------------------------------------------------------------

(let ((macro-file (expand-file-name "local/macros/macros.el" user-emacs-directory)))
  (when (file-exists-p macro-file)
    (load-file macro-file)
    ;; Check if the function exists (not variable)
    (when (fboundp 'copy-scenario-from-data-center)
      ;; Get the function definition to use as macro
      (let ((macro-def (symbol-function 'copy-scenario-from-data-center)))
        ;; Add your macro to the head of the macro ring
        (push macro-def kmacro-ring)
        ;; Trim the macro ring to its max size
        (when (> (length kmacro-ring) kmacro-ring-max)
          (setq kmacro-ring (butlast kmacro-ring (- (length kmacro-ring) kmacro-ring-max))))
        ;; Set the last-kbd-macro to your newly added macro
        (setq last-kbd-macro macro-def)))))

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
(straight-use-package 'bind-key)  ; Required for use-package
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

;; Configure straight.el (without use-package)
(setq straight-use-package-by-default t)  ; Replace the use-package call

;; Prefer reusing the selected window for anything that uses display-buffer.
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-same-window)))

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
  (helm-show-completion-display-function #'helm-show-completion-default-display-function)
  (helm-buffer-max-length 60))

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
                                          (- (frame-height parent) height)) 1)))
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

(defun my-helm-display-bottom (buffer &optional _resume)
  "Display Helm BUFFER at the bottom of the frame."
  (let ((display-buffer-alist
         '(("\\`\\*helm.*?\\*\\'"
            (display-buffer-in-side-window)
            (inhibit-same-window . t)
            (window-height . 0.4)
            (side . bottom)))))
    (display-buffer buffer)))

;; Set the custom function as the display function for Helm buffers.
;; For WSL, avoid applying the custom function because it frequently creaks.
(cond ((and (eq system-type 'gnu/linux)
            (getenv "WSLENV"))
       (setq helm-display-function #'my-helm-display-bottom))
      (t
       (setq helm-display-function #'my-helm-display-frame-center)))

;; helm-git-grep
(use-package helm-git-grep
  :bind ("C-c g g" . helm-git-grep-at-point))

;; helm-ag
(defvar my/search-ignores '("svnet_output" "*.min.js" "compile_commands.json"))

(defun my/helm-ag-options-for (backend)
  (pcase backend
    ('ag (mapconcat (lambda (p)
                      (format "--ignore=%s" (shell-quote-argument p)))
                    my/search-ignores " "))
    ('rg (mapconcat (lambda (p)
                      (format "-g %s" (shell-quote-argument (concat "!" p))))
                    my/search-ignores " "))))

(use-package helm-ag
  :custom
  (helm-ag-insert-at-point 'symbol)
  (helm-ag-use-grep-ignore-list nil)
  (helm-ag-command-option (my/helm-ag-options-for 'ag)))

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
(setq default-frame-alist
      '((width . 180)
        (height . 50)
        (font . "D2Coding")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq scroll-step 1)

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

;;; --- macOS 한글 IME 단축키 호환 ----------------------------------------------
;; 시스템 한글(2벌식) 입력 중에도 수식키(C-/M-/s-)와 프리픽스 시퀀스(C-x o,
;; M-s h r, C-c v l ...)가 동작하도록 하는 모듈.  key-translation-map 자모
;; 역번역 + sis(macism) 입력소스 자동전환.  메커니즘/한계/확장법은 모듈
;; Commentary 참고: local/packages/macos/korean-ime-keys.el
(use-package korean-ime-keys
  :if (eq system-type 'darwin)
  :straight nil
  :load-path (lambda () (list local-packages-dir))
  :demand t)
;; ---------------------------------------------------------------------------

(use-package nerd-icons
  :ensure t)

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

;;; --- treesit --------------------------------------------------------------
(use-package treesit-compat
  :straight nil
  :load-path (lambda () (list local-packages-dir)))
;; ---------------------------------------------------------------------------

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

;;; --- TRAMP remote-file performance guards --------------------------------
;; Predicate used by hooks/filters below.
(defun my/file-is-remote-p () (file-remote-p default-directory))

;; lsp-mode: skip starting LSP on remote buffers (referenced from lsp-mode :hook).
;; Safe here because the major-mode-hook runs once per buffer; we just elect
;; not to call lsp-deferred — no mode toggling, no hook recursion.
(defun my/lsp-deferred-unless-remote ()
  (unless (my/file-is-remote-p) (lsp-deferred)))

;; magit-auto-revert: filter used in magit :config below — skips remote-tracked buffers.
(defun my/magit-auto-revert-filter-no-remote (buffer)
  (and (magit-auto-revert-repository-buffer-p buffer)
       (not (with-current-buffer buffer
              (file-remote-p default-directory)))))

;; Disable backups/lockfiles/auto-saves AND the heavy minor modes on remote files.
;; Done from find-file-hook (one fire per visit) — NOT from each mode's *-mode-hook,
;; because `define-minor-mode` runs the mode-hook on every toggle, so calling
;; (flycheck-mode -1) from flycheck-mode-hook would re-fire the hook and recurse
;; until max-lisp-eval-depth is exceeded.
(setq remote-file-name-inhibit-locks t)   ; Emacs 28+
(defun my/disable-heavy-modes-on-remote ()
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    ;; backups / lockfiles / autosave — each saves one extra TRAMP roundtrip per save.
    (setq-local make-backup-files nil)
    (setq-local create-lockfiles nil)
    (setq-local auto-save-default nil)
    (auto-save-mode -1)
    ;; Disable heavy IDE modes if they were turned on by their globalized counterparts.
    (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
    (when (bound-and-true-p company-mode)  (company-mode -1))))
(add-hook 'find-file-hook #'my/disable-heavy-modes-on-remote)
;; -------------------------------------------------------------------------

;;; --- lsp core -------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c++-ts-mode    . my/lsp-deferred-unless-remote)
         (c-ts-mode      . my/lsp-deferred-unless-remote)
         (python-ts-mode . my/lsp-deferred-unless-remote)
         (c++-mode       . my/lsp-deferred-unless-remote)
         (c-mode         . my/lsp-deferred-unless-remote)
         (python-mode    . my/lsp-deferred-unless-remote))
  :init
  (setq lsp-auto-guess-root t)
  :config
  ;; generic, cross-language defaults
  (setq lsp-prefer-capf                  t
        lsp-headerline-breadcrumb-enable t
        lsp-enable-file-watchers         nil
        lsp-enable-symbol-highlighting   nil
        lsp-log-io                       nil  ; Disable heavy IO logging
        lsp-response-timeout             30
        gc-cons-threshold                (* 100 1024 1024)  ; 100MB GC threshold
        read-process-output-max          (   * 3 1024 1024)  ; 3MB output buffer)
        lsp-enable-indentation           nil
        lsp-enable-on-type-formatting    nil))
;; ---------------------------------------------------------------------------

;;; --- language layers ------------------------------------------------------
(use-package cpp-config-lsp-clangd
  :straight nil
  :load-path (lambda () (list local-packages-dir))
  :demand t)

(use-package python-config-lsp-pyright
  :straight nil
  :load-path (lambda () (list local-packages-dir))
  :demand t)

(use-package yaml-config
  :straight nil
  :load-path (lambda () (list local-packages-dir))
  :demand t)

(use-package cmake-config
  :straight nil
  :load-path (lambda () (list local-packages-dir))
  :demand t)

(use-package json-config
  :straight nil
  :load-path (lambda () (list local-packages-dir))
  :demand t)

(use-package lua-config
  :straight nil
  :load-path (lambda () (list local-packages-dir))
  :demand t)
;; ---------------------------------------------------------------------------

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
        lsp-ui-doc-use-childframe nil  ; Disable heavy rendering
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics nil))  ; Disable constant analysis

;; company
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  (prog-mode . (lambda ()
                 (company-mode t)
                 (define-key company-mode-map [backtab] 'company-complete)
                 (define-key company-active-map [tab] 'company-complete-selection)))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-show-quick-access t
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

;; NOTE: which-key is intentionally not enabled: helm-descbinds-mode
;; (enabled above) replaces the C-h-after-prefix flow and hard-blocks
;; `which-key-mode' (see `helm-descbinds-disable-which-key').

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
        #'my/magit-auto-revert-filter-no-remote)
  :bind
  ("C-c v s" . magit-status)
  ("C-c v y" . magit-show-refs-popup)
  ("C-c v l" . magit-log-head))

(defun my-magit-log-with-git-dir (gitdir)
  "Show Magit log using GITDIR as --git-dir for this invocation."
  (interactive "DGit dir (the directory that contains HEAD, objects/, refs/, ...): ")
  (let ((magit-git-global-arguments
         (cons (concat "--git-dir=" (expand-file-name gitdir))
               magit-git-global-arguments)))
    (call-interactively #'magit-log-all)))

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
          (lambda ()
            (unless (file-remote-p default-directory)
              (load "dired-x")
              ;; Set dired-x buffer-local variables here.  For example:
              (setq dired-omit-files-p t)
              (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
              (setq dired-omit-extensions '("~")))))

(defun my-dired-toggle-human-readable ()
  "Toggle human-readable file sizes in current Dired buffer."
  (interactive)
  (setq-local dired-actual-switches
              (if (string-match "h" dired-actual-switches)
                  (replace-regexp-in-string "h" "" dired-actual-switches)
                (concat dired-actual-switches "h")))
  (dired-sort-other dired-actual-switches))

(defun my-dired-mode-hook ()
  (define-key dired-mode-map [backspace] 'dired-up-directory)
  (define-key dired-mode-map "#" 'my-dired-toggle-human-readable))
(add-hook 'dired-mode-hook 'my-dired-mode-hook)

;; Development generic
(add-hook 'prog-mode-hook '(lambda () (setq tab-width 4)))
(add-hook 'before-save-hook 'prog-delete-trailing-whitespace)

(defun prog-delete-trailing-whitespace ()
  (interactive)
  (when (derived-mode-p 'prog-mode 'emacs-lisp-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'prog-delete-trailing-whitespace)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-switch-project-action #'projectile-vc)

  :bind (:map projectile-mode-map
              ("s-p"        . projectile-command-map)
              ("C-c p"      . projectile-command-map)
              ("C-c p s s " . helm-ag)
              ("C-c p s r " . helm-ag-project-root))

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*projectile-files-errors\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))

  (defun my/projectile-reset-root-cache ()
    "Wipe projectile's in-memory project-root cache.
Use when projectile incorrectly reports a directory as \"not a project\"
after a .git (or other root marker) was created mid-session."
    (interactive)
    (clrhash projectile-project-root-cache)
    (message "projectile-project-root-cache cleared")))

;; CMake
(setq cmake-tab-width 4)

;; Lisp
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
(defun markdown-filter (buffer)
  "Render Markdown BUFFER to HTML for impatient-mode."
  (princ
   (with-temp-buffer
     (let ((tmp (generate-new-buffer-name "*markdown-html*")))
       (with-current-buffer buffer
         (markdown tmp)) ; Convert to HTML using markdown-mode
       (set-buffer tmp)
       (format "<!DOCTYPE html>
<html>
<head>
<title>Markdown Preview</title>
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
</head>
<body>
<article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">
%s
</article>
</body>
</html>"
               (buffer-string))))
   (current-buffer)))

(defun org-html-filter (buffer)
  "Render Org BUFFER to HTML for impatient-mode with buffer cleanup."
  (princ ; Return the HTML string to impatient-mode
   (with-temp-buffer
     (let ((tmp (get-buffer-create "*org-html*"))) ; Reuse single buffer
       (with-current-buffer buffer
         (save-selected-window ; Prevent window switching
           (org-export-to-buffer 'html tmp nil nil t t)))
       (with-current-buffer tmp
         (prog1 ; Return HTML while cleaning up
             (format "<!DOCTYPE html>
<html>
<head>
<title>Org Presentation</title>
<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/water.css@2/out/water.min.css\">
<style>
body { max-width: 900px; margin: auto; padding: 2em; }
</style>
</head>
<body>
%s
</body>
</html>"
                     (buffer-string))
           (kill-buffer tmp))))))) ; Clean up immediately

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
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Skip VC entirely on remote paths (canonical TRAMP manual fix).
  ;; Local VC is unaffected; remote dirs no longer trigger .git/.hg/.svn/... probes.
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

;; --- Recover from stale SSH masters after sleep/wake or Wi-Fi reconnect ---
;; When macOS sleeps (or Wi-Fi/VPN flaps), a ControlMaster's TCP dies but the
;; master process and its socket file remain. A subsequent TRAMP op attaches
;; to that zombie master and hangs until ssh keepalives notice (~minutes), or
;; trips over a dead socket FILE that delete-process left behind ("disabling
;; multiplexing" noise) and stalls connection setup.
;;
;; IMPORTANT: with `tramp-use-connection-share' = t (the macOS default), TRAMP
;; injects its OWN "-o ControlPath=tramp.%C" and on macOS that is a RELATIVE
;; path, so TRAMP's masters live at `tramp.*' under `temporary-file-directory'
;; (e.g. ~/.cache/emacs/) — NOT in ~/.ssh.  The ~/.ssh/control-* sockets only
;; hold the ProxyJump *hop* masters (those use ~/.ssh/config).  The old version
;; of this command cleaned only ~/.ssh/control-*, so for a DIRECT host like
;; `data' it never removed TRAMP's actual stale socket.  Clean both locations.
(defun my/tramp-reset-all ()
  "Tear down all TRAMP connections AND stale SSH ControlMaster sockets.
Run this when TRAMP hangs reconnecting after a sleep/wake cycle, or when a
remote that previously worked suddenly stalls mid-operation."
  (interactive)
  ;; 1. Drop Emacs-side connection state + cached connection properties.
  (tramp-cleanup-all-connections)
  ;; 2. Gracefully exit, then unlink, every ControlMaster socket in both
  ;;    locations.  `-O exit' is a local control-socket op (it does not touch
  ;;    the network), so it returns fast even on a TCP-dead master; the `x'
  ;;    host arg is required by ssh's parser but unused — `-S' supplies the
  ;;    real path.
  (dolist (sock (append
                 (file-expand-wildcards
                  (expand-file-name "tramp.*" temporary-file-directory))
                 (when (bound-and-true-p small-temporary-file-directory)
                   (file-expand-wildcards
                    (expand-file-name "tramp.*" small-temporary-file-directory)))
                 (file-expand-wildcards (expand-file-name "~/.ssh/control-*"))))
    (ignore-errors
      (call-process "ssh" nil nil nil
                    "-O" "exit" "-o" "BatchMode=yes" "-S" sock "x"))
    (when (file-exists-p sock)
      (ignore-errors (delete-file sock))))
  (message "TRAMP and SSH ControlMaster state reset. Reconnect normally."))

(global-set-key (kbd "C-c t r") #'my/tramp-reset-all)
;; -------------------------------------------------------------------------

;;; --- Stop helm-find-files from auto-connecting (freezing) on remote paths ---
;; helm computes candidates from a 0.05s idle timer as you type, and
;; `helm-find-files-get-candidates' deliberately binds `non-essential' to nil so
;; TRAMP OPENS the connection mid-typing (helm's own comment there: "the tramp
;; connection is triggered").  Inside that timer the minibuffer is occupied, so
;; any slow open — unreachable ProxyJump, password prompt, dead ControlMaster —
;; freezes ALL of Emacs until C-g.  Every TRAMP hang we hit entered through here
;; (`timer-event-handler' -> helm-check-minibuffer-input -> file-directory-p).
;;
;; TRAMP already refuses to open a NEW connection when `non-essential' is non-nil:
;; `tramp-maybe-open-connection' does (unless (tramp-connectable-p vec)
;; (throw 'non-essential ...)).  Rather than fight helm's internal nil binding at
;; each probe site (helm-ff-set-pattern, helm-ff--transform-pattern-for-completion,
;; an inner expand-file-name, ...), gate the single chokepoint they all funnel
;; through — `tramp-connectable-p' — for the duration of candidate computation:
;; report a not-yet-open host as non-connectable.  Net effect: typing a remote
;; path never auto-connects; the connection opens when you press RET, a normal
;; command context where TRAMP can prompt for a password and C-g aborts cleanly.
;; Already-open connections still complete, and host-name completion (/ssh:<TAB>,
;; which never connects) is unaffected.
(defvar my/ff-completing nil
  "Non-nil while helm is computing `find-files' candidates.")
(defun my/ff-completing-around (orig &rest args)
  (let ((my/ff-completing t)) (apply orig args)))
(defun my/tramp-connectable-p-around (orig vec-or-filename)
  "During helm completion, treat an unopened host as non-connectable."
  (if my/ff-completing
      (let ((non-essential t)) (funcall orig vec-or-filename))
    (funcall orig vec-or-filename)))
(with-eval-after-load 'tramp
  (advice-add 'tramp-connectable-p :around #'my/tramp-connectable-p-around))
(with-eval-after-load 'helm-files
  (advice-add 'helm-find-files-get-candidates :around #'my/ff-completing-around))
;; -------------------------------------------------------------------------

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

;; ghostel
(use-package ghostel
  :straight (ghostel :type git :host github :repo "dakra/ghostel"
                     :files (:defaults
                             "etc" "src"
                             "build.zig" "build.zig.zon"
                             "build.sh" "Makefile"))
  :custom
  ;; copy-mode를 vterm-copy-mode처럼: M-w는 복사 후 모드 유지, 종료는
  ;; C-c C-t로만. fast-exit가 t면 M-w/아무 키나 누를 때 빠져나가버린다.
  (ghostel-readonly-fast-exit nil))

;;; --- macOS(NS) 한글 IME preedit가 vterm/ghostel에서 0열로 튀는 문제 수정 ---
;;; 원인: NS 포트는 조합 중 글자를 point 위치의 zero-width 오버레이
;;; (ns-working-overlay, 'after-string)로 표시한다. vterm/ghostel은 화면 갱신 때
;;; 커서 줄을 통째로 delete-region 후 재삽입하는데, 삭제 범위 안의 zero-width
;;; 오버레이는 Emacs 마커 규칙에 따라 삭제 시작점(=줄 맨 앞)으로 끌려간다.
;;; 그 뒤 preedit가 0열에 그려져 줄 전체가 한 칸 밀려 보인다.
;;; (front/rear-advance 어느 조합도 살아남지 못함 — batch 실험으로 확인)
;;; 해법: 두 터미널의 redraw 단일 진입점 직후, 살아있는 ns-working-overlay를
;;; 터미널 커서 위치로 되돌린다. 확정 텍스트 경로(키 이벤트->pty)는 불변.
;;; 주의: 내부 심볼(vterm--delayed-redraw 등)에 의존 — 패키지 업데이트로 이름이
;;; 바뀌면 에러 없이 조용히 원래 증상으로 돌아간다.
(when (featurep 'ns)                ; NS(Cocoa) 빌드에서만. Linux/WSL은 no-op.
  (defun my/ns-preedit--reanchor (buffer cursor-pt-fn editable-p-fn)
    "BUFFER 안에 살아있는 NS preedit 오버레이를 터미널 커서로 옮긴다.
CURSOR-PT-FN: 터미널 커서 위치(없으면 nil)를 돌려주는 함수.
EDITABLE-P-FN: 키 입력이 pty로 전달되는 상태인지 알려주는 함수."
    ;; process-filter 꼬리에서 실행되므로 어떤 에러도 출력 처리를 끊으면 안 됨.
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay) ; echo-area 표시 중엔 정수 -> skip
                 (buffer-live-p buffer)
                 (eq (overlay-buffer ns-working-overlay) buffer))
        (with-current-buffer buffer
          (when (funcall editable-p-fn)
            (let* ((pt (or (funcall cursor-pt-fn) (point)))
                   (pt (and (integerp pt)
                            (max (point-min) (min pt (point-max))))))
              (when pt
                (move-overlay ns-working-overlay pt pt buffer))))))))

  ;; --- vterm: 모든 lisp 경유 redraw는 vterm--delayed-redraw 하나를 거친다 ---
  (with-eval-after-load 'vterm
    (defun my/ns-preedit--vterm-reanchor (buffer)
      (my/ns-preedit--reanchor
       buffer #'vterm--get-cursor-point
       (lambda () (not (bound-and-true-p vterm-copy-mode)))))
    (advice-add 'vterm--delayed-redraw :after #'my/ns-preedit--vterm-reanchor)
    ;; 창 리사이즈는 C(Fvterm_set_size)에서 직접 term_redraw를 부르므로
    ;; vterm--delayed-redraw를 안 거친다. 리사이즈 직후에도 한 번 재고정.
    (defun my/ns-preedit--vterm-reanchor-after-resize (process &rest _)
      (when (processp process)
        (my/ns-preedit--vterm-reanchor (process-buffer process))))
    (advice-add 'vterm--window-adjust-process-window-size :after
                #'my/ns-preedit--vterm-reanchor-after-resize))

  ;; --- ghostel: 모든 redraw는 단일 진입점 하나를 거친다 ---
  ;; (v0.34.0에서 ghostel--delayed-redraw → ghostel--redraw-now로 리네임.
  ;;  시그니처(buffer)와 단일 진입점 역할은 동일 — 리사이즈 경로
  ;;  ghostel--window-adjust-process-window-size도 이 함수를 부름)
  (with-eval-after-load 'ghostel
    (defun my/ns-preedit--ghostel-reanchor (buffer)
      (my/ns-preedit--reanchor
       buffer #'ghostel-cursor-point
       ;; semi-char/char에서만. copy/emacs/line 모드는 커서≠조합 위치라 제외.
       #'ghostel--buffer-editable-p))
    (advice-add (if (fboundp 'ghostel--redraw-now)
                    'ghostel--redraw-now
                  'ghostel--delayed-redraw)
                :after #'my/ns-preedit--ghostel-reanchor)))

;;; --- (2차) 조합 중 Emacs 커서(녹색 박스)를 조합 글자 위에 고정 ---
;;; Claude Code(2.1.84+)는 IME 인라인 조합을 위해 하드웨어 커서를 입력 캐럿에
;;; 보이게 유지한다. preedit after-string엔 'cursor 속성이 없어서, 커서 오른쪽에
;;; 버퍼 글자가 있으면(Claude placeholder 등) Emacs가 박스를 preedit "다음" 글자
;;; 위에 그린다(xdisp exact-match 규칙) → 조합 글자와 박스가 겹쳐 보임.
;;; 해법: preedit 첫 글자에 정수 'cursor 1 — exact-match 규칙을 override해서
;;; 박스를 조합 글자 위로 가져온다(일반 버퍼의 한글 조합과 같은 모양).
;;; 주의: 아래 "폭 보존" 블록보다 *먼저* 둘 것 — 같은 함수의 :after advice는
;;; 추가한 순서대로 실행되므로, 'cursor가 붙은 문자열을 cover가 가져가야 한다.
(when (featurep 'ns)
  (defun my/ns-preedit--cursor-on-preedit (&rest _)
    "vterm/ghostel에서 preedit 첫 글자에 'cursor 1을 부여."
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay)
                 (derived-mode-p '(vterm-mode ghostel-mode)))
        (let ((str (overlay-get ns-working-overlay 'after-string)))
          (when (and (stringp str) (> (length str) 0))
            (put-text-property 0 1 'cursor 1 str))))))
  (with-eval-after-load 'term/ns-win
    (advice-add 'ns-insert-working-text :after
                #'my/ns-preedit--cursor-on-preedit)))

;;; --- (2차) Claude Code 등 TUI 안 한글 preedit 폭 밀림(출렁임) 수정 ---
;;; 원인: NS preedit는 zero-width 오버레이의 'after-string이라 그 줄에 표시폭
;;; 2칸을 "삽입"한다. zsh 프롬프트에선 커서 오른쪽이 비어 있어 무해하지만,
;;; Claude Code 입력줄에선 커서 오른쪽에 placeholder("Try ...")나 뒷글자가
;;; 있어서 음절을 조합할 때마다 그 내용이 2칸씩 밀렸다 당겨졌다 한다.
;;; 해법: vterm/ghostel 버퍼에서는 after-string 대신 커서 오른쪽 버퍼 셀을
;;; preedit 표시폭만큼 오버레이 'display로 "덮어서" 그린다(폭 보존 →
;;; 레이아웃이 전혀 안 움직임. 네이티브 IME가 화면 위에 덧그리는 방식과 동일).
;;; 줄 끝이라 덮을 셀이 없으면 기존 after-string 폴백 — zsh 프롬프트(EOL)는
;;; 기존과 동일. 줄 "중간" 편집은 동작이 바뀐다: 뒷글자가 밀리는 대신 조합 중
;;; 잠깐 가려졌다가 커밋 시 돌아온다.
;;; 합성: 위의 my/ns-preedit--reanchor가 redraw 후 오버레이를 zero-width로
;;; 재고정하므로, 그 함수에 :after를 걸어 덮는 범위를 매번 재계산한다.
(when (featurep 'ns)
  (defun my/ns-preedit--coverable-p ()
    "현재 버퍼가 폭 보존 덮기를 해도 되는 상태인가(키 입력이 pty로 가는 모드)."
    (or (and (derived-mode-p 'vterm-mode)
             (not (bound-and-true-p vterm-copy-mode)))
        (and (derived-mode-p 'ghostel-mode)
             (fboundp 'ghostel--buffer-editable-p)
             (ghostel--buffer-editable-p))))

  (defun my/ns-preedit--cover (ov)
    "ns-working-overlay OV를 폭 보존형으로 (재)설정한다.
overlay-start(=터미널 커서)부터 preedit 표시폭 이상이 될 때까지 같은 줄의
버퍼 문자를 덮고 'display로 preedit를 그린다. 덮은 폭이 남으면 공백 패딩.
덮을 문자가 없으면(줄 끝) after-string으로 폴백한다."
    (let ((str (overlay-get ov 'my/ns-preedit-str)))
      (when (and (stringp str) (overlay-buffer ov))
        (with-current-buffer (overlay-buffer ov)
          (let* ((beg (overlay-start ov))
                 (need (string-width str))
                 (eol (save-excursion (goto-char beg) (line-end-position)))
                 (end beg) (got 0))
            (while (and (< end eol) (< got need))
              (setq got (+ got (char-width (char-after end)))
                    end (1+ end)))
            (cond
             ((= end beg)            ; 줄 끝: 덮을 셀 없음 → 기존 방식 폴백
              (overlay-put ov 'display nil)
              (overlay-put ov 'after-string str))
             (t
              (overlay-put ov 'after-string nil)
              (move-overlay ov beg end)
              (overlay-put ov 'display
                           (if (> got need) ; 마지막 덮은 글자가 wide면 패딩
                               (concat str (propertize
                                            (make-string (- got need) ?\s)
                                            'face 'ns-working-text-face))
                             str)))))))))

  (defun my/ns-preedit--maybe-cover (&rest _)
    "조합 시작/갱신 직후(ns-insert-working-text 후): 터미널 버퍼면 전환."
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay)
                 (my/ns-preedit--coverable-p))
        (let ((str (overlay-get ns-working-overlay 'after-string)))
          (when (stringp str)
            (overlay-put ns-working-overlay 'my/ns-preedit-str str)))
        (my/ns-preedit--cover ns-working-overlay))))
  (with-eval-after-load 'term/ns-win
    (advice-add 'ns-insert-working-text :after #'my/ns-preedit--maybe-cover))

  ;; reanchor(터미널 redraw 후 재고정)가 zero-width로 만든 직후 다시 덮는다.
  ;; my/ns-preedit--reanchor 자체에 :after — vterm/ghostel/리사이즈 경로
  ;; 전부 자동으로 따라가고, 기존 코드는 한 줄도 수정하지 않는다.
  ;; (advice-add는 대상이 아직 미정의여도 합법 — 정의 시점에 활성화)
  (defun my/ns-preedit--recover-after-reanchor (buffer &rest _)
    (ignore-errors
      (when (and (boundp 'ns-working-overlay)
                 (overlayp ns-working-overlay)
                 (buffer-live-p buffer)
                 (eq (overlay-buffer ns-working-overlay) buffer))
        (with-current-buffer buffer
          (when (my/ns-preedit--coverable-p)
            (my/ns-preedit--cover ns-working-overlay))))))
  (advice-add 'my/ns-preedit--reanchor :after
              #'my/ns-preedit--recover-after-reanchor))

;;; --- (3차) ghostel: Ink TUI(Claude Code 등)에서 끝 공백이 "숨고" 커서가 멈추는 문제 보정 ---
;;; 원인: Ink는 입력줄을 끝 공백을 잘라낸(trim) 채로 그리고, 캐럿은 하드웨어
;;; 커서 이동(CUF/CUB)으로만 알린다. ghostel(<=0.26.0)은 trim된 빈 셀 위의
;;; 커서를 줄 끝(eol)으로 클램프한다(Renderer.zig positionCursorByCell의 @min).
;;; 해법: redraw 직후, 터미널 raw 커서 칸(ghostel--cursor-pos의 COL)이
;;; 버퍼 줄 폭보다 크면 그 차이만큼 실제 공백을 eol에 채워서 point와
;;; ghostel--cursor-char-pos가 진짜 캐럿 칸에 앉게 한다.
;;; (vterm은 C의 goto_col이 같은 방식으로 이미 패딩하므로 이 버그가 없음.
;;;  ghostel을 PR #346 포함 버전(>=0.34.0)으로 올리면 deficit이 항상 0이라
;;;  이 advice는 자동으로 no-op — 보험으로 남겨 둠.)
(with-eval-after-load 'ghostel
  (defun my/ghostel--pad-hidden-tail (buffer)
    "redraw 후 BUFFER에서 trim된 끝 빈 칸을 실제 공백으로 채워 커서를 복원."
    ;; process-filter 꼬리에서 실행 — 어떤 에러도 출력 처리를 끊으면 안 됨.
    (ignore-errors
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'ghostel-mode)
                     (fboundp 'ghostel--buffer-editable-p)
                     (ghostel--buffer-editable-p) ; copy/emacs/line 모드 제외
                     (consp ghostel--cursor-pos)
                     (integerp ghostel--cursor-char-pos))
            (let* ((cx (car ghostel--cursor-pos)) ; 터미널 raw 커서 칸(0-based)
                   (cpos ghostel--cursor-char-pos)
                   (deficit
                    (save-excursion
                      (goto-char cpos)
                      ;; 클램프는 eol에서만 발생. 줄 중간이면 손대지 않는다.
                      (if (eolp)
                          (- cx (string-width
                                 (buffer-substring-no-properties
                                  (line-beginning-position) (point))))
                        0))))
              (when (> deficit 0)
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t)
                      (at-cursor (= (point) cpos))
                      (wins (seq-filter
                             (lambda (w) (= (window-point w) cpos))
                             (get-buffer-window-list buffer nil t))))
                  (save-excursion
                    (goto-char cpos)
                    (insert (make-string deficit ?\s))
                    (setq ghostel--cursor-char-pos (point)))
                  ;; redraw가 point를 커서에 두었던 경우에만 따라가 준다
                  ;; (preedit anchor/emacs-mode 등 예외 위치는 건드리지 않음).
                  (when at-cursor (goto-char ghostel--cursor-char-pos))
                  (dolist (w wins)
                    (set-window-point w ghostel--cursor-char-pos))))))))))
  ;; depth -100: 같은 함수의 :after 중 가장 먼저 실행 →
  ;; 뒤따르는 my/ns-preedit--ghostel-reanchor가 보정된
  ;; ghostel-cursor-point(=ghostel--cursor-char-pos)를 보게 한다.
  (advice-add (if (fboundp 'ghostel--redraw-now)
                  'ghostel--redraw-now
                'ghostel--delayed-redraw)
              :after #'my/ghostel--pad-hidden-tail
              '((depth . -100))))

;; cuda-mode
(use-package cuda-mode)

;; Astyle in C/C++ modes: trigger only on TAB when region is active.
;; Plain TAB/RET keep normal mode behavior.
(use-package astyle
  :ensure t
  :when (executable-find "astyle")
  :init
  (setq astyle-default-rc-name ".astylerc"
        astyle-custom-args '("--quiet"))

  :preface
  (defun my/ts-ancestor-of-type (node types)
    (while (and node (not (member (treesit-node-type node) types)))
      (setq node (treesit-parent node)))
    node)

  (defun my/astyle-context-bounds ()
    "Return (BEG . END) of best context: class/struct > namespace > function."
    (condition-case nil
        (when (and (fboundp 'treesit-node-at) (treesit-available-p))
          (let* ((node (treesit-node-at (point)))
                 (class-like (my/ts-ancestor-of-type node '("class_specifier" "struct_specifier")))
                 (ns-like (and (not class-like)
                               (my/ts-ancestor-of-type node '("namespace_definition"))))
                 (func-like (and (not class-like) (not ns-like)
                                 (my/ts-ancestor-of-type node '("function_definition"))))
                 (target (or class-like ns-like func-like)))
            (when target
              (cons (treesit-node-start target)
                    (treesit-node-end target)))))
      (error nil)))

  (defun my/astyle-defun-bounds ()
    "Return (BEG . END) of current defun, or nil."
    (save-excursion
      (condition-case nil
          (progn
            (end-of-defun)
            (let ((end (point)))
              (beginning-of-defun)
              (let ((beg (point)))
                (when (< beg end)
                  (cons beg end)))))
        (error nil))))

  (defun my/astyle-brace-block-bounds ()
    "Return (BEG . END) of nearest enclosing {...} block, or nil."
    (save-excursion
      (condition-case nil
          (progn
            (unless (eq (char-after) ?{)
              (backward-up-list 1))
            (when (eq (char-after) ?{)
              (let ((beg (point)))
                (forward-list 1)
                (let ((end (point)))
                  (when (< beg end)
                    (cons beg end))))))
        (error nil))))

  (defun my/astyle-local-bounds ()
    "Pick a safe local formatting context around point."
    (or (my/astyle-context-bounds)
        (my/astyle-defun-bounds)
        (my/astyle-brace-block-bounds)))

  (defun my/astyle-bounds-at (pos)
    "Return local astyle bounds at POS, or nil."
    (save-excursion
      (goto-char pos)
      (my/astyle-local-bounds)))

  (defun my/astyle-valid-bounds-p (bds)
    "Return non-nil when BDS is a valid (BEG . END) range."
    (and bds (< (car bds) (cdr bds))))

  (defun my/astyle-bounds-enclose-region-p (bds beg end)
    "Return non-nil when BDS fully contains BEG..END."
    (and (my/astyle-valid-bounds-p bds)
         (<= (car bds) beg)
         (<= end (cdr bds))))

  (defun my/astyle-region-context-bounds (beg end)
    "Pick local bounds for formatting BEG..END with indentation context."
    (let* ((rbeg (min beg end))
           (rend (max beg end))
           (end-pos (if (> rend rbeg) (1- rend) rend))
           (from-beg (my/astyle-bounds-at rbeg))
           (from-end (my/astyle-bounds-at end-pos))
           (from-point (my/astyle-local-bounds)))
      (or (and (my/astyle-bounds-enclose-region-p from-beg rbeg rend) from-beg)
          (and (my/astyle-bounds-enclose-region-p from-end rbeg rend) from-end)
          (and (my/astyle-bounds-enclose-region-p from-point rbeg rend) from-point)
          from-beg
          from-end
          from-point)))

  (defun my/astyle-format-region-context (beg end)
    "Format BEG..END using enclosing context so indentation is preserved."
    (let ((bds (ignore-errors (my/astyle-region-context-bounds beg end))))
      (if (my/astyle-valid-bounds-p bds)
          (progn
            (save-excursion
              (astyle-region (car bds) (cdr bds)))
            t)
        nil)))

  (defun my/astyle-tab (arg)
    "TAB: format selected region with astyle context; else default TAB."
    (interactive "P")
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (unless (my/astyle-format-region-context beg end)
            (astyle-region beg end)))
      (indent-for-tab-command arg)))

  (defun my/astyle-keys-setup ()
    (local-set-key (kbd "<tab>") #'my/astyle-tab))

  :hook ((c-mode      . my/astyle-keys-setup)
         (c++-mode    . my/astyle-keys-setup)
         (c-ts-mode   . my/astyle-keys-setup)
         (c++-ts-mode . my/astyle-keys-setup)))

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
(defun my-large-file-warning-threshold ()
  "Set `large-file-warning-threshold' to nil for local files, default for remote."
  (when buffer-file-name
    (setq-local large-file-warning-threshold
                (if (file-remote-p buffer-file-name)
                    10000000   ;; threshold in bytes
                  nil))))

(add-hook 'find-file-hook #'my-large-file-warning-threshold)

;; ellama
(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "codellama" :embedding-model "codellama")))

;; gptel
(defun my-ollama-model-list ()
  "Return a list of available Ollama model names as strings."
  (let* ((output (shell-command-to-string "ollama list"))
         (lines (split-string output "\n" t))
         (data-lines (cdr lines))) ;; Skip the header line
    (mapcar (lambda (line)
              (car (split-string line))) ;; Take the first column (model name)
            data-lines)))

(use-package gptel
  :ensure t
  :config
  ;; Set your preferred model here
  (setq gptel-model "qwen3:14b-q4_K_M")
  ;; Register and set Ollama as the default backend
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models (my-ollama-model-list)))
  (setq gptel-default-mode 'org-mode))

(defun excluded-file-p (file patterns)
  "Return non-nil if FILE matches any regex in PATTERNS."
  (seq-some (lambda (pattern)
              (string-match-p pattern file))
            patterns))

(defun gptel-context-add-directory-recursively-with-filter (root-dir filter-exclude)
  ;; TODO: Write a function description
  (let* ((all-files (directory-files-recursively root-dir ""))
         (filtered-files
          (seq-remove (lambda (file)
                        (excluded-file-p file filter-exclude))
                      all-files)))
    (dolist (file filtered-files)
      (gptel-add-file file))))

(defun process-files-with-regex-filters (directory &optional include-patterns exclude-patterns processor-fn)
  "Process files in DIRECTORY that match INCLUDE-PATTERNS and don't match EXCLUDE-PATTERNS.
INCLUDE-PATTERNS is a list of regex patterns to include files (all files if nil).
EXCLUDE-PATTERNS is a list of regex patterns to exclude files (none excluded if nil).
PROCESSOR-FN is a function that takes a file path as argument (defaults to `gptel-add-file')."
  (let* ((processor (or processor-fn #'gptel-add-file))
         (all-files (directory-files-recursively directory ""))
         (filtered-files
          (seq-filter
           (lambda (file)
             (and
              ;; Include file if no include patterns or if it matches any include pattern
              (or (null include-patterns)
                  (seq-some (lambda (pattern) (string-match-p pattern file)) include-patterns))
              ;; Include file if no exclude patterns or if it doesn't match any exclude pattern
              (or (null exclude-patterns)
                  (not (seq-some (lambda (pattern) (string-match-p pattern file)) exclude-patterns)))))
           all-files)))
    (dolist (file filtered-files)
      (funcall processor file))
    filtered-files))

;; kill-current-buffer
;; 주의: C-S-k는 영문(ABC) 모드에서만 동작. 한글 IME에서는 NS 포트가
;; Ctrl+Shift+k의 shift를 소실시켜 Ctrl+k와 동일한 이벤트(C-ㅏ)가 됨
;; (실측 확인, elisp 복구 불가) → 한/영 공통으로 동작하는 s-k(Option+k) 병행.
(bind-key* (kbd "C-S-k") 'kill-current-buffer)  ; 영문 모드 전용
(bind-key* (kbd "s-k") 'kill-current-buffer)    ; 한/영 공통 (s-ㅏ→s-k 번역으로 생존)

;; helpful
(use-package helpful
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-h x"   . helpful-command)
         ("C-h F"   . helpful-function)
         ("C-c C-d" . helpful-at-point)))

;; command-log-mode
(use-package command-log-mode
  :config
  (global-command-log-mode)
  (add-hook 'emacs-startup-hook #'clm/open-command-log-buffer)
  ;; Hide the *command-log* window shortly after it's shown
  (run-with-timer
   0.1 nil
   (lambda ()
     (let ((win (get-buffer-window " *command-log*")))
       (when (window-live-p win)
         (delete-window win))))))

;; indent-bars
(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition
                                      for_statement if_statement with_statement
                                      while_statement)))
  :hook ((python-mode python-base-mode) . indent-bars-mode))


;;; init.el ends here
