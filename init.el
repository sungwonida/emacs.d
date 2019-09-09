;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(setq custom-file "~/.emacs.d/local/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/local/packages")
(let ((default-directory "~/.emacs.d/local/packages"))
  (normal-top-level-add-subdirs-to-load-path))
(setq default-directory "~/")


;; helm
(helm-mode 1)
(bind-key* "M-x" 'helm-M-x)
(bind-key* "C-x C-f" 'helm-find-files)
(bind-key* "C-x C-l" 'helm-locate)
(bind-key* "C-x C-b" 'helm-mini) ;;helm-buffers-list
(bind-key* "C-x r b" 'helm-filtered-bookmarks)
(bind-key* "C-c C-r" 'helm-recentf)
(bind-key* "M-y" 'helm-show-kill-ring)
(bind-key* "M-s o" 'helm-occur)
(bind-key* "M-g M-o M-k" 'helm-google-suggest)
(bind-key* "C-c h p" 'helm-browse-project)
(bind-key* "C-c C-h C-p" 'helm-browse-project)
;; (eval-after-load "helm"
;;   '(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)) ;; uncomment to use tab as auto-completion


;; helm-ls-git-ls
(define-key global-map (kbd "C-c h f") 'helm-ls-git-ls)
(define-key global-map (kbd "C-c C-h C-f") 'helm-ls-git-ls)


;; helm-git-grep
(define-key global-map (kbd "C-c h g") 'helm-git-grep-at-point)
(define-key global-map (kbd "C-c C-h M-g") 'helm-git-grep-at-point)


;; helm-gtags
(defun my-helm-mode-hook ()
  (helm-gtags-mode 1)
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-find-rtag) ;helm-gtags-pop-stack
  (define-key helm-gtags-mode-map (kbd "C-c C-,") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c C-.") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c g u") 'helm-gtags-update-tags))
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-common-hook 'my-helm-mode-hook)
;; (add-hook 'c-mode-hook 'my-helm-mode-hook)
;; (add-hook 'c++-mode-hook 'my-helm-mode-hook)
(setq helm-split-window-in-side-p               t
      helm-autoresize-mode                      t
      helm-move-to-line-cycle-in-source         t
      helm-ff-search-library-in-sexp            t
      helm-scroll-amount                        8
      helm-ff-file-name-history-use-recentf     t
      helm-gtags-ignore-case                    t
      helm-gtags-auto-update                    t
      helm-gtags-use-input-at-cursor            t
      helm-gtags-pulse-at-cursor                t
      helm-gtags-prefix-key                     "\C-cg"
      helm-gtags-suggested-key-mapping          t
      )
(setq helm-show-completion-display-function #'helm-show-completion-default-display-function)

(require 'helm-descbinds)
(helm-descbinds-mode)


;; org-mode
(bind-key* "C-c c" 'org-capture)
(bind-key* "C-c l" 'org-store-link)
(bind-key* "C-c a" 'org-agenda)
(bind-key* "C-c b" 'org-switchb)
(setq org-image-actual-width nil)
(setq org-html-inline-image-rules
      '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|bmp\\)\\'")
        ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|bmp\\)\\'")
        ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|bmp\\)\\'")))


;;; My swiss army knife (notes, todos, agenda, etc.)
(setq org-root-path "~/my_Swiss_army_Knife/")
(defun notes ()
  "Switch to my notes dir."
  (interactive)
  (find-file org-root-path))
(defun inbox ()
  "Show my own inbox."
  (interactive)
  (find-file (concat org-root-path "inbox.org")))
(defun voca ()
  "Show my vocabulary tables."
  (interactive)
  (find-file (concat org-root-path "personal/voca.org")))

(require 'find-lisp)
(defun org-agenda-files-load-files ()
  (interactive)
  (if (file-directory-p org-root-path)
      (setq org-agenda-files
            (find-lisp-find-files
             org-root-path
             "\\.org$"))
    nil))
(add-hook 'after-init-hook 'org-agenda-files-load-files)

(setq org-default-notes-file
      (concat org-root-path "inbox.org"))


;; nyan-mode
(nyan-mode)
(setq nyan-wavy-trail t)
(nyan-start-animation)


;; smart-mode-line
(sml/setup)


;; face
;;; Monaco for Linux
;;;; 1) http://www.gringod.com/wp-upload/software/Fonts/Monaco_Linux.ttf
;;;; 2) Copy the downloaded font into ~/.fonts/
;;;; 3) Run sudo fc-cache -fv
;;; Monaco for Windows
;;;; 1) http://www.gringod.com/wp-upload/software/Fonts/Monaco_5.1.ttf
;;;; 2) Copy the downloaded font into %SystemRoot%/Fonts/
(if (display-graphic-p)
    (progn
      (setq default-frame-alist
            '((width . 162)
              (height . 40)))
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (setq scroll-step 1)
      (if (eq system-type 'darwin)
          (add-to-list 'default-frame-alist '(font . "Monaco-10"))
        (add-to-list 'default-frame-alist '(font . "Monaco-10")))
      (if (eq system-type 'windows-nt)
          (add-to-list 'default-frame-alist '(font . "Monaco-9"))
        (add-to-list 'default-frame-alist '(font . "Monaco-9")))))

(set-language-environment '"Korean")
(prefer-coding-system 'utf-8)
(cond ((string-equal system-type "windows-nt") (unicode-fonts-setup)))
(when (not (eq system-type 'cygwin))
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc) '("나눔고딕코딩" . "unicode-bmp")))
(when (eq system-type 'darwin))


;; anzu
(global-anzu-mode +1)


;; javascript
(add-hook 'js-mode-hook
          (lambda ()
            (tern-mode t)))
(tern-ac-setup)


;; octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(defun my-octave-mode-hook ()
  (define-key octave-mode-map (kbd "M-s M-l") 'octave-send-line)
  (define-key octave-mode-map (kbd "M-s M-b") 'octave-send-block)
  (define-key octave-mode-map (kbd "M-s M-f") 'octave-send-defun)
  (define-key octave-mode-map (kbd "M-s M-a") 'octave-send-buffer)
  (define-key octave-mode-map (kbd "M-s M-r") 'octave-send-region))
(add-hook 'octave-mode-hook 'my-octave-mode-hook)


;; plantuml
;;; plantuml.jar can be downloaded from http://plantuml.com/en/download
;;; Download the file and place it to
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))


;; semantic
(define-key global-map (kbd "C-c i j") 'semantic-ia-fast-jump)
(define-key global-map (kbd "C-c i m") 'semantic-ia-complete-symbol-menu)


;; flycheck
;; (bind-key "C-c ! h" 'helm-flycheck flycheck-mode-map)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))


;; company
(require 'company)
(add-hook 'prog-mode-hook (lambda ()
                            (company-mode t)
                            (define-key company-mode-map [backtab] 'company-complete)
                            (define-key company-active-map [tab] 'company-complete-selection)))


;; rtags (Gradually substitute to lsp-mode + ccls)
;; only run this if rtags is installed
(when (and (require 'rtags nil :noerror) (not (eq system-type 'windows-nt)))
  (setq rtags-install-path "~/.emacs.d/")

  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  (setq rtags-use-helm t)

  (add-hook 'c-mode-hook 'rtags-start-process-maybe)
  (add-hook 'c++-mode-hook 'rtags-start-process-maybe)
  (setq rtags-verify-protocol-version nil))


;; lsp-mode + ccls (Use only for Windows right now)
(defun my-lsp-ui-mode-hook ()
  (lsp-ui-sideline-mode 0)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(when (eq system-type 'windows-nt)
  (use-package lsp-mode :commands lsp)
  (use-package lsp-ui :commands lsp-ui-mode :config (my-lsp-ui-mode-hook))
  (use-package company-lsp :commands company-lsp)
  (use-package ccls
    :hook ((c-mode c++-mode objc-mode) .
           (lambda () (require 'ccls) (lsp))))
  (if (eq system-type 'windows-nt)
      (setq ccls-executable
            "d:/Users/dit-698/Development/ccls/Release/Release/ccls.exe")
    (setq ccls-args
          '("--log-file=d:/users/dit-698/tmp/ccls.log"))) ;; may cause crash if the path doesn't exist)
  )


;; magit
(diminish 'magit-auto-revert-mode)
(define-key global-map (kbd "C-c v s") 'magit-status)
(define-key global-map (kbd "C-c C-v C-s") 'magit-status)
(define-key global-map (kbd "C-c v y") 'magit-show-refs-popup)
(define-key global-map (kbd "C-c C-v C-y") 'magit-show-refs-popup)
(define-key global-map (kbd "C-c v l") 'magit-log-head)
(define-key global-map (kbd "C-c C-v C-l") 'magit-log-head)


;; exec-path-from-shell
(when
    (or
     (not (eq system-type 'windows-nt))
     (memq window-system '(mac ns)))
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))


;; files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)


;; ;; clipboard access in X Window
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-selection-value)


;; C-SPC selection in Windows
(cond
 ((string-equal system-type "windows-nt")
  (global-set-key [C-kanji] 'set-mark-command)))


;; startup page
(setq inhibit-startup-message t)


;; Naver Dictionary
(defun jm-ndic (word)
  "search WORD in endic.naver.com"
  (interactive
   (list (let* ((wd (current-word))
                (word (read-string
                       (format "Dict what (default `%s'): " wd))))
           (if (string= "" word) wd word))))
  (browse-url (concat "http://endic.naver.com/popManager.nhn?sLn=kr&m=search&searchOption=&query=" word)))
(define-key global-map [(control x) (j)] 'jm-ndic)


;; Google Translate
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ko")
(defun my-google-translate-query-translate-ko-to-en ()
  (interactive)
  (let ((google-translate-default-source-language "ko")
        (google-translate-default-target-language "en"))
    (google-translate-query-translate)))
(bind-key* "C-M-g" 'google-translate-at-point)
(bind-key* "M-g t" 'google-translate-query-translate)
(bind-key* "M-g M-t" 'google-translate-query-translate)
(bind-key* "M-g k t" 'my-google-translate-query-translate-ko-to-en)
(bind-key* "M-g M-k M-t" 'my-google-translate-query-translate-ko-to-en)


;; quick insert-date
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %p %l:%M")))


;; Easier Transition between Windows
;;; M-up, M-down, M-left, and M-right keys.
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


;; Development
;;; Font
(setq font-lock-comment-face 'italic)
(set-face-foreground 'italic "gray50")
(set-face-attribute 'region nil :background "#aaff90")
(add-hook 'prog-mode-hook '(lambda () (setq tab-width 4)))

(require 'eassist)
;; (global-ede-mode 1)


;;; C
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(defun my-c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'helm-semantic-or-imenu))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook
          (lambda()
            (hs-minor-mode t)
            (local-set-key (kbd "C-c u") 'hs-toggle-hiding)
            (local-set-key (kbd "C-c <down>") 'hs-hide-all)
            (local-set-key (kbd "C-c <up>") 'hs-show-all)))

(defun cpp-highlight-if-0/1 ()
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

(defun jpk/c-mode-hook ()
  (cpp-highlight-if-0/1)
  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local))

(add-hook 'c-mode-common-hook
          (lambda () (add-to-list 'local-write-file-hooks 'delete-trailing-whitespace 'jpk/c-mode-hook)))
(add-hook 'c-mode-hook
          (lambda () (add-to-list 'local-write-file-hooks 'delete-trailing-whitespace 'jpk/c-mode-hook)))
(add-hook 'c++-mode-hook
          (lambda () (add-to-list 'local-write-file-hooks 'delete-trailing-whitespace 'jpk/c-mode-hook)))
(add-hook 'c-or-c++-mode-hook
          (lambda () (add-to-list 'local-write-file-hooks 'delete-trailing-whitespace 'jpk/c-mode-hook)))


;; ;; Another version of if-0/1 highlighting
;; (defun my/cc-mode/highlight-if-0 ()
;;   "highlight c/c++ #if 0 #endif macros"
;;   (setq cpp-known-face 'default)
;;   (setq cpp-unknown-face 'default)
;;   (setq cpp-known-writable 't)
;;   (setq cpp-unknown-writable 't)
;;   (setq cpp-edit-list '(("0" '(foreground-color . "gray")  default both)
;;                         ("1" default font-lock-comment-face both)))
;;   (cpp-highlight-buffer t))

;; (defun my/cc-mode/highlight-if-0-hook ()
;;   (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
;;     (my/cc-mode/highlight-if-0)))
;; ;; (add-hook 'after-save-hook #'my/cc-mode/highlight-if-0-hook)


;;; Python
(when (executable-find "python")
  (setq python-shell-interpreter "python"))
(defun my-python-mode-hook ()
  (define-key python-mode-map (kbd "M-m") 'helm-semantic-or-imenu))
(add-hook 'python-mode-hook 'my-python-mode-hook)
(define-key lisp-mode-shared-map (kbd "M-m") 'helm-semantic-or-imenu)


;;;; elpy
(elpy-enable)
(defun my-elpy-mode-hook ()
  (cl-dolist (key '("C-<right>" "C-<left>" "C-<down>" "C-<up>" "M-<right>" "M-<left>" "M-<down>" "M-<up>"))
    (define-key elpy-mode-map (kbd key) nil))
  (print "applied my-elpy-mode-hook"))
(add-hook 'elpy-mode-hook 'my-elpy-mode-hook)


;;;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:use-shortcuts t)
(setq jedi:complete-on-dot t)


;;;; py-autopep8
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))


;;; smartparens
(diminish 'smartparens-mode "()")
(smartparens-global-mode t)


;; Nuts and Bolts for CB Projects
(setq cb_functions_file (concat org-root-path "business/canvasbio/cb_internal_functions.org"))
(if (file-exists-p cb_functions_file)
    (org-babel-load-file cb_functions_file))


;; Recent files
(recentf-mode)


;; eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))


;; redo
(require 'redo+)
(global-set-key (kbd "C-.") 'redo)


;; tdd
;;; Turn on/off the mode manually because it runs recompile automatically
;;; after saving any buffer no matters it's prog-mode or not.
(require 'tdd)


;; Publishing the live buffer
;;; http://stackoverflow.com/questions/36183071/how-can-i-real-time-preview-markdown-in-emacs]]
;;; 1) M-x httpd-start
;;; 2) M-x impatient-mode
;;; 3) localhost:8080/imp
(require 'simple-httpd)
(require 'impatient-mode)
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))


;; Pandoc
(setq markdown-command "/usr/bin/pandoc")


;; Replace the region with yank buffer
(delete-selection-mode 1)


;; rainbow-mode
(require 'rainbow-mode)


;; god-mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(add-to-list 'god-exempt-major-modes 'dired-mode)
(setq god-exempt-predicates nil)

(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "enabled" "#21abcd"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "enabled" "#21abcd"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "disabled" "#e9e2cb"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "disabled" "#e9e2cb")))))))

(add-hook 'god-mode-enabled-hook 'c/god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'c/god-mode-update-cursor)

(require 'god-mode-isearch)
(defun my-god-mode-hook ()
  (define-key isearch-mode-map (kbd "<tab>") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<tab>") 'god-mode-isearch-disable))
(add-hook 'god-mode-enabled-hook 'my-god-mode-hook)

(define-key god-local-mode-map (kbd "i") 'god-mode-all)
(define-key god-local-mode-map (kbd "S-<escape>") (kbd "C-g"))
(define-key god-local-mode-map (kbd "q") 'quit-window)
(bind-key* "C-x C-r C-b" 'helm-filtered-bookmarks)
(bind-key* "C-x C-r C-m" 'bookmark-set)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(god-mode)


;; conda
(require 'conda)
;;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;;; if you want eshell support, include:
(conda-env-initialize-eshell)
;;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)
;;; If your Anaconda installation is in elsewhere other than the default
(setq conda-anaconda-home (concat (getenv "HOME") "miniconda3"))
;; Remove code below after the testing
;; (cond ((eq system-type 'gnu/linux)
;;        (custom-set-variables
;;         '(conda-anaconda-home (concat (getenv "HOME") "miniconda3")))) ;; need test
;;       ((eq system-type 'darwin)
;;        (custom-set-variables
;;         '(conda-anaconda-home (concat (getenv "HOME") "miniconda3")))) ;; need test
;;       ((eq system-type 'windows-nt)
;;        (custom-set-variables
;;         '(conda-anaconda-home (concat (getenv "HOME") "miniconda3")))
;;         ;; '(conda-env-home-directory (concat (getenv "HOME") "miniconda3"))
;;         ))

;; Docker
(require 'docker)
