(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -shell-escape")
 '(TeX-PDF-mode t)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c-mode . "k&r")
     (c++-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(custom-raised-buttons t)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(default-frame-alist (quote ((menu-bar-lines . 1))))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(ecb-compile-window-temporally-enlarge (quote after-display))
 '(ecb-layout-name "left-dir-plus-speedbar")
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(frame-background-mode nil)
 '(gdb-many-windows t)
 '(global-auto-revert-mode t)
 '(global-semantic-decoration-mode t)
 '(global-semantic-idle-local-symbol-highlight-mode t nil (semantic/idle))
 '(global-semantic-idle-scheduler-mode t)
 '(global-semantic-idle-summary-mode t)
 '(global-semantic-stickyfunc-mode t)
 '(global-semanticdb-minor-mode t)
 '(gud-gdb-command-name "gdb --annotate=1 -i=mi ~/Development/cs/muon/csfp")
 '(image-dired-display-window-height-correction 0)
 '(image-dired-display-window-width-correction 1)
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold 10000000)
 '(make-backup-files nil)
 '(php-completion-file "~/.emacs.d/php-completion-file")
 '(recentf-mode t)
 '(safe-local-variable-values (quote ((dired-omit-mode . t))))
 '(scroll-bar-mode nil)
 '(semantic-c-obey-conditional-section-parsing-flag t)
 '(semantic-default-c-path (quote ("~/Development/cs/muon")))
 '(semantic-imenu-bucketize-file nil)
 '(semantic-mode t)
 '(semanticdb-project-roots (quote ("~/Development/cs/muon")))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undofiles"))))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(shm-current-face ((t (:background "#525252"))) t))

