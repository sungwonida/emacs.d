; ---------------------------------------------------------------------------------------------------
; Author: David Jung (2014)
;
; This is initial version of .emacs file written by David Jung.
; ---------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------
; Default Settings
; ---------------------------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(c-basic-offset 4)
 '(c-default-style "k&r")
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(default-frame-alist (quote ((menu-bar-lines . 1))))
 '(diary-file "~/diary/diary")
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
 '(package-user-dir "~/.emacs.d/packages")
 '(php-completion-file "~/.emacs.d/elisp-swjung/php-completion-file")
 '(recentf-mode t)
 '(safe-local-variable-values (quote ((dired-omit-mode . t))))
 '(scroll-bar-mode nil)
 '(semantic-c-obey-conditional-section-parsing-flag t)
 '(semantic-default-c-path (quote ("~/Development/cs")))
 '(semantic-imenu-bucketize-file nil)
 '(semantic-mode t)
 '(semanticdb-project-roots (quote ("~/Development/cs")))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(add-to-list 'default-frame-alist '(font . "NanumGothicCoding-10"))
(add-to-list 'default-frame-alist '(line-spacing . 2))
(set-fontset-font "fontset-default" '(#x1100 . #xffdc) '("나눔고딕코딩" . "unicode-bmp"))
(set-language-environment '"Korean")
(prefer-coding-system 'utf-8)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq default-frame-alist
      '(
        (width . 200) ; character
        (height . 100) ; lines
        ))
;(set-face-foreground 'font-lock-comment-face "red")
(setq font-lock-comment-face 'italic)
(set-face-foreground 'italic "gray50")

(add-to-list 'load-path "~/.emacs.d/packages")
(let ((default-directory "~/.emacs.d/packages"))
      (normal-top-level-add-subdirs-to-load-path))

(unless (eq window-system nil) 
;  (require 'color-theme-solarized-autoloads)
;  (load-theme 'solarized t)
;  (load-theme 'deeper-blue)
;  (disable-theme 'deeper-blue)
  )

;(add-to-list 'default-frame-alist '(background-color . "#10428b"))

(global-auto-revert-mode t) ; buffer auto-refreshing
(setq default-tab-width 4) ; tab-width
;(global-linum-mode 1) ; show line-numbers
(setq scroll-step 1) ; scroll line by line
;(setq c-default-style "linux" 
;	  c-basic-offset 4) ; for indenting..

;(setq exec-path (cons "/usr/local/bin" exec-path))

; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

; Dont show the GNU splash screen
(setq inhibit-startup-message t)

; bind (C-x C-l) to downcase-region
(put 'downcase-region 'disabled nil)

; ---------------------------------------------------------------------------------------------------
; Org Mode
; ---------------------------------------------------------------------------------------------------
(require 'org-depend)

(org-babel-do-load-languages 
 'org-babel-load-languages 
 '((python . t) 
   (C . t)
   (sh . t)))

;(define-key global-map "\M-\C-g" 'org-plot/gnuplot)
(local-set-key (kbd "M-C-g") 'org-plot/gnuplot)

; ---------------------------------------------------------------------------------------------------
; Keep Frame Setting
; ---------------------------------------------------------------------------------------------------
(require 'workgroups)
(require 'bookmark+)

; ---------------------------------------------------------------------------------------------------
; Gnuplot
; ---------------------------------------------------------------------------------------------------
(require 'gnuplot)
(define-key global-map [(control x) (p)] 'gnuplot-help-fn)

; ---------------------------------------------------------------------------------------------------
; Web Browser
; ---------------------------------------------------------------------------------------------------
;(require 'w3m-load)

; ---------------------------------------------------------------------------------------------------
; Naver Dictionary
; ---------------------------------------------------------------------------------------------------
(defun jm-ndic (word)
  "search WORD in endic.naver.com"
  (interactive
   (list (let* ((wd (current-word))
		(word (read-string
		       (format "Dict what (default `%s'): " wd))))
	   (if (string= "" word) wd word))))
;  (browse-url (concat "http://dic.naver.com/search.naver?query=" word)
  (browse-url (concat "http://endic.naver.com/popManager.nhn?sLn=kr&m=search&searchOption=&query=" word)
))

(define-key global-map [(control x) (j)] 'jm-ndic)
(define-key global-map [(control x) (c)] 'calc)

; ---------------------------------------------------------------------------------------------------
; ibuffer
; ---------------------------------------------------------------------------------------------------
(setq ibuffer-saved-filter-groups
	  '(("default"
		 ("Dired" (mode . dired-mode))
		 ("TODO" (filename . "todo.org"))
		 ("Notes" (or
				   (mode . org-mode)
				   (filename . ".org")))
		 ("Development (Fermion / Muon)" (filename . "Development/cs/"))
		 ("Coding stuffs" (or
						   (mode . c-mode)
						   (mode . c++-mode)
						   (mode . python-mode)
						   (mode . shell-script-mode)
						   (mode . sh-mode)
						   ))
		 ("Emacs" (or
				   (filename . ".emacs")
				   (name . "^\\*scratch\\*$")
				   (name . "^\\*Messages\\*$")
				   (name . "^\\*eshell\\*$")
				   (mode . Custom-mode)))
		 ("Help" (or
				  (mode . Man-mode)
				  (mode . Info-mode)
				  (mode . Help-mode)
				  (mode . help-mode)
				  (name . "^\\*Help*\\*$"))))))

(setq ibuffer-expert t)

(add-hook 'ibuffer-mode-hook
		  (lambda ()
			(ibuffer-auto-mode 1)
			(ibuffer-switch-to-saved-filter-groups "default")))

; revert all selected buffers
; To do this, originally, M-x ibuffer RET / f . RET m (on [default])
; After then, V y RET (you have to type this manually)
(eval-after-load "ibuffer"
  '(define-key ibuffer-mode-map (kbd "* f") 'ibuffer-mark-by-file-name-regexp))

;(define-key global-map (kbd "\C-x \C-b") 'ibuffer)
(define-key global-map (kbd "\C-x \C-b") 'helm-buffers-list)

; ---------------------------------------------------------------------------------------------------
; Development - Muon
; ---------------------------------------------------------------------------------------------------
(defun build-muon ()
  "build-muon"
  (interactive)
  (compile "cd /home/swjung/Development/cs/muon/ && ./build_csfp.sh --enable_encrypt")
)

(defun build-muon-debug ()
  "build-muon-debug"
  (interactive)
  (compile "cd /home/swjung/Development/cs/muon/ && ./build_csfp.sh --enable_encrypt --debug")
)

(defun build-muon-image-stitcher ()
  "build-muon-image-stitcher"
  (interactive)
  (compile "cd /home/swjung/Development/cs/muon/ && ./build_csfp.sh --enable_encrypt --build_image_stitcher")
)

(defun build-muon-image-stitcher-debug ()
  "build-muon-image-stitcher-debug"
  (interactive)
  (compile "cd /home/swjung/Development/cs/muon/ && ./build_csfp.sh --enable_encrypt --build_image_stitcher --debug")
)

(defun build-muon-android ()
  "build-muon-android"
  (interactive)
;  (compile "cd /home/swjung/Development/cs/muon/ && ./build_csfp.sh --spec android_aarch64-linux-android-4.9_m64_arm64-v8a.spec --library_type shared --enable_encrypt")
(compile "cd /home/swjung/Development/cs/muon/ && ./build_csfp.sh --spec android_arm-linux-androideabi-4.6_m32_armeabi-v7a.spec --library_type shared --enable_encrypt")
)

(defun cp-libs-for-android ()
  "cp-libs-for-android"
  (interactive)
  (start-process-shell-command "cp-libs-for-android" "*cp-libs-for-android*" "/home/swjung/Development/cs/muon/localutil/cp_libs_for_android.sh")
)

(defun build-csfp-gui ()
  "build-csfp-gui"
  (interactive)
  (start-process-shell-command "cp-libs-for-android" "*build-csfp-gui*" "cd /home/swjung/Development/cs/csfp_gui/gui && ./make_gui.sh --ix11094a --muon --cp --build --install")
  (message "csfp-gui build done.")
)

(defun clean-muon ()
  "Clean muon for the PC."
  (interactive)
  (message "Clearing muon compilation environment (PC)..")
  (start-process-shell-command "clean-muon" "*clean build*" "cd /home/swjung/Development/cs/muon && ./build_csfp.sh dist-clean")
  (message "Clean done.")
)

(defun run-muon (mode train query)
  "Run muon with interative argument"
  (interactive
   (list (let* ((dm "ipmerge")
				(mode (read-string 
					   (format "mode (default `%s'): " dm))))
		   (if (string= "" mode) dm mode))
		 (let* ((dt "~/Development/cs/muon/img/l0_00.bmp")
				(train (read-string 
						(format "train path (default `%s'): " dt))))
		   (if (string= "" train) dt train))
		 (let* ((dq "~/Development/cs/muon/img/l0_01.bmp")
				(query (read-string 
						(format "query path (default `%s'): " dq))))
		   (if (string= "" query) dq query))
		 )
;		 (if (string= "fenrol" mode)
;			 (let* ((dq1 "~/Development/cs/muon/img/l0_02.bmp")
;					(query1 (read-string 
;							(format "another query path (default `%s'): " dq1))))
;			   (if (string= "" query1) dq1 query1)))
;		 ))
 ; (if (= mode (string fenrol)) 
;	  (async-shell-command (concat "cd ~/Development/cs/muon/ && ./csfp" " -m " mode " " train " " query " " query1) "muon-execution"))
   )
  (async-shell-command (concat "cd ~/Development/cs/muon/ && ./csfp" " -m " mode " " train " " query) "muon-execution")
  )

(defun build-muon-board ()
  "Build muon for the board."
  (interactive)
  (message "Building muon (board)..")
  (compile "cd /home/swjung/Development/cs/muon/stm32f4 && make")
)

(defun clean-muon-board ()
  "Clean muon for the board."
  (interactive)
  (message "Clearing muon compilation environment (board)..")
  (start-process-shell-command "clean-muon" "*clean build*" "cd /home/swjung/Development/cs/muon/stm32f4 && make clean")
  (message "Clean done.")
)

(defun flash-muon ()
  "Flash muon into the board."
  (interactive)
  (message "Flashing muon.bin to connected STM32 board..")
  (async-shell-command "st-flash write /home/swjung/Development/cs/muon/stm32f4/muon.bin 0x8000000" "flash_m4"))

(defun start-st-link-server ()
  "Start ST-Link server."
  (interactive)
  (message "Starting ST-Link server..")
  (async-shell-command "st-util" "debug-server"))

(defun debug-muon-board ()
  "Debug muon for the board using GDB."
  (interactive)
  (async-shell-command "arm-none-eabi-gdb /home/swjung/Development/cs/muon/stm32f4/muon.elf" "gdb-muon")
)

;(define-key global-map [(f10)] 'run-muon)
(define-key global-map [(f11)] 'clean-muon)
(define-key global-map [(f12)] 'build-muon)
(define-key global-map [(f9)] 'build-muon-debug)
(define-key global-map [(f7)] 'build-muon-image-stitcher)
(define-key global-map [(f8)] 'build-muon-image-stitcher-debug)
(define-key global-map [(f4)] 'build-muon-android)
(define-key global-map [(f5)] 'cp-libs-for-android)
(define-key global-map [(f6)] 'build-csfp-gui)

;(define-key global-map [(f4)] 'flash-muon)
;(define-key global-map [(f5)] 'debug-muon-board)
;(define-key global-map [(f6)] 'clean-muon-board)
;(define-key global-map [(f7)] 'build-muon-board)
;(define-key global-map [(f8)] 'start-st-link-server)

; ---------------------------------------------------------------------------------------------------
; Using packages
; ---------------------------------------------------------------------------------------------------
; start package.el with emacs
(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
; initialize package.el
(package-initialize)

; ---------------------------------------------------------------------------------------------------
; Building for C/C++ IDE
; ---------------------------------------------------------------------------------------------------
; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
; start yasnippet with emacs
;(require 'yasnippet)
;(yas-global-mode 1)
;(require 'prepaint)
;(prepaint-global-mode 1)
(add-hook 'after-init-hook 'global-company-mode)
(defun my-company-mode-hook ()
  (define-key company-mode-map [backtab] 'company-complete))
(add-hook 'company-mode-hook 'my-company-mode-hook)

; ---------------------------------------------------------------------------------------------------
; CScope
; ---------------------------------------------------------------------------------------------------
(require 'xcscope) 
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f7)]  'cscope-find-this-text-string)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)

; ---------------------------------------------------------------------------------------------------
; REDO
; ---------------------------------------------------------------------------------------------------
;; REDO (a better way to handle undo and redo)
;; http://www.wonderworks.com/
(require 'redo)
(global-set-key [(control .)] 'redo)

; ---------------------------------------------------------------------------------------------------
; Additional Components
; ---------------------------------------------------------------------------------------------------
;; Kills live buffers, leaves some emacs work buffers
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-some-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (not (string-equal name "*Messages*"))
	  ;; (not (string-equal name "*Buffer List*"))
	  ;; (not (string-equal name "*buffer-selection*"))
	  ;; (not (string-equal name "*Shell Command Output*"))
	   (not (string-equal name "*scratch*"))
	   (/= (aref name 0) ? )
	   (if (buffer-modified-p buffer)
	       (if (yes-or-no-p
		    (format "Buffer %s has been edited. Kill? " name))
		   (kill-buffer buffer))
	     (kill-buffer buffer))))
    (setq list (cdr list))))

;; ;; Kills all them buffers except scratch
;; ;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))

;; insert date into buffer at point
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a %p %l:%M")))

;; TAGS creating
(defvar c-files-regex ".*\\.\\(c\\|cpp\\|h\\|hpp\\)"
  "A regular expression to match any c/c++ related files under a directory")
 
(defun my-semantic-parse-dir (root regex)
  "
   This function is an attempt of mine to force semantic to
   parse all source files under a root directory. Arguments:
   -- root: The full path to the root directory
   -- regex: A regular expression against which to match all files in the directory
  "
  (let (
        ;;make sure that root has a trailing slash and is a dir
        (root (file-name-as-directory root))
        (files (directory-files root t ))
       )
    ;; remove current dir and parent dir from list
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    ;; remove any known version control directories
    (setq files (delete (format "%s.git" root) files))
    (setq files (delete (format "%s.hg" root) files))
    (while files
      (setq file (pop files))
      (if (not(file-accessible-directory-p file))
          ;;if it's a file that matches the regex we seek
          (progn (when (string-match-p regex file)
                   (save-excursion
                     (semanticdb-file-table-object file))
           ))
          ;;else if it's a directory
          (my-semantic-parse-dir file regex)
      )
     )
  )
)
 
(defun my-semantic-parse-current-dir (regex)
  "
   Parses all files under the current directory matching regex
  "
  (my-semantic-parse-dir (file-name-directory(buffer-file-name)) regex)
)
 
(defun create-tags-curdir ()
  "
   Parses all the c/c++ related files under the current directory
   and inputs their data into semantic
  "
  (interactive)
  (my-semantic-parse-current-dir c-files-regex)
)
 
(defun create-tags (dir)
  "Prompts the user for a directory and parses all c/c++ related files
   under the directory
  "
  (interactive (list (read-directory-name "Provide the directory to search in:")))
  (my-semantic-parse-dir (expand-file-name dir) c-files-regex)
)

;; Dired Enhancements ------------------------------------------------------------
 (eval-after-load "dired"
      '(progn
         (defadvice dired-advertised-find-file (around dired-subst-directory activate)
           "Replace current buffer if file is a directory."
           (interactive)
            (let* ((orig (current-buffer))
                   ;; (filename (dired-get-filename))
                   (filename (dired-get-filename t t))
                  (bye-p (file-directory-p filename)))
             ad-do-it
             (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
               (kill-buffer orig))))))

;; (eval-after-load "dired"
;;   ;; don't remove `other-window', the caller expects it to be there
;;   '(defun dired-up-directory (&optional other-window)
;;      "Run Dired on parent directory of current directory."
;;      (interactive "P")
;;      (let* ((dir (dired-current-directory))
;;      	    (orig (current-buffer))
;;      	    (up (file-name-directory (directory-file-name dir))))
;;        (or (dired-goto-file (directory-file-name dir))
;;      	   ;; Only try dired-goto-subdir if buffer has more than one dir.
;;      	   (and (cdr dired-subdir-alist)
;;      		(dired-goto-subdir up))
;;      	   (progn
;;      	     (kill-buffer orig)
;;      	     (dired up)
;;      	     (dired-goto-file dir))))))

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

;; CEDET
(require 'cedet)
(define-key global-map [(meta .)] 'semantic-ia-fast-jump)
(global-set-key [(meta return)] 'semantic-complete-analyze-inline)
(global-ede-mode 1)                      ; Enable the Project management system

(require 'ecb)

;; hs-minor-mode
(add-hook 'c-mode-common-hook
          (lambda()
            (hs-minor-mode t)
            (local-set-key (kbd "C-c u") 'hs-toggle-hiding)
            (local-set-key (kbd "C-c <down>") 'hs-hide-all)
            (local-set-key (kbd "C-c <up>") 'hs-show-all)))

;; Fast switch frame & buffer like quick-silver style
(require 'ido-vertical-mode)
(ido-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)
(ido-vertical-mode t)


;(require 'powerline)
;(powerline-default-theme)

(require 'anything)
(define-key global-map (kbd "M-+") 'anything)

(require 'git)

;(setq-default exec-path (quote ("/home/swjung/tools/jdk1.8.0_05/bin" "/home/swjung/tools/android-sdk-linux/tools" "/home/swjung/tools/android-sdk-linux/platform-tools" "/home/swjung/tools/android-ndk-r9d" "/home/swjung/bin" "/home/swjung/bin" "/usr/lib/lightdm/lightdm" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games")))

;; Shell environment
;(setenv "PATH" (mapconcat 'identity exec-path path-separator))
;(setq-default eshell-path-env (mapconcat 'identity exec-path path-separator))

;; Eassist.el
(require 'eassist)
(defun my-c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-python-mode-hook ()
  (define-key python-mode-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'python-mode-hook 'my-python-mode-hook)
(define-key lisp-mode-shared-map (kbd "M-m") 'eassist-list-methods)

;; Mail client 'Wanderlust'
(autoload 'wl "wl" "Wanderlust" t)

;; IMAP, gmail:
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "sungwonida@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl

      ;;for non ascii-characters in folder-names
      elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "myname"
      wl-smtp-posting-server "smtp.gmail.com"
      wl-local-domain "gmail.com"
      wl-message-id-domain "smtp.gmail.com")

(setq wl-from "David Jung <sungwonida@gmail.com>"

      ;;all system folders (draft, trash, spam, etc) are placed in the
      ;;[Gmail]-folder, except inbox. "%" means it's an IMAP-folder
      wl-default-folder "%inbox"
      wl-draft-folder   "%[Gmail]/Drafts"
      wl-trash-folder   "%[Gmail]/Trash"
      wl-fcc            "%[Gmail]/Sent"

      ;; mark sent messages as read (sent messages get sent back to you and
      ;; placed in the folder specified by wl-fcc)
      wl-fcc-force-as-read    t

      ;;for when auto-compleating foldernames
      wl-default-spec "%")

;; Highlighting #if 0 ~ #endif in c-mode
;; ----------------------------------------------------------------
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
  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)
  )

(add-hook 'c-mode-common-hook 'jpk/c-mode-hook)
;; ----------------------------------------------------------------



;; ----------------------------------------------------------------------------  
;; Easier Transition between Windows  
;; ----------------------------------------------------------------------------  
;  M-up, M-down, M-left, and M-right keys.  
(windmove-default-keybindings 'meta)

;; Spawning Window
(fset 'spawn-window-right
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 51 M-left] 0 "%d")) arg)))
(fset 'spawn-window-left
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 51 M-right] 0 "%d")) arg)))
(fset 'spawn-window-down
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 50 M-down] 0 "%d")) arg)))
(fset 'spawn-window-up
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 24 50 M-up] 0 "%d")) arg)))
(define-key global-map [(control right)]  'spawn-window-left)
(define-key global-map [(control left)]  'spawn-window-right)
(define-key global-map [(control down)]  'spawn-window-down)
(define-key global-map [(control up)]  'spawn-window-up)
(define-key global-map [(super q)] 'kill-this-buffer)
(define-key global-map [(meta q)] 'kill-buffer-and-window)
(define-key global-map [(control tab)] 'switch-to-buffer)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(require 'exec-path-from-shell)
(when window-system (exec-path-from-shell-initialize))

(require 'ggtags)
(add-hook 'prog-mode-hook
          (lambda ()
            (when (derived-mode-p
                   'c-mode
                   'c++-mode
                   'java-mode
                   'asm-mode)
              (progn
                (cscope-minor-mode)
                (ggtags-mode))))) 

(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "C-c M-n") 'ggtags-next-mark)

(defun notes ()
  "Switch to my notes dir."
   (interactive)
   (find-file "~/Notes")
   )

(require 'sr-speedbar)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(define-key global-map (kbd "C-x v s") 'magit-status)

; for image-mode
(defun my-image-mode-hook ()
  (define-key image-mode-map (kbd "w") 'image-transform-fit-to-width)
  (define-key image-mode-map (kbd "h") 'image-transform-fit-to-height)
  (define-key image-mode-map (kbd "s") 'image-transform-set-scale))
(add-hook 'image-mode-hook 'my-image-mode-hook)

; ---------------------------------------------------------------------------------------------------
; Helm
; ---------------------------------------------------------------------------------------------------
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;(helm-mode 1)

(define-key global-map (kbd "C-c h g") 'helm-git-grep-at-point)
(define-key global-map (kbd "C-c h f") 'helm-git-files)

(setq helm-split-window-in-side-p               nil
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

(require 'helm-gtags)
(defun my-helm-gtags-mode-hook ()
  (helm-gtags-mode 1)
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-find-rtag) ;helm-gtags-pop-stack
  (define-key helm-gtags-mode-map (kbd "C-c C-,") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c C-.") 'helm-gtags-next-history)
  )
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-common-hook 'my-helm-gtags-mode-hook)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)

; ---------------------------------------------------------------------------------------------------
; image-dired
; ---------------------------------------------------------------------------------------------------
(eval-after-load 'image-dired+ '(image-diredx-async-mode 1))
(eval-after-load 'image-dired+ '(image-diredx-adjust-mode 1))
(setq image-dired-track-movement nil)

; ---------------------------------------------------------------------------------------------------
; Last History
; ---------------------------------------------------------------------------------------------------
;; Run recent-open-files on startup
(define-key global-map [(control x)(control r)]  'recentf-open-files)
(recentf-open-files)


(put 'erase-buffer 'disabled nil)
