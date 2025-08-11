;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Speed up font rendering over the Wayland socket
(setq inhibit-compacting-font-caches t)

;; Avoid giant frames when Windows monitor DPI changes
(setq frame-resize-pixelwise t)

;; Make webkit xwidgets obey dark mode (GTK >= 3.24.30)
(when (featurep 'xwidget-internal)
  (add-hook 'xwidget-webkit-mode-hook
            (lambda ()
              (xwidget-webkit-execute-script
               (xwidget-webkit-current-session)
               "document.documentElement.style.background='#1e1e1e';"))))

;; --- WSL clipboard: robust, synchronous ---
(defun my/wsl-copy (text &optional _push)
  "Copy TEXT to Windows clipboard (no background process)."
  (when (and text (> (length text) 0))
    ;; Keep Emacs/Wayland clipboard for WSLg apps
    (ignore-errors (gui-set-selection 'CLIPBOARD text))
    ;; Send UTF-16LE with CRLF to clip.exe (CF_UNICODETEXT)
    (let ((coding-system-for-write 'utf-16le-dos))
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max)
                             "clip.exe" nil 0 nil)))))

(defun my/wsl-paste ()
  "Read text from the Windows clipboard reliably (UTF-16LE, CRLF->LF)."
  (let ((coding-system-for-read 'utf-16le-dos))  ;; expect UTF-16LE from PS
    (string-trim-right
     (with-output-to-string
       (call-process "powershell.exe" nil standard-output nil
                     "-NoProfile" "-Command"
                     "[Console]::OutputEncoding=[System.Text.Encoding]::Unicode;
                      Get-Clipboard -Raw")))))

(setq interprogram-cut-function   #'my/wsl-copy
      interprogram-paste-function #'my/wsl-paste)

;; Sensible defaults
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t)

;; Default font (size is 1/10 pt; 140 = 14pt)
(set-face-attribute 'default nil :family "Monaco" :height 140)

;; Add CJK/emoji fallbacks (only if installed)
(dolist (pair '((han      . "Noto Sans CJK JP")   ; Kanji
                (kana     . "Noto Sans CJK JP")   ; Hiragana/Katakana
                (cjk-misc . "Noto Sans CJK JP")
                (hangul   . "Noto Sans CJK KR")   ; Korean, if needed
                (symbol   . "Noto Color Emoji")))
  (when (member (cdr pair) (font-family-list))
    (set-fontset-font t (car pair)
                      (font-spec :family (cdr pair)) nil 'append)))
