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

;;; --- WSLg (Wayland) clipboard: stable + fast, Emacs 30.x ---
;;; Requires: wl-clipboard  (sudo apt install wl-clipboard)

(eval-when-compile (require 'cl-lib))

;; Sensible defaults: Emacs yanks use the kill ring only (fast/predictable)
(setq interprogram-cut-function nil
      interprogram-paste-function #'my/wl-paste  ; If this brings any issues like stale external clipboard trumping your kill ring, set it to `nil`.
      save-interprogram-paste-before-kill nil)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING TEXT STRING)
      select-enable-clipboard t
      select-enable-primary t)

(defgroup my/wl-clipboard nil
  "WSLg/Wayland clipboard mirroring via wl-clipboard."
  :group 'convenience)

(defcustom my/wl-copy-commands
  '(kill-ring-save kill-region kill-line kill-word backward-kill-word copy-region-as-kill
    cua-copy-region cua-cut-region cua-copy-rectangle
    vterm-copy-mode-done magit-copy-buffer-revision
    mouse-save-then-kill dired-copy-filename-as-kill)
  "Commands treated as explicit copies to mirror to OS clipboard."
  :type '(repeat symbol))

(defcustom my/wl-copy-publish-primary nil
  "Also publish the PRIMARY selection (middle-click) when mirroring."
  :type 'boolean)

(defvar my/wl-debug nil
  "If non-nil, log wl-clipboard actions to *Messages*.")

(defun my/wl--have-tools-p ()
  (and (executable-find "wl-copy")
       (executable-find "wl-paste")))

(defun my/wl-copy-oneshot (text &optional primary)
  "Own the Wayland CLIPBOARD (or PRIMARY) until the first paste, then exit.
Uses:  wl-copy --paste-once --type text/plain;charset=utf-8"
  (when (and (my/wl--have-tools-p) (stringp text) (> (length text) 0))
    (let* ((args (append (when primary '("--primary"))
                         '("--paste-once" "--type" "text/plain;charset=utf-8")))
           (coding-system-for-write 'utf-8-unix)
           (out (generate-new-buffer " *wl-copy*"))
           status)
      (with-temp-buffer
        (insert text)
        (setq status (apply #'call-process-region
                            (point-min) (point-max)
                            "wl-copy" nil out nil args)))
      (when my/wl-debug
        (message "[wl] wl-copy status=%s args=%S out=%s"
                 status args (with-current-buffer out (buffer-string))))
      (kill-buffer out)
      (= status 0))))

(defun my/wl-after-kill (text &rest _)
  "Mirror TEXT to OS clipboard after Emacs updates the kill ring."
  (when my/wl-debug
    (message "[wl] after-kill: this-command=%S bytes=%d"
             this-command (length text)))
  (when (memq this-command my/wl-copy-commands)
    (my/wl-copy-oneshot text)
    (when my/wl-copy-publish-primary
      (my/wl-copy-oneshot text 'primary))))

;;; Global toggle: enable/disable mirroring without touching your kill-ring behavior
(define-minor-mode my/wl-clipboard-mirroring-mode
  "Mirror explicit copies to Wayland clipboard using wl-copy --paste-once."
  :global t :lighter " WLclip"
  (if my/wl-clipboard-mirroring-mode
      (advice-add  'kill-new :after #'my/wl-after-kill)
    (advice-remove 'kill-new :after #'my/wl-after-kill)))

;;; OS→Emacs paste on demand (bind to C-S-v): normalize CRLF→LF
(defun my/wl-paste ()
  "Return string from OS (Wayland) clipboard, or nil."
  (when (executable-find "wl-paste")
    (let* ((coding-system-for-read 'utf-8-unix)
           (buf (generate-new-buffer " *wl-paste*"))
           (status (with-current-buffer buf
                     (call-process "wl-paste" nil t nil "--no-newline")))
           s)
      (unwind-protect
          (when (and (integerp status) (= status 0))
            (setq s (with-current-buffer buf (buffer-string)))
            (when (stringp s)
              (setq s (replace-regexp-in-string "\r\n" "\n" s))))
        (kill-buffer buf))
      s)))

(defun my/os-clipboard-yank ()
  "Insert text from the OS clipboard into the current buffer."
  (interactive)
  (if-let ((s (my/wl-paste)))
      (insert s)
    (user-error "OS clipboard is empty (wl-paste returned nothing)")))

(global-set-key (kbd "C-S-v") #'my/os-clipboard-yank)

;;; One-shot health check & round-trip test
(defun my/wl-clipboard-health ()
  "Report wl-clipboard & WSLg status; do a one-shot round-trip test."
  (interactive)
  (let* ((ws window-system)
         (way (getenv "WAYLAND_DISPLAY"))
         (copy (executable-find "wl-copy"))
         (paste (executable-find "wl-paste"))
         ;; 24-bit hex token, avoids most-positive-fixnum entirely
         (token (format "WLTEST-%06X" (random #x1000000)))
         (copy-ok nil) (rt nil) (ok nil))
    (message "WL Health: window-system=%S WAYLAND_DISPLAY=%S wl-copy=%S wl-paste=%S"
             ws way copy paste)
    (setq copy-ok (and copy
                       (with-temp-buffer
                         (insert token)
                         (eq 0 (call-process-region
                                (point-min) (point-max)
                                "wl-copy" nil nil nil
                                "--paste-once" "--type" "text/plain;charset=utf-8")))))
    (setq rt (when paste
               (with-output-to-string
                 (call-process "wl-paste" nil standard-output nil "--no-newline"))))
    (setq rt (when (stringp rt) (replace-regexp-in-string "\r\n" "\n" rt)))
    (setq ok (and copy-ok (string= token rt)))
    (message (if ok
                 "WL Health: OK (round-trip %s)"
               "WL Health: FAIL (copy-ok=%S, got=%S)")
             token copy-ok rt)
    ok))

;; Enable the Wayland/WSL clipboard mirroring ONLY when in WSL + Wayland
(when (and (eq system-type 'gnu/linux)
           (getenv "WSL_DISTRO_NAME")
           (getenv "WAYLAND_DISPLAY"))
  (my/wl-clipboard-mirroring-mode 1))


;;; early-init.el ends here
