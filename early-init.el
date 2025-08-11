;; Default font (size is 1/10 pt; 140 = 14pt)
;; Pick a font you have inside WSL
(set-face-attribute 'default nil :family "Monaco" :height 140)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Speed up font rendering over the Wayland socket
(setq inhibit-compacting-font-caches t)

;; Avoid giant frames when Windows monitor DPI changes
(setq frame-resize-pixelwise t)

;; Better clipboard/selection behaviour
(setq select-enable-clipboard t
      select-enable-primary nil)

;; Make webkit xwidgets obey dark mode (GTK >= 3.24.30)
(when (featurep 'xwidget-internal)
  (add-hook 'xwidget-webkit-mode-hook
            (lambda ()
              (xwidget-webkit-execute-script
               (xwidget-webkit-current-session)
               "document.documentElement.style.background='#1e1e1e';"))))
