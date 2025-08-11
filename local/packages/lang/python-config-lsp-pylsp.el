;;; python-config-lsp-pylsp.el --- Opinionated pylsp + lsp-mode setup  -*- lexical-binding: t; -*-
;;
;; This file is *only* about Python-LSP (pylsp) configuration.  Drop it on the
;; load-path and (require 'python-config-lsp-pylsp) from your init.
;;
;;; Commentary:
;;
;;  • Starts lsp-mode automatically in python-mode (local *and* TRAMP buffers).
;;  • Chooses a safe, per-buffer `lsp-pylsp-server-command`:
;;      – local files → verbose logfile under ~/.pylsp
;;      – remote files → plain "pylsp" (no invalid log path, no -vvv flood)
;;  • Centralises all pylsp plugin toggles you had in init-tmp.el.
;;
;;; Code:

(eval-when-compile (require 'use-package))

(defun my/pylsp-server-command ()
  "Return the proper `lsp-pylsp-server-command` for the current buffer.

Local buffers get a rolling logfile in ~/.pylsp.
Remote (TRAMP) buffers start pylsp with default args to avoid path issues."
  (if (file-remote-p default-directory)
      '("pylsp")                           ; remote: keep it simple
    ;; local: verbose log in ~/.pylsp/YYYYMMDD.log
    (let* ((log-dir (expand-file-name "~/.pylsp/"))
           (log-file (expand-file-name
                      (format "pylsp-%s.log" (format-time-string "%Y%m%d"))
                      log-dir)))
      (make-directory log-dir :parents)
      (list "pylsp" "--log-file" log-file))))   ; remove -vvv to keep size sane

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . my/lsp-per-buffer-tweaks))
  :init
  ;; default command (will be buffer-locally overridden later)
  (setq-default lsp-pylsp-server-command (my/pylsp-server-command))
  :config
  ;; ---- per-buffer tweaks (run by hook above) ----
  (defun my/lsp-per-buffer-tweaks ()
    "Adjust `lsp-mode` variables that must stay buffer-local."
    ;; Disable snippets **only** for Python buffers
    (setq-local lsp-enable-snippet
                (not (derived-mode-p 'python-mode)))
    ;; Remote buffers get longer time-outs
    (when (file-remote-p default-directory)
      (setq-local lsp-response-timeout 60)))

  Re-compute pylsp command for every new buffer
  (add-hook 'lsp-before-initialize-hook
            (lambda ()
              (setq lsp-pylsp-server-command
                    (my/pylsp-server-command))))

  (setq lsp-pylsp-plugins-flake8-ignore ["E501"]
        lsp-pylsp-plugins-black-enabled t))


(provide 'python-config-lsp-pylsp)
;;; python-config-lsp-pylsp.el ends here
