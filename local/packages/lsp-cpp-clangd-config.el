;;; lsp-cpp-clangd-config.el --- C++ IDE‑like setup on top of your tmp.el -*- lexical-binding: t; -*-
;;
;; It assumes lsp-mode, company, lsp-ui, etc. are already installed there.

;;; Commentary:
;;  ░█▀█░█░█░█░█░█▀▀
;;  ░█▀▀░█░█░▀▄▀░█▀▀   Modern C++ workflow on Ubuntu 20.04
;;  ░▀░░░▀▀▀░░▀░░▀▀▀
;;
;;  Highlights
;;  ──────────
;;  • Zero‑config jump‑to‑definition, xref & inline docs via clangd
;;  • On‑type diagnostics & code‑actions (include‑fix‑it, rename, etc.)
;;  • company‑backed completion (snippets, keywords, #includes)
;;  • project‑root detection through Projectile/CMake/bear
;;  • One‑key clang‑format, automatic on save (dir‑local opt‑in)
;;  • Scalable to ~MLOC code bases – tuned clangd flags & lsp‑mode vars.
;;
;;  Installation prerequisites
;;  ──────────────────────────
;;  $ sudo apt install clang clang-format bear
;;    (or substitute your preferred LLVM version; keep clangd & clang-format in sync!)
;;  Inside your build dir run:
;;  $ bear -- cmake … && bear -- make -j$(nproc)
;;  → generates compile_commands.json for clangd to pick up include paths.
;;
;;; Code:

;; ---------------------------------------------------------------------------
;; LSP – clangd client tweaks -------------------------------------------------
;; ---------------------------------------------------------------------------
(with-eval-after-load 'lsp-mode
  ;; Prefer clangd over ccls if both present
  ;; Always let the remote host find its own clangd
  (setq lsp-clients-clangd-executable "clangd")

  ;; Extra juice for big codebases
  (setq lsp-clients-clangd-args
        '("--header-insertion=never"          ; don’t auto‑insert includes
          "--header-insertion-decorators=0"
          "--clang-tidy"                     ; clang‑tidy diagnostics
          "--background-index"               ; async index
          "--pch-storage=memory"             ; faster at cost of RAM
          "--completion-style=detailed"      ; show return type
          "--suggest-missing-includes"))
  ;; Reduce keystroke latency (tweak if CPU‑bound)
  (setq lsp-idle-delay 0.2
        lsp-enable-symbol-highlighting t
        lsp-log-max nil)
  ;; Handle compile_commands.json in parent dirs (cmake‑out/build/...)
  (setq lsp-clients-clangd-cache-dir (expand-file-name "~/.cache/clangd"))
  ;; Register the C/C++ major modes handled
  (add-hook 'c-mode-common-hook #'lsp-deferred))

;; ---------------------------------------------------------------------------
;; UI / completion helpers ----------------------------------------------------
;; ---------------------------------------------------------------------------
(use-package company
  :after lsp-mode
  :demand t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.05
        company-tooltip-limit 20
        company-selection-wrap-around t)
  (global-company-mode))

(use-package company-box           ; icons & docs popup (optional)
  :after company
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :hook ((c-mode c++-mode) . yas-minor-mode)
  :config (yas-reload-all))

(use-package clang-format
  :commands (clang-format-buffer clang-format-region)
  :init
  ;; Re‑use project‑local .clang-format if present; fallback to Google style.
  (setq clang-format-style-option "file")
  (defun my-c++-clang-format-save-hook ()
    (when (or (derived-mode-p 'c-mode 'c++-mode)
              (eq major-mode 'cuda-mode))
      (clang-format-buffer)))
  ;; Opt‑in: put the next line in .dir-locals.el at project root:
  ;;   ((c-mode c++-mode) (eval . (add-hook 'before-save-hook #'my-c++-clang-format-save-hook nil t)))
  )

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; (use-package projectile
;;   :demand t
;;   :config
;;   (setq projectile-project-search-path '("~/dev/" "/opt/work/"))
;;   (projectile-mode +1))

;; ---------------------------------------------------------------------------
;; Quality‑of‑life keybindings ------------------------------------------------
;; ---------------------------------------------------------------------------
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-f") #'clang-format-buffer)
  (define-key c-mode-base-map (kbd "C-c C-o") #'lsp-organize-imports)
  (define-key c-mode-base-map (kbd "C-c C-r") #'lsp-rename)
  (define-key c-mode-base-map (kbd "C-c C-a") #'lsp-execute-code-action)
  (define-key c-mode-base-map (kbd "M-?")     #'lsp-find-references)
  (define-key c-mode-base-map (kbd "M-.")     #'lsp-find-definition))

;; ---------------------------------------------------------------------------
;; DAP (debugging) – optional -------------------------------------------------
;; ---------------------------------------------------------------------------
;; Uncomment to bring in LLDB debugging via DAP-mode.
;;
;; (use-package dap-mode
;;   :after lsp-mode
;;   :commands dap-debug
;;   :config
;;   (require 'dap-lldb))

(provide 'lsp-cpp-clangd-config)
;;; lsp-cpp-clangd-config.el ends here
