;;; cpp-config-lsp-clangd.el --- C++ IDE‑like setup on top of your tmp.el -*- lexical-binding: t; -*-
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
  (setq lsp-clients-clangd-cache-dir (expand-file-name "~/.cache/clangd")))

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

;; ---------------------------------------------------------------------------
;; Quality‑of‑life keybindings ------------------------------------------------
;; ---------------------------------------------------------------------------
(with-eval-after-load 'cc-mode
  ;; (define-key c-mode-base-map (kbd "C-c C-f") #'clang-format-buffer)
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

;; ---------------------------------------------------------------------------
;; CC-mode helpers -----------------------------------------------------------
;; ---------------------------------------------------------------------------
(defun my/cpp-dim-if0 ()
  "Dim code excluded by preprocessor conditionals like `#if 0 ... #endif`."
  (require 'cpp) ;; built-in
  ;; Make all cpp.el variables buffer-local so we don’t affect other buffers.
  (setq-local cpp-known-face 'default)
  (setq-local cpp-unknown-face 'default)
  ;; Set to 'dark or 'light depending on your theme background.
  (setq-local cpp-face-type 'dark)
  (setq-local cpp-known-writable t)
  (setq-local cpp-unknown-writable t)

  ;; Dim false branches (#if 0) using a neutral gray.
  ;; (You can swap `(foreground-color . "dim gray")` with `(face . shadow)` if you prefer.)
  (setq-local cpp-edit-list
              '(("1" nil (foreground-color . "dim gray") both nil)  ; true branch style for known “1”
                ("0" (foreground-color . "dim gray") nil both nil))) ; false branch style for known “0”
  ;; Apply overlays
  (cpp-highlight-buffer t))

(defun my/c-ts-mode-common-setup ()
  "Run everything I normally put in `c-mode-common-hook'
   when visiting a `c-ts-mode' or `c++-ts-mode' buffer."
  ;; features from my-c-mode-common-hook -------------------
  (hs-minor-mode 1)
  (local-set-key (kbd "M-o") #'ff-get-other-file)
  (local-set-key (kbd "M-m") #'helm-semantic-or-imenu)
  (local-set-key (kbd "C-c u") #'hs-toggle-hiding)
  (local-set-key (kbd "C-c <down>") #'hs-hide-all)
  (local-set-key (kbd "C-c <up>") #'hs-show-all)
  (my/cpp-dim-if0))

;; run it in both “old” and new modes
(add-hook 'c-mode-common-hook  #'my/c-ts-mode-common-setup) ; CC Mode family (c-mode/c++-mode/etc.)
(add-hook 'c-ts-mode-hook      #'my/c-ts-mode-common-setup) ; Tree-sitter C
(add-hook 'c++-ts-mode-hook    #'my/c-ts-mode-common-setup) ; Tree-sitter C++

(defun my/c-ts-mode-indent-setup ()
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local indent-tabs-mode nil)
  ;; Don’t let LSP reformat as you type / on TAB
  (when (boundp 'lsp-enable-indentation)
    (setq-local lsp-enable-indentation nil)
    (setq-local lsp-enable-on-type-formatting nil))
  ;; Make region indent behave like cc-mode (no formatter surprises)
  (setq-local indent-region-function #'indent-region-line-by-line)
  ;; Keep TAB = indent (not formatter)
  (define-key (current-local-map) (kbd "TAB") #'indent-for-tab-command))

(add-hook 'c-ts-mode-hook   #'my/c-ts-mode-indent-setup)
(add-hook 'c++-ts-mode-hook #'my/c-ts-mode-indent-setup)

;; my/cpp-dim-if0-refresh --- a quick toggle if you ever want to re-apply or clear in the current buffer.
(defun my/cpp-dim-if0-refresh () (interactive) (my/cpp-dim-if0))
(global-set-key (kbd "C-c C-~") #'my/cpp-dim-if0-refresh)

;; clangd-reload-compile-db --- one-touch function to reload clangd compile commands
;;
;; Usage:
;;   (load "lsp-cpp-clangd-refresh-compile-db")
;;   ;; Optional convenient binding:
;;   (with-eval-after-load 'cc-mode
;;     (define-key c-mode-base-map (kbd "C-c C-c") #'clangd-reload-compile-db))
;;
;; Call `clangd-reload-compile-db` after you have regenerated or moved
;; `compile_commands.json` so clangd will pick up the new file.
(defun clangd-reload-compile-db ()
  "Restart the clangd LSP workspace so it rereads the current project's compile_commands.json."
  (interactive)
  (if-let ((workspace (lsp-find-workspace 'clangd (buffer-file-name))))
      (progn
        (lsp-workspace-restart workspace)
        (message "clangd workspace restarted – compile_commands.json reloaded."))
    (user-error "No active clangd workspace for this buffer")))


(provide 'cpp-config-lsp-clangd)
;;; cpp-config-lsp-clangd.el ends here
