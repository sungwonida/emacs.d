;;; yaml-config.el --- YAML editing with yaml-pro (Tree-sitter) -*- lexical-binding: t; -*-

;;; Commentary:
;; Purpose
;; -------
;; Configure YAML editing using `yaml-pro` with the Tree-sitter major mode
;; (`yaml-pro-ts-mode`). It associates *.yml / *.yaml files with the mode.
;;
;; Requirements
;; ------------
;; - Emacs 29+ (Tree-sitter integration)
;; - `use-package` available at compile time
;; - One of:
;;   * straight.el (this file uses `:straight t`), or
;;   * package.el (switch `:straight t` -> `:ensure t`)
;;
;; Optional (but recommended)
;; --------------------------
;; - YAML Tree-sitter grammar installed:
;;     M-x treesit-install-language-grammar RET yaml RET
;;   Verify with:
;;     (treesit-language-available-p 'yaml)
;;
;; - YAML LSP support (optional):
;;   Install the language server, then enable `lsp-mode` in YAML buffers.
;;     npm i -g yaml-language-server
;;   Minimal Emacs setup:
;;     (use-package lsp-mode
;;       :commands lsp
;;       :hook (yaml-pro-ts-mode . lsp))
;;
;; What this config does
;; ---------------------
;; - Ensures `use-package` macro is available at byte-compile time.
;; - Installs/loads `yaml-pro`.
;; - Uses `yaml-pro-ts-mode` for *.yml / *.yaml files.
;;
;; Verification
;; ------------
;; 1) Open a YAML file (e.g., test.yaml).
;; 2) M-: major-mode RET  => should print `yaml-pro-ts-mode`.
;; 3) Optional: M-x lsp  => should start the YAML language server if installed.
;;
;; Pitfalls & Notes
;; ----------------
;; - If Emacs < 29 or Tree-sitter grammar is missing, `yaml-pro-ts-mode` may not
;;   activate. Install the grammar or use a non-TS fallback (e.g., `yaml-mode`).
;; - If you use package.el, replace `:straight t` with `:ensure t`.
;; - On locked-down networks, `treesit-install-language-grammar` might need proxy
;;   env vars to fetch the grammar sources.
;;
;; Platform notes
;; --------------
;; - WSL2 Ubuntu: Make sure your Node/npm is available for `yaml-language-server`
;;   if you plan to use LSP.
;; - macOS (Apple Silicon): Ensure your grammar build tools (git, a C compiler)
;;   are present; then run `treesit-install-language-grammar`.
;;
;;; Code:

(eval-when-compile (require 'use-package))   ;; make sure the macro exists

;; Install the major mode that wraps the grammar
(use-package yaml-pro
  :straight t                      ;; or :ensure t if you rely on package.el
  :mode ("\\.ya?ml\\'" . yaml-pro-ts-mode))


(provide 'yaml-config)
