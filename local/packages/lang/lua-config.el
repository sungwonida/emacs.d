;;; lua-config.el --- Tree-sitter setup for Lua -*- lexical-binding: t; -*-

;;; Commentary:
;; Purpose
;; -------
;; Configure Lua editing using the built-in Tree-sitter major mode
;; (`lua-ts-mode', shipped with Emacs 29.1+).  It registers the grammar
;; source, associates *.lua files with the mode, remaps the legacy
;; `lua-mode', and sets a sane indentation offset.
;;
;; Requirements
;; ------------
;; - Emacs 29.1+ with Tree-sitter support (`lua-ts-mode' is bundled).
;; - The Lua grammar installed.  Two ways to get it:
;;     * Automatically: `treesit-auto' (enabled in `treesit-compat.el')
;;       will prompt to install it the first time you open a *.lua file.
;;     * Manually:
;;         M-x treesit-install-language-grammar RET lua RET
;;   Verify with:
;;     M-: (treesit-language-available-p 'lua)  ;; => t
;;   The built grammar lands in ~/.emacs.d/tree-sitter/ (.dylib on macOS,
;;   .so on Linux/WSL2).
;;
;; Optional (but recommended)
;; --------------------------
;; - Linting: `lua-ts-mode' wires `flymake-mode' to `luacheck' out of the
;;   box.  Install luacheck (`luarocks install luacheck') and enable
;;   `flymake-mode' to get on-the-fly diagnostics.
;; - LSP: install lua-language-server and enable `lsp-mode' in Lua buffers:
;;     brew install lua-language-server      ;; macOS
;;     ;; then, e.g.
;;     (add-hook 'lua-ts-mode-hook #'lsp-deferred)
;;
;; What this config does
;; ---------------------
;; - Registers the Lua grammar source for `treesit-install-language-grammar'.
;; - Remaps `lua-mode' -> `lua-ts-mode' so existing file associations upgrade.
;; - Associates *.lua with `lua-ts-mode' unconditionally, so visiting a Lua
;;   file selects the mode even before the grammar is built (which then lets
;;   `treesit-auto' offer to install it).
;; - Sets `lua-ts-indent-offset' to 4.
;;
;; Verification
;; ------------
;; 1) Open a Lua file (e.g., test.lua).
;; 2) M-: major-mode RET  => should print `lua-ts-mode'.
;; 3) M-: (treesit-language-available-p 'lua) RET  => t once installed.
;;
;; Pitfalls & Notes
;; ----------------
;; - If the grammar is missing, `lua-ts-mode' still loads but font-lock and
;;   indentation stay inert until you install it (see Requirements).
;; - On locked-down networks, `treesit-install-language-grammar' may need
;;   proxy env vars (and a C compiler + git) to fetch and build the grammar.
;;
;;; Code:

(eval-when-compile (require 'use-package))   ;; make sure the macro exists

;; 1) Register the grammar source so M-x treesit-install-language-grammar works.
;;    (`treesit-auto' supplies the same recipe, but registering it here keeps
;;    this module self-contained and matches the other lang configs.)
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))))

;; 2) Prefer the Tree-sitter major mode whenever `lua-mode' would be chosen,
;;    and associate *.lua files with `lua-ts-mode'.  `lua-ts-mode' only
;;    self-registers these when the grammar is already built, so we do it
;;    unconditionally here.
(add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

;; 3) Indentation.
(add-hook 'lua-ts-mode-hook
          (lambda () (setq-local lua-ts-indent-offset 4)))

(provide 'lua-config)
;;; lua-config.el ends here
