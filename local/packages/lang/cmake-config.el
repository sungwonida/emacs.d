;;; cmake-config.el --- Tree-sitter setup for CMake -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal setup to use Tree-sitter’s `cmake-ts-mode` for CMake files.
;;
;; Requirements:
;; - Emacs with Tree-sitter support (Emacs 29+; `cmake-ts-mode` is in newer builds).
;; - The CMake grammar installed (e.g., via:
;;     (add-to-list 'treesit-language-source-alist
;;                  '(cmake "https://github.com/uyha/tree-sitter-cmake"))
;;     (treesit-install-language-grammar 'cmake)
;;   )
;; - Ensure Emacs can find the built grammar (.so on Linux/WSL2, .dylib on macOS):
;;   - Default location: ~/.emacs.d/tree-sitter/
;;   - Or point to your custom dir: (add-to-list 'treesit-extra-load-path "/path/to/dir")
;;
;; Verification:
;;   M-: (treesit-available-p)               ;; => t
;;   M-: (treesit-language-available-p 'cmake) ;; => t
;;   Open CMakeLists.txt and confirm mode line shows `cmake-ts-mode`.

;;; Code:

;; 1) Prefer the Tree-sitter major mode whenever `cmake-mode` would be chosen.
;;    This keeps your file associations intact but upgrades the actual major mode.
(add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))

;; 2) Associate common CMake file names with `cmake-ts-mode`.
;;    - CMake’s top-level/build files:
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
;;    - Module/helper files:
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))

(provide 'cmake-config)
;;; cmake-config.el ends here
