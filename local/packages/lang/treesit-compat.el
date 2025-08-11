;;; treesit-compat.el --- Make existing hooks & keymaps work with *-ts-modes  -------------------

;;; --- Tree-sitter integration -----------------------------------------------
;; Emacs 29 ships native treesit.el support.  Emacs ≤ 28 needs the external bridge.

(if (and (fboundp 'treesit-available-p) (treesit-available-p)) ; Emacs 29+
    ;; ── Native route ────────────────────────────────────────────────────────
    (progn
      ;; 1. Auto-install/compile grammars the first time you visit a file
      (use-package treesit-auto
        :straight (:host github :repo "renzmann/treesit-auto")
        :demand t
        :custom (treesit-auto-install 'prompt)   ; 'always | 'prompt | nil
        :config (global-treesit-auto-mode))

      ;; 2. Tell Emacs to prefer *-ts-mode over the legacy regex modes
      (setq major-mode-remap-alist
            '((c-mode          . c-ts-mode)
              (c++-mode        . c++-ts-mode)
              (python-mode     . python-ts-mode)
              (json-mode       . json-ts-mode)
              (yaml-mode       . yaml-ts-mode)
              (js-mode         . js-ts-mode)
              (typescript-mode . typescript-ts-mode)
              (sh-mode         . bash-ts-mode)
              (cmake-mode      . cmake-ts-mode)))

      ;; 3. Where grammars aren’t bundled with Emacs, tell treesit-auto
      (setq treesit-language-source-alist
            '((c   "https://github.com/tree-sitter/tree-sitter-c")
              (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
              (python  "https://github.com/tree-sitter/tree-sitter-python")
              (yaml    "https://github.com/ikatyang/tree-sitter-yaml")
              (bash    "https://github.com/tree-sitter/tree-sitter-bash")
              (cmake   "https://github.com/uyha/tree-sitter-cmake")))

      ;; 4. (Optional) richer highlighting: 0–4, higher = more faces
      (setq treesit-font-lock-level 4))

  ;; ── Legacy route for Emacs 28 and earlier ────────────────────────────────
  (use-package tree-sitter         :straight t  :hook ((prog-mode . tree-sitter-mode)
                                                       (tree-sitter-after-on . tree-sitter-hl-mode)))
  (use-package tree-sitter-langs   :straight t))


(provide 'treesit-compat)
;;; treesit-compat.el ends here
