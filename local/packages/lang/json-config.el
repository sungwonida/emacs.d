;;; json-config.el --- JSON editing with json-ts-mode (Tree-sitter) -*- lexical-binding: t; -*-

(eval-when-compile (require 'use-package))   ;; make sure the macro exists

(with-eval-after-load 'treesit
  (add-hook 'json-ts-mode-hook
            (lambda () (setq-local json-ts-mode-indent-offset 2))))


(provide 'json-config)
