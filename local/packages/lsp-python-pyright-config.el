;;; lsp-python-pyright-config.el --- Pyright + lsp-mode  -*- lexical-binding:t -*-
;;
;; Requires Node + pyright CLI.
;;

(eval-when-compile (require 'use-package))

(defun my/pyright-command ()
  "Return a safe `lsp-pyright-langserver-command' list for this buffer."
  ;; Keep it extra-simple on remote hosts where Node/pyright may be missing.
  (if (file-remote-p default-directory)
      '("pyright-langserver" "--stdio")
    ;; Local: allow multi-root & use cache dir under ~/.cache/pyright
    (let* ((cache (expand-file-name "pyright" (xdg-cache-home))))
      (make-directory cache :parents)
      (list "pyright-langserver" "--stdio"
            "--lib" "--typeshed-path" cache))))

(use-package lsp-pyright
  :ensure t                          ;; install via straight/ELPA if not present
  :defer t
  :init
  ;; Start automatically for Python
  (add-hook 'python-mode-hook #'lsp-deferred)
  ;; Disable pylsp so lsp-mode chooses pyright
  (setq lsp-disabled-clients '(pylsp))
  :custom
  (lsp-pyright-typechecking-mode "basic")     ; "basic"|"strict"|"off"
  (lsp-pyright-multi-root nil)                ; one server per project
  (lsp-pyright-diagnostic-mode "workspace")   ; analyse whole project
  :config
  (setq lsp-pyright-langserver-command (my/pyright-command)))

(provide 'lsp-python-pyright-config)
;;; lsp-python-pyright-config.el ends here
