;;; python-config-lsp-pyright.el --- Pyright + lsp-mode  -*- lexical-binding:t -*-
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

(with-eval-after-load 'lsp-mode
  ;; lsp-mode clones local clients for TRAMP by default; that can bake in local
  ;; absolute paths (e.g., ~/.emacs.d/.cache/lsp/...), which won't exist remotely.
  (setq lsp-auto-register-remote-clients nil)

  (with-eval-after-load 'lsp-pyright
    ;; lsp-pyright recommends setting this to "pyright" (or "basedpyright") locally.
    (setq lsp-pyright-langserver-command "pyright")

    (lsp-register-client
     (make-lsp-client
      :new-connection
      (lsp-tramp-connection
       (lambda ()
         '("pyright-langserver" "--stdio")))
      :major-modes '(python-mode python-ts-mode)
      :remote? t
      :server-id 'pyright-tramp
      :priority 3))))

;;;; 2.  Python helpers ───────────────────────────────────────────────────────
(defun my/python-common-setup ()
  "Things I want in both `python-mode' and `python-ts-mode'."
  (local-set-key (kbd "M-m") #'helm-semantic-or-imenu)
  (setq-local forward-sexp-function nil))

(add-hook 'python-mode-hook    #'my/python-common-setup)
(add-hook 'python-ts-mode-hook #'my/python-common-setup)

;;;; 3.  LSP auto-start for new modes  ---------------------------------------
(with-eval-after-load 'lsp-mode
  (dolist (hook '(c-ts-mode-hook c++-ts-mode-hook python-ts-mode-hook))
    (add-hook hook #'lsp-deferred)))


(provide 'python-config-lsp-pyright)
;;; python-config-lsp-pyright.el ends here
