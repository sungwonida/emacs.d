;;; lsp-cpp-clangd-refresh-compile-db.el --- one-touch function to reload clangd compile commands -*- lexical-binding: t; -*-
;;
;; Usage:
;;   (load "lsp-cpp-clangd-refresh-compile-db")
;;   ;; Optional convenient binding:
;;   (with-eval-after-load 'cc-mode
;;     (define-key c-mode-base-map (kbd "C-c C-c") #'clangd-reload-compile-db))
;;
;; Call `clangd-reload-compile-db` after you have regenerated or moved
;; `compile_commands.json` so clangd will pick up the new file.
;;
;;; Code:

(defun clangd-reload-compile-db ()
  "Restart the clangd LSP workspace so it rereads the current project's compile_commands.json."
  (interactive)
  (if-let ((workspace (lsp-find-workspace 'clangd (buffer-file-name))))
      (progn
        (lsp-workspace-restart workspace)
        (message "clangd workspace restarted – compile_commands.json reloaded."))
    (user-error "No active clangd workspace for this buffer")))

(provide 'lsp-cpp-clangd-refresh-compile-db)
;;; lsp-cpp-clangd-refresh-compile-db.el ends here
