(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(ac-auto-start nil)
 '(c-basic-offset 4)
 '(c-default-style
   '((c-mode . "k&r")
     (c++-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(cua-rectangle-mark-key [C-M-return])
 '(custom-safe-themes
   '("6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "77b9cad4f0e64f7267acc55181d5c1999627b16f9d6424ed57420a39134e66e7" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "437cd756e079901ccdecd9c397662a3ee4da646417d7469a1c35aa8e246562fe" "8feca8afd3492985094597385f6a36d1f62298d289827aaa0d8a62fe6889b33c" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(lsp-ui-sideline-enable nil)
 '(ns-command-modifier 'meta)
 '(org-agenda-files
   '("/home/david/org/personal/todo.org" "/home/david/org/personal/shopping_list.org" "/home/david/org/personal/note.org" "/home/david/org/personal/indebtedness.org" "/home/david/org/personal/financialplan.org" "/home/david/org/personal/career_description.org" "/home/david/org/note.org" "/home/david/org/learning/voca.org" "/home/david/org/learning/terms.org" "/home/david/org/learning/study.org" "/home/david/org/learning/rce.org" "/home/david/org/learning/Fourier/Fourier_Summary.org" "/home/david/org/learning/Fourier/An_Intuitive_Explanation_of_Fourier_Theory.org" "/home/david/org/learning/dl4cv/progress.org" "/home/david/org/learning/Direct_Gray_Scale_Minutiae_Detection.org" "/home/david/org/journal.org" "/home/david/org/inbox.org" "/home/david/org/gtd.org" "/home/david/org/event.org" "/home/david/org/dict.org" "/home/david/org/development/note.org" "/home/david/org/development/languages.org" "/home/david/org/development/emacs/todo.org" "/home/david/org/development/emacs/note.org" "/home/david/org/business/siliconcube/nvidia_ai_conference_2019.org" "/home/david/org/business/canvasbio/projects/Stickman/note.org" "/home/david/org/business/canvasbio/projects/muon/todo.org" "/home/david/org/business/canvasbio/projects/muon/note.org" "/home/david/org/business/canvasbio/projects/M/todo.org" "/home/david/org/business/canvasbio/projects/fermion/todo.org" "/home/david/org/business/canvasbio/projects/fermion/note.org" "/home/david/org/business/canvasbio/projects/dalton/todo.org" "/home/david/org/business/canvasbio/projects/dalton/note.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150126.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150124.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150123.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150122.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150121.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150120.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150119.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150108.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150107.org" "/home/david/org/business/canvasbio/dailyrecord/dailyrecord_150106.org" "/home/david/org/business/canvasbio/cb_internal_functions.org" "/home/david/org/business/canvasbio/6th_entrance_education.org" "/home/david/org/agenda-2019.org"))
 '(org-agenda-start-on-weekday 0)
 '(org-log-done 'time)
 '(org-stuck-projects
   '("+LEVEL=2/-DONE"
     ("TODO" "NEXT" "NEXTACTION" "CANCELED" "RECEIVED")
     nil ""))
 '(package-selected-packages
   '(cuda-mode vscode-dark-plus-theme highlight-symbol caroline-theme flatui-theme github-modern-theme hydandata-light-theme leuven-theme tommyh-theme twilight-bright-theme treemacs-projectile lsp-treemacs srefactor smart-compile org-gcal yasnippet-snippets stickyfunc-enhance ag helm-projectile atom-one-dark-theme helm-lsp use-package company-lsp lsp-ui lsp-mode ztree docker conda cmake-mode flycheck flycheck-irony company-irony-c-headers company-irony irony helm-descbinds god-mode elpy py-autopep8 smartparens unicode-fonts tern-auto-complete smart-mode-line rainbow-mode rainbow-delimiters plantuml-mode pallet markdown-mode magit jedi impatient-mode helm-ls-git helm-gtags helm-git-grep helm-flycheck helm-ag google-translate exec-path-from-shell edit-server diminish company color-identifiers-mode cider bind-key anzu))
 '(recentf-keep
   '((lambda
       (file)
       (cond
        ((file-remote-p file nil t)
         (file-readable-p file))
        ((file-readable-p file))))))
 '(show-paren-mode t)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-code-action ((t (:foreground "indian red")))))
