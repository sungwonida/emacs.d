;;; image-dired+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (image-diredx-async-mode image-diredx-adjust-mode
;;;;;;  image-diredx-setup imagex-dired-show-image-thumbnails) "image-dired+"
;;;;;;  "image-dired+.el" (21872 64307 469353 593000))
;;; Generated autoloads from image-dired+.el

(autoload 'imagex-dired-show-image-thumbnails "image-dired+" "\
Utility to insert thumbnail of FILES to BUFFER.
That thumbnails are not associated to any dired buffer although.

\(fn BUFFER FILES &optional APPEND)" nil nil)

(autoload 'image-diredx-setup "image-dired+" "\
Setup image-dired extensions.

\(fn)" nil nil)

(defvar image-diredx-adjust-mode nil "\
Non-nil if Image-Diredx-Adjust mode is enabled.
See the command `image-diredx-adjust-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `image-diredx-adjust-mode'.")

(custom-autoload 'image-diredx-adjust-mode "image-dired+" nil)

(autoload 'image-diredx-adjust-mode "image-dired+" "\
Extension for `image-dired' show image as long as adjusting to frame.

\(fn &optional ARG)" t nil)

(defvar image-diredx-async-mode nil "\
Non-nil if Image-Diredx-Async mode is enabled.
See the command `image-diredx-async-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `image-diredx-async-mode'.")

(custom-autoload 'image-diredx-async-mode "image-dired+" nil)

(autoload 'image-diredx-async-mode "image-dired+" "\
Extension for `image-dired' asynchrounous image thumbnail.

\(fn &optional ARG)" t nil)
(add-hook 'image-dired-thumbnail-mode-hook 'image-diredx-setup)
(image-diredx-async-mode 1)

;;;***

;;;### (autoloads nil nil ("image-dired+-pkg.el") (21872 64307 545416
;;;;;;  79000))

;;;***

(provide 'image-dired+-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; image-dired+-autoloads.el ends here
