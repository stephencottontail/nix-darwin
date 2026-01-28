;;; -*- lexical-binding: t -*-
(setq user-emacs-directory
      (expand-file-name "emacs" (or (getenv "XDG_CONFIG_HOME")
				    "~/.config")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(let ((local-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file nil (quote nomessage))))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(load-theme (quote modus-vivendi-tinted) t)
(add-to-list 'default-frame-alist '(font . "UnixWare"))
(add-hook (quote after-make-frame-functions)
          (lambda (frame)
            (with-selected-frame frame
              (set-frame-font "UnixWare" t t))))
(add-hook (quote server-after-make-frame-hook)
	  (lambda (frame)
	    (with-selected-frame frame
	      (sat-frame-font "UnixWare" t t))))
