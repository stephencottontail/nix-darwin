;;; -*- lexical-binding: t -*-
(setq user-emacs-directory
      (expand-file-name "emacs" (or (getenv "XDG_CONFIG_HOME")
				    "~/.config")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(let ((local-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file nil 'nomessage)))

(add-to-list 'backup-directory-alist
	     `(("." . ,(concat user-emacs-directory "/backups"))))

;; This is the name of the built-in package in Emacs 30
;; I think
(use-package treesit
  :init (add-to-list 'treesit-extra-load-path
		     (concat user-emacs-directory "/tree-sitter")))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :config (add-hook 'tsx-ts-mode-hook #'eglot-ensure))

(use-package slime
  :init (add-hook 'lisp-mode-hook 'slime-mode)
  :config (progn ()
		 (setq browse-url-handlers
		       '(("lispworks\\.com" . eww-browse-url))
		       inferior-lisp-program "ros -Q run")))

(load-theme 'modus-vivendi-tinted t)
(tool-bar-mode -1)
(setq mac-option-modifier '(:ordinary meta :function meta :mouse nil)
      mac-command-modifier 'nil)
(set-face-attribute 'default nil
		    :font "3270 Nerd Font"
		    :height 240)
(set-face-attribute 'variable-pitch nil
		    :font "Geneva"
		    :height 180)

