;;; -*- lexical-binding: t -*-
(setq user-emacs-directory
      (expand-file-name "emacs" (or (getenv "XDG_CONFIG_HOME")
				    "~/.config")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(let ((local-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file nil (quote nomessage))))

;; This is the name of the built-in package in Emacs 30
;; I think
(use-package treesit
  :init (add-to-list 'treesit-extra-load-path
		     (concat user-emacs-directory "/tree-sitter")))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :config (add-hook 'tsx-ts-mode-hook #'eglot-ensure))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package sly
  :commands sly
  :config (when (executable-find (getenv "LISP"))
	    (setq inferior-lisp-program (getenv "LISP"))))

(use-package symex
  :init (global-set-key (kbd "M-;") #'symex-mode-interface)
  :hook ((emacs-lisp-mode lisp-mode) . symex-mode))

(use-package symex-core
  :after (symex))

(use-package symex-ide
  :after (symex))

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
