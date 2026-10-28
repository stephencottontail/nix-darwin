;;; -*- lexical-binding: t -*-
(setq user-emacs-directory
      (expand-file-name "emacs" (or (getenv "XDG_CONFIG_HOME")
                                    "~/.config")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(let ((local-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file nil 'nomessage)))

(add-to-list 'backup-directory-alist
             `("." . ,(concat user-emacs-directory "/backups")))

(use-package gptel
  :config (setq gptel-log-level 'debug
                gptel-backend (gptel-make-openai "OLMo"
                                :host "localhost:8080"
                                :protocol "http"
                                :endpoint "/v1/chat/completions"
                                :stream t
                                :models '("mlx-community/OLMo-3-7B-Instruct-4bit"))))

(use-package gptel-agent
  :after gptel
  :defer t
  :config (gptel-agent-update))

(use-package ellama
  :init (require 'llm-ollama)
  :custom (ellama-provider (make-llm-ollama
                             :host "localhost"
                             :port 11434
                             :embedding-model "qwen3.5:9b"
                             :chat-model "qwen3.5:9b"
                             :default-chat-non-standard-params '((num_ctx . 16384))))
  :bind ("C-c e" . ellama-transient-main-menu)
  :config (progn ()
                 (ellama-setup-agentic-coding)
                 (ellama-context-header-line-global-mode +1)
                 (ellama-session-header-line-global-mode +1)
                 (setq ellama-tools-agent-default-max-steps 2000
                       ellama-tools-allowed '(ellama-tools-grep-in-file-tool ellama-tools-grep-tool ellama-tools-lines-range-tool ellama-tools-now-tool ellama-tools-project-root-tool ellama-tools-read-file-tool))))

(use-package magit
  :bind (("C-x g" . magit-status)))

;; This is the name of the built-in package in Emacs 30
;; I think
(use-package treesit
  :init (add-to-list 'treesit-extra-load-path
                     (concat user-emacs-directory "/tree-sitter")))

(use-package lisp-mode
  :mode "\\.lisp\\'"
  :hook (lisp-mode . (lambda ()
                       (setq indent-tabs-mode nil))))

(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :hook (emacs-lisp-mode . (lambda ()
                             (setq indent-tabs-mode nil))))

(use-package tsx-ts-mode
  :mode "\\.tsx\\'"
  :config (add-hook 'tsx-ts-mode-hook #'eglot-ensure))

(use-package envrc
  :hook (after-init . envrc-global-mode))

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
                    :font "Atkinson Hyperlegible"
                    :height 200
                    :weight 'bold)
(set-face-attribute 'variable-pitch nil
                    :font "Atkinson Hyperlegible"
                    :height 200)

(setenv "DTK_PROGRAM" "swiftmac")
(load-file "/Users/stephen/.config/emacs/site-lisp/emacspeak/lisp/emacspeak-setup.el")

