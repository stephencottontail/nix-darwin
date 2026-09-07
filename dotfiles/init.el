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

(use-package meow
  :init (defun meow-setup ()
          (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
                meow-char-thing-table '((114 . round)
                                        (115 . square)
                                        (99 . curly)
                                        (103 . string)
                                        (101 . symbol)
                                        (119 . window)
                                        (98 . buffer)
                                        (112 . paragraph)
                                        (108 . line)
                                        (118 . visual-line)
                                        (100 . defun)
                                        (46 . sentence)))
          (meow-motion-define-key
           '("j" . meow-next)
           '("k" . meow-prev)
           '("<escape>" . ignore))
          (meow-leader-define-key
           ;; Use SPC (0-9) for digit arguments.
           '("1" . meow-digit-argument)
           '("2" . meow-digit-argument)
           '("3" . meow-digit-argument)
           '("4" . meow-digit-argument)
           '("5" . meow-digit-argument)
           '("6" . meow-digit-argument)
           '("7" . meow-digit-argument)
           '("8" . meow-digit-argument)
           '("9" . meow-digit-argument)
           '("0" . meow-digit-argument)
           '("/" . meow-keypad-describe-key)
           '("?" . meow-cheatsheet))
          (meow-normal-define-key
           '("0" . meow-expand-0)
           '("9" . meow-expand-9)
           '("8" . meow-expand-8)
           '("7" . meow-expand-7)
           '("6" . meow-expand-6)
           '("5" . meow-expand-5)
           '("4" . meow-expand-4)
           '("3" . meow-expand-3)
           '("2" . meow-expand-2)
           '("1" . meow-expand-1)
           '("-" . negative-argument)
           '(";" . meow-reverse)
           '("," . meow-inner-of-thing)
           '("." . meow-bounds-of-thing)
           '("[" . meow-beginning-of-thing)
           '("]" . meow-end-of-thing)
           '("a" . meow-append)
           '("A" . meow-open-below)
           '("b" . meow-back-word)
           '("B" . meow-back-symbol)
           '("c" . meow-change)
           '("d" . meow-delete)
           '("D" . meow-backward-delete)
           '("e" . meow-next-word)
           '("E" . meow-next-symbol)
           '("f" . meow-find)
           '("g" . meow-cancel-selection)
           '("G" . meow-grab)
           '("h" . meow-left)
           '("H" . meow-left-expand)
           '("i" . meow-insert)
           '("I" . meow-open-above)
           '("j" . meow-next)
           '("J" . meow-next-expand)
           '("k" . meow-prev)
           '("K" . meow-prev-expand)
           '("l" . meow-right)
           '("L" . meow-right-expand)
           '("m" . meow-join)
           '("n" . meow-search)
           '("o" . meow-block)
           '("O" . meow-to-block)
           '("p" . meow-yank)
           '("q" . meow-quit)
           '("Q" . meow-goto-line)
           '("r" . meow-replace)
           '("R" . meow-swap-grab)
           '("s" . meow-kill)
           '("t" . meow-till)
           '("u" . meow-undo)
           '("U" . meow-undo-in-selection)
           '("v" . meow-visit)
           '("w" . meow-mark-word)
           '("W" . meow-mark-symbol)
           '("x" . meow-line)
           '("X" . meow-goto-line)
           '("y" . meow-save)
           '("Y" . meow-sync-grab)
           '("z" . meow-pop-selection)
           '("'" . repeat)
           '("<escape>" . ignore)))
  :config (progn ()
                 (meow-setup)
                 (meow-global-mode 1)))

(use-package gptel
  :config (setq gptel-log-level 'debug
                gptel-model 'qwen3.5:9b
                gptel-backend (gptel-make-ollama "Ollama"
                                :host "localhost:11434"
                                :stream nil
                                :models '(qwen3.5:9b :capabilities (tool_use)))))

(use-package gptel-agent
  :after gptel
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
                    :font "DejaVu Sans"
                    :height 180)
(set-face-attribute 'variable-pitch nil
                    :font "Geneva"
                    :height 180)
