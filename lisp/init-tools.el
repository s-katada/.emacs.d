;;; init-tools.el --- Development tools -*- lexical-binding: t -*-

;;; Commentary:
;; Terminal emulator, AI assistant, and other development tools

;;; Code:

(use-package vterm ; 高性能ターミナルエミュレータ
  :ensure t
  :bind (("C-c t" . 'vterm))
  :config
  (setq vterm-environment '("LANG=en_US.UTF-8" "LC_ALL=en_US.UTF-8" "TERM=xterm-256color")) ; 文字エンコーディング
  (setq vterm-shell (getenv "SHELL")) ; シェルを明示的に指定
  (setq vterm-buffer-name-string "vterm %s") ; バッファ名
  (setq vterm-max-scrollback 10000) ; スクロールバック行数
  (setq vterm-term-environment-variable "xterm-256color") ; 256色サポート
  (define-key vterm-mode-map (kbd "C-h") 'vterm-send-backspace) ; C-hをバックスペースに
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))) ; 行番号を非表示

(use-package claude-code-ide ; Claude Code IDE連携
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :defer t
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'init-tools)
;;; init-tools.el ends here
