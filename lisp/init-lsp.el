;;; init-lsp.el --- LSP and tree-sitter configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot (LSP client) and tree-sitter setup

;;; Code:

(use-package treesit ; tree-sitter文法の自動インストール
  :ensure nil ; 組み込みパッケージ
  :config
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
          (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2" "src")))
  (dolist (lang '(typescript tsx dockerfile rust)) ; 未インストールの文法を自動インストール
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(use-package eglot ; LSPクライアント
  :ensure nil ; 組み込みパッケージ (Emacs 29+)
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (ruby-ts-mode . eglot-ensure)
         (markdown-mode . eglot-ensure)
         (dockerfile-ts-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :config
  (setq eglot-events-buffer-size 0) ; イベントログを無効化（軽量化）
  (setq eglot-autoshutdown t) ; バッファを閉じたらサーバーを自動停止
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)) ; 警告を抑制
  (add-to-list 'eglot-server-programs ; Markdown
               '(markdown-mode . ("marksman" "server")))
  (add-to-list 'eglot-server-programs ; Dockerfile
               '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs ; YAML
               '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(yaml-ts-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs ; Rust
               '(rust-ts-mode . ("rust-analyzer"))))

(provide 'init-lsp)
;;; init-lsp.el ends here
