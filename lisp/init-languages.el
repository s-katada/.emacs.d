;;; init-languages.el --- Language-specific configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Major modes and settings for specific programming languages

;;; Code:

(use-package typescript-ts-mode ; TypeScript/TSX
  :ensure nil ; 組み込みパッケージ (Emacs 29+)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2)) ; インデント幅を2に

(use-package markdown-mode ; Markdown
  :ensure
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "marked")) ; プレビュー用コマンド

(use-package dockerfile-ts-mode ; Dockerfile
  :ensure nil
  :mode (("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)))

(use-package yaml-mode ; YAML
  :ensure
  :mode (("\\.ya?ml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package rust-ts-mode ; Rust
  :ensure nil
  :mode "\\.rs\\'"
  :config
  (setq rust-ts-mode-indent-offset 2)) ; インデント幅を2に

(provide 'init-languages)
;;; init-languages.el ends here
