;;; init-ui.el --- UI and appearance settings -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, icons, and visual appearance configurations

;;; Code:

(use-package frame ; windowサイズを修正
  :config
  (unless (frame-parameter nil 'fullscreen) ; 起動時のみ最大化
    (toggle-frame-maximized))
  (set-frame-parameter nil 'alpha 85) ; 透明度を設定
  (if (>= (frame-width) 543) ; 画面幅に応じてフォントサイズを調整
      (set-face-attribute 'default (selected-frame) :height 180)))

(use-package display-line-numbers ; 行数表示
  :config
  (global-display-line-numbers-mode)
  :custom
  (cursor-type 'bar)) ; カーソルをバー型に

(use-package doom-themes ; テーマ
  :ensure
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package nerd-icons ; アイコン（doom-modelineに必要）
  :ensure)

(use-package doom-modeline ; モードライン
  :ensure
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-icon t))

(use-package rainbow-delimiters ; 括弧の色分け
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package whitespace ; 空白を見やすくする
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-normal-modes '(not emacs-lisp-mode))
  :config
  (setq whitespace-style '(face trailing))
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?\u00BB ?\t] [?\t])))
  (set-face-attribute 'whitespace-trailing nil
                      :background "red"
                      :foreground "white"
                      :weight 'bold)
  (setq whitespace-global-modes '(not org-mode))
  :diminish whitespace-mode)

(provide 'init-ui)
;;; init-ui.el ends here
