;;; init-base.el --- Basic settings and keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; Basic Emacs settings, keybindings, and core configurations

;;; Code:

(setq inhibit-startup-message t) ; スタートアップメッセージを非表示
(setq make-backup-files nil) ; バックアップファイルを作成しない
(setq auto-save-default nil) ; 自動保存ファイルを作成しない
(setq create-lockfiles nil) ; ロックファイルを作成しない
(global-set-key (kbd "C-h") 'backward-delete-char) ; キーバインド
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'undo-redo)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-c C-c") 'scroll-down-command)
(tool-bar-mode -1) ; toolbarを非表示にする
(fset 'yes-or-no-p 'y-or-n-p) ; yes noで答えるのを y nにする
(setq scroll-conservatively 50) ; スクロールの設定
(global-hl-line-mode t) ; 現在行にハイライトを設定
(custom-set-faces
 '(hl-line ((t (:background "grey20")))))
(electric-pair-mode t) ; エレクトリックインデントを有効にする
(use-package recentf ; ファイルの履歴
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 1000))

(provide 'init-base)
;;; init-base.el ends here
