;;; init-editor.el --- Editor features and enhancements -*- lexical-binding: t -*-

;;; Commentary:
;; Multiple cursors, auto-revert, and other editing enhancements

;;; Code:

(use-package multiple-cursors ; 複数カーソルで一気に編集
  :ensure
  :config
  (define-key mc/keymap (kbd "C-h") 'delete-backward-char)
  (define-key global-map (kbd "C-x C-a") 'mc/mark-all-like-this)
  (define-key global-map (kbd "C-x C-d") 'mc/mark-all-like-this-in-defun)
  (define-key global-map (kbd "C-x C-e") 'mc/edit-ends-of-lines)
  (define-key global-map (kbd "C-x C-r") 'mc/mark-all-in-region-regexp))

(use-package autorevert ; ファイルの自動リロード
  :init
  (setq auto-revert-mode-text "Auto-Reload")
  :config
  (global-auto-revert-mode t))

(use-package transient ; transientメニューの設定
  :ensure
  :config
  (define-key transient-base-map (kbd "C-h") 'backward-delete-char) ; C-hをバックスペースに
  (define-key transient-sticky-map (kbd "C-h") 'backward-delete-char))

(provide 'init-editor)
;;; init-editor.el ends here
