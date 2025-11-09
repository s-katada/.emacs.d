;;; init-navigation.el --- Navigation and project management -*- lexical-binding: t -*-

;;; Commentary:
;; Treemacs, Projectile, Ace-window configurations

;;; Code:

(use-package ace-window ; ウィンドウ切り替え
  :ensure t
  :bind (("C-x C-o" . ace-window))
  :init
  (setq aw-dispatch-always nil)
  (setq aw-dispatch-alist
        '((?x aw-delete-window " Ace - Delete Window")
          (?m delete-other-windows " Ace - Delete Other Windows")
          (?b balance-windows " Ace - Balance Windows")
          (?s ace-swap-window " Ace - Swap Window")
          (?n aw-flip-window)
          (?i aw-swap-iw " Ace - Swap with Ace-Window")
          (?o delete-other-windows)
          (?? aw-show-dispatch-help))))

(use-package treemacs ; ファイルツリー表示
  :ensure
  :defer
  :bind
  ("s-b" . treemacs)
  :custom
  (treemacs-width 50)
  :config
  (progn
    (setq treemacs-follow-mode t)
    (setq treemacs-filewatch-mode t)
    (setq treemacs-fringe-indicator-mode 'always)
    (setq treemacs-show-hidden-files t)
    (setq treemacs-silent-filewatch 'post-command-hook))
  (treemacs-git-mode 'extended))

(use-package projectile ; プロジェクト管理
  :ensure
  :init
  (projectile-mode))

(use-package consult-projectile ; consultとprojectileを連携
  :ensure
  :after (consult projectile))

(provide 'init-navigation)
;;; init-navigation.el ends here
