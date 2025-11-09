;;; init-completion.el --- Completion frameworks -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico, Consult, Orderless, and Company configurations

;;; Code:

(use-package vertico ; ミニバッファのUIと操作を拡張
  :ensure
  :init
  (vertico-mode)
  :config
  (setq vertico-count 30) ; 表示する候補数
  :bind ((:map minibuffer-local-map
               ("C-w" . backward-kill-word)) ; 単語単位で削除
         (:map vertico-map
               ("C-w" . vertico-directory-delete-word))) ; パス区切りで削除
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)) ; 自動的にパスを整理

(use-package orderless ; 柔軟な補完スタイル
  :ensure
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult ; I-searchを強化
  :ensure
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-x C-b" . consult-recent-file)
         ("C-x C-i" . consult-projectile)))

(use-package company ; コード補完UI
  :ensure
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.1) ; 0.1秒後に補完候補を表示
  (setq company-minimum-prefix-length 2) ; 2文字入力後に補完開始
  (setq company-selection-wrap-around t) ; 候補の最後から最初に戻る
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-h" . delete-backward-char))) ; C-hをバックスペースに

(provide 'init-completion)
;;; init-completion.el ends here
