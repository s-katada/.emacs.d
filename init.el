;; パッケージマネージャーのアルパカをインストール
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  ;;Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(elpaca-wait)
;; package.elを使わない
(setq package-enable-at-startup nil)

;; welcome表示を削除
(setq inhibit-startup-message t)

;; macではlsに--diredオプションがないので削除する
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; yes noで答えるのを y nにする
(fset 'yes-or-no-p 'y-or-n-p)

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; 現在行にハイライトを設定
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "grey20")))))

;; キーバインド
(bind-key "C-h" 'backward-delete-char)
(bind-key "s-z" 'undo)
(bind-key "s-Z" 'undo-redo)
(bind-key "C-;" 'comment-line)
(bind-key "C-c C-c" 'scroll-down-command)

;; 画面上部のツールバーを削除
(use-package tool-bar
  :if (display-graphic-p)
  :config
  (tool-bar-mode -1))

;; テーマ
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

;; modeline
(use-package doom-modeline
  :ensure
  :config
  ;; 表示する情報を設定する
  (setq doom-modeline-height 20
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-modal-icon t
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-max-length 40
        doom-modeline-env-version t
        doom-modeline-env-enable-ruby t
        doom-modeline-check-simple-format t
        doom-modeline-indent-info t)
  (set-face-attribute 'mode-line nil
                      :background "#4b0082"
                      :foreground "white")
  (set-face-attribute 'mode-line-inactive nil
                      :background "black"
                      :foreground "gray"))

;; ディレクトリを表示する
(use-package treemacs
  :ensure t
  :defer t
  :bind
  ("s-b" . treemacs)
  :custom
  (treemacs-width 50)
  :config
  (progn
    (setq treemacs-follow-mode t)
    (setq treemacs-filewatch-mode t)
    (setq treemacs-fringe-indicator-mode 'always)
    (setq treemacs-show-cursor t)
    (setq treemacs-show-hidden-files t)
    (setq treemacs-silent-filewatch 'post-command-hook))
  (treemacs-git-mode 'extended))

;; 空白を見やすくする
(use-package whitespace
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

;; ホットリロード的なことをする
(use-package autorevert
  :init
  (setq auto-revert-mode-text "Auto-Reload")
  :config
  (global-auto-revert-mode t))

;; スクロールをなめらかにする
(use-package smooth-scrolling
  :ensure
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

;; 行数表示
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode)
  :custom
  (cursor-type 'bar))

;; 補完システム
(use-package vertico
  :ensure
  :init
  (vertico-mode)
  :config
  (setq vertico-count 30))

;; Orderlessの設定
(use-package orderless
  :ensure
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; I-searchを強化する
(use-package consult
  :ensure t
  :bind (
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)
	 ("M-g g" . consult-goto-line)
	 ("C-x C-b" . consult-recent-file)
	 ("C-x C-i" . consult-projectile)
	 )
  :config
  ;; 一部のコマンドを自動的に`consult`に置き換える
  (setq consult-async-min-input 2)  ;; 非同期検索の最小入力数
  )

;; プロジェクト内検索を便利にする
(use-package projectile
  :ensure t
  :init
  (projectile-mode))

;; consultとprojectileを連携させる
(use-package consult-projectile
  :ensure t
  :after (consult projectile))

;; 補完候補に説明文を付与する
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; ファイルの履歴
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 1000))

;; windowサイズを修正
(use-package frame
  :config
  (toggle-frame-maximized)
  (set-frame-parameter nil 'alpha 85)
  (if (>= (frame-width) 543)
      (set-face-attribute 'default (selected-frame) :height 180)))

;; バッファの切り替え
(use-package ace-window
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

;; 一気に編集できるようにする
(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "C-h") 'delete-backward-char)
  (define-key global-map (kbd "C-x C-a") 'mc/mark-all-like-this)
  (define-key global-map (kbd "C-x C-d") 'mc/mark-all-like-this-in-defun)
  (define-key global-map (kbd "C-x C-e") 'mc/edit-ends-of-lines)
  (define-key global-map (kbd "C-x C-r") 'mc/mark-all-in-region-regexp))

;; ruby
(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
	 ("\\.ruby\\'" . ruby-mode))
  :custom
  (lsp-solargraph-use-bundler nil)
  (lsp-solargraph-extra-options '("--plugin" "rubocop")))

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-script-padding 2
        web-mode-block-padding 2
        web-mode-comment-style 2
	web-mode-style-padding 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-indentation t
	web-mode-enable-auto-quoting nil)
  (add-hook 'web-mode-hook
            (lambda ()
	      (setq web-mode-enable-auto-indentation nil)
              (setq-local indent-tabs-mode nil))))

;; typescript-mode
(use-package typescript-mode
  :ensure t)

;; lspの設定
(use-package lsp-mode
  :ensure t
  :hook
  ((ruby-mode . lsp)
   (web-mode . lsp))
  :config
  ;; LSPのフォーマット機能を無効にする
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ("C-x C-d" . lsp-ui-doc-glance))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-border "cyan")
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-alignment 'frame)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 80)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-delay 1.0)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-sideline-enable nil)
  :bind (:map lsp-ui-mode-map
	      ("M-." . lsp-ui-peek-find-definitions)
	      ("M-?" . lsp-ui-peek-find-references)
	      ("C-." . lsp-ui-peek-jump-forward)
	      ("C-," . lsp-ui-peek-jump-backward)))

;; lsp-modeが依存するパッケージ
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; 補完候補表示
(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-dabbrev-downcase nil)
  :bind
  (:map company-active-map
        ("C-h" . 'backward-delete-char)))

(use-package prog-mode
  :hook(prog-mode . copilot-mode))

;; copilotの設定
(use-package copilot
  :ensure t
  :init
  (defun my-tab ()
    (interactive "*")
    (or (copilot-accept-completion)
	(company-indent-or-complete-common nil)))
  (bind-key "TAB" 'my-tab)
  :bind(("M-]" . copilot-next-completion)
	("M-[" . copilot-previous-completion))
  :config
  (setq copilot-indent-offset-warning-disable t))
