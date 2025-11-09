;; package manager 'elpaca'の設定
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1 :inherit ignore
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-packageでensureでパッケージをインストールする時にelpacaを利用する様にする
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; keybind
(setq inhibit-startup-message t)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'undo-redo)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-c C-c") 'scroll-down-command)

;; toolbarを非表示にする
(tool-bar-mode -1)

;; yes noで答えるのを y nにする
(fset 'yes-or-no-p 'y-or-n-p)

;; スクロールの設定
(setq scroll-conservatively 50)

;; 現在行にハイライトを設定
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "grey20")))))

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

;; I-searchを強化する
(use-package consult
  :ensure
  :bind (
	 ("C-s" . consult-line)
	 ("C-x b" . consult-buffer)
	 ("M-g g" . consult-goto-line)
	 ("C-x C-b" . consult-recent-file)
	 ("C-x C-i" . consult-projectile)))

;; ミニバッファのUIと操作を拡張する
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
  :ensure
  :config
  (define-key mc/keymap (kbd "C-h") 'delete-backward-char)
  (define-key global-map (kbd "C-x C-a") 'mc/mark-all-like-this)
  (define-key global-map (kbd "C-x C-d") 'mc/mark-all-like-this-in-defun)
  (define-key global-map (kbd "C-x C-e") 'mc/edit-ends-of-lines)
  (define-key global-map (kbd "C-x C-r") 'mc/mark-all-in-region-regexp))

(use-package vterm
    :ensure t
    :bind (("C-c t" . 'vterm))
    :config
    ;; 文字エンコーディングの設定
    (setq vterm-environment '("LANG=en_US.UTF-8" "LC_ALL=en_US.UTF-8" "TERM=xterm-256color"))
    ;; シェルの設定を明示的に指定
    (setq vterm-shell (getenv "SHELL"))
    ;; バッファサイズを大きくする
    (setq vterm-buffer-name-string "vterm %s")
    ;; ターミナルの最大行数
    (setq vterm-max-scrollback 10000)
    ;; 256色サポート
    (setq vterm-term-environment-variable "xterm-256color")
    ;; vterm内でもC-hをバックスペースとして使用
    (define-key vterm-mode-map (kbd "C-h") 'vterm-send-backspace)
    ;; vterm内で行番号を非表示にする
    (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package transient
  :ensure
  :config
  ;; transient内でもC-hをバックスペースとして使用
  (define-key transient-base-map (kbd "C-h") 'backward-delete-char)
  (define-key transient-sticky-map (kbd "C-h") 'backward-delete-char))

(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :defer t
  :config
  (claude-code-ide-emacs-tools-setup))

;; ホットリロード的なことをする
(use-package autorevert
  :init
  (setq auto-revert-mode-text "Auto-Reload")
  :config
  (global-auto-revert-mode t))

;; 行数表示
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode)
  :custom
  (cursor-type 'bar))

;; ディレクトリを左サイドバーに表示する
;; ディレクトリを表示する
(use-package treemacs
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

;; プロジェクト内検索を便利にする
(use-package projectile
  :ensure
  :init
  (projectile-mode))

;; consultとprojectileを連携させる
(use-package consult-projectile
  :ensure
  :after (consult projectile))

;; テーマ
(use-package doom-themes
  :ensure
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; アイコン（doom-modelineに必要）
(use-package nerd-icons
  :ensure)

;; モードライン
(use-package doom-modeline
  :ensure
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-icon t))

;; 括弧の色分け
(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

