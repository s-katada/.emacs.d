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

;; エレクトリックインデントを有効にする
(electric-pair-mode t)

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
  (setq vertico-count 30)
  ;; ミニバッファでのキーバインド設定
  :bind ((:map minibuffer-local-map
               ("C-w" . backward-kill-word))  ;; 単語単位で削除
         (:map vertico-map
               ("C-w" . vertico-directory-delete-word)))  ;; パス区切りで削除
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))  ;; 自動的にパスを整理

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
  ;; 起動時のみ最大化（既に最大化されている場合はスキップ）
  (unless (frame-parameter nil 'fullscreen)
    (toggle-frame-maximized))
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

;; コード補完UI
(use-package company
  :ensure
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.1)  ; 0.1秒後に補完候補を表示
  (setq company-minimum-prefix-length 2)  ; 2文字入力後に補完開始
  (setq company-selection-wrap-around t)  ; 候補の最後から最初に戻る
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-h" . delete-backward-char)))  ; C-hをバックスペースに

;; tree-sitter文法のインストール
(use-package treesit
  :ensure nil  ; 組み込みパッケージ
  :config
  ;; tree-sitter文法の自動インストール
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
          (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2" "src")))

  ;; 文法がインストールされていない場合は自動インストール
  (dolist (lang '(typescript tsx dockerfile rust))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;; eglot (LSPクライアント)
(use-package eglot
  :ensure nil  ; 組み込みパッケージ (Emacs 29+)
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
  ;; エラー・警告の表示間隔を調整
  (setq eglot-events-buffer-size 0)  ; イベントログを無効化（軽量化）
  (setq eglot-autoshutdown t)  ; バッファを閉じたらサーバーを自動停止

  ;; サポートされていない機能の警告を抑制
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))

  ;; Markdown用言語サーバーの設定
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("marksman" "server")))

  ;; Dockerfile用言語サーバーの設定
  (add-to-list 'eglot-server-programs
               '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))

  ;; YAML用言語サーバーの設定
  (add-to-list 'eglot-server-programs
               '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(yaml-ts-mode . ("yaml-language-server" "--stdio")))

  ;; Rust用言語サーバーの設定
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"))))

;; TypeScript/TSXファイルの自動認識
(use-package typescript-ts-mode
  :ensure nil  ; 組み込みパッケージ (Emacs 29+)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;; Markdown
(use-package markdown-mode
  :ensure
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "marked"))  ; プレビュー用コマンド（オプション）


;; Dockerfile
(use-package dockerfile-ts-mode
  :ensure nil
  :mode (("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)))

;; YAML
(use-package yaml-mode
  :ensure
  :mode (("\\.ya?ml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Rust
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :config
  (setq rust-ts-mode-indent-offset 2))

;; いつかちゃんと実装したい
;; LLM抽象化レイヤー
;; (use-package llm
;;   :ensure (:host github :repo "ahyatt/llm")
;;   :init
;;   (require 'llm-ollama))

;; ;; Copilot風自動インライン補完（Qwen使用）
;; (defvar qwen-completion-overlay nil
;;   "補完候補を表示するためのオーバーレイ")

;; (defvar qwen-completion-timer nil
;;   "補完候補を自動取得するためのタイマー")

;; (defvar qwen-completion-delay 0.5
;;   "入力停止後、補完候補を取得するまでの遅延（秒）")

;; (defvar qwen-last-point nil
;;   "最後に補完を実行したポイント")

;; (defvar qwen-completion-active nil
;;   "補完リクエストが進行中かどうか")

;; (defun qwen-clear-completion ()
;;   "補完候補をクリア"
;;   (when qwen-completion-overlay
;;     (delete-overlay qwen-completion-overlay)
;;     (setq qwen-completion-overlay nil)))

;; (defun qwen-inline-complete-auto ()
;;   "Qwenを使って自動的にインライン補完を実行"
;;   (when (and (not qwen-completion-active)
;;              (eq (current-buffer) (window-buffer (selected-window))))
;;     (let* ((current-buffer (current-buffer))
;;            (current-point (point))
;;            (context-start (max (point-min) (- (point) 2000)))
;;            (prefix (buffer-substring-no-properties context-start current-point))
;;            (suffix (buffer-substring-no-properties
;;                     current-point
;;                     (min (point-max) (+ current-point 500))))
;;            ;; Qwen2.5-CoderのFIM形式
;;            (prompt (format "<|fim_prefix|>%s<|fim_suffix|>%s<|fim_middle|>"
;;                            prefix suffix)))

;;       ;; 既存の補完をクリア
;;       (qwen-clear-completion)

;;       (setq qwen-completion-active t)
;;       (setq qwen-last-point current-point)

;;       ;; Ollamaに非同期リクエスト
;;       (llm-chat-async
;;        (make-llm-ollama
;;         :chat-model "qwen2.5-coder:7b"
;;         :default-chat-non-standard-params '(("num_ctx" . 8192)
;;                                              ("temperature" . 0.2)
;;                                              ("top_p" . 0.95)))
;;        (llm-make-simple-chat-prompt prompt)
;;        (lambda (response)
;;          (setq qwen-completion-active nil)
;;          (with-current-buffer current-buffer
;;            (when (and response
;;                       (not (string-empty-p response))
;;                       (eq (point) current-point)
;;                       (eq (current-buffer) (window-buffer (selected-window))))
;;              (let ((completion (string-trim response)))
;;                ;; オーバーレイを作成して補完候補を表示
;;                (setq qwen-completion-overlay (make-overlay (point) (point)))
;;                (overlay-put qwen-completion-overlay
;;                             'after-string
;;                             (propertize completion 'face '(:foreground "gray50")))))))
;;        (lambda (type error-data)
;;          (setq qwen-completion-active nil))))))

;; (defun qwen-accept-completion ()
;;   "表示された補完候補を受け入れる"
;;   (interactive)
;;   (when qwen-completion-overlay
;;     (let ((completion (overlay-get qwen-completion-overlay 'after-string)))
;;       (when completion
;;         (insert (substring-no-properties completion))
;;         (qwen-clear-completion)))))

;; (defun qwen-schedule-completion ()
;;   "補完候補の取得をスケジュール"
;;   (when qwen-completion-timer
;;     (cancel-timer qwen-completion-timer))
;;   (qwen-clear-completion)
;;   (setq qwen-completion-timer
;;         (run-with-idle-timer qwen-completion-delay nil #'qwen-inline-complete-auto)))

;; (defun qwen-post-command ()
;;   "コマンド実行後のフック"
;;   (when (and qwen-copilot-mode
;;              (not (minibufferp)))
;;     (if (and qwen-completion-overlay
;;              (or (not (eq (point) qwen-last-point))
;;                  (eq this-command 'self-insert-command)))
;;         (qwen-clear-completion))
;;     (when (eq this-command 'self-insert-command)
;;       (qwen-schedule-completion))))

;; ;; グローバルマイナーモード
;; (define-minor-mode qwen-copilot-mode
;;   "Copilot風の自動インライン補完モード"
;;   :global t
;;   :lighter " QCopilot"
;;   (if qwen-copilot-mode
;;       (progn
;;         (add-hook 'post-command-hook #'qwen-post-command)
;;         (global-set-key (kbd "TAB")
;;                         (lambda ()
;;                           (interactive)
;;                           (if qwen-completion-overlay
;;                               (qwen-accept-completion)
;;                             (indent-for-tab-command)))))
;;     (remove-hook 'post-command-hook #'qwen-post-command)
;;     (qwen-clear-completion)
;;     (when qwen-completion-timer
;;       (cancel-timer qwen-completion-timer))))

;; ;; デフォルトで有効化（prog-modeのみ）
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (qwen-copilot-mode 1)))
