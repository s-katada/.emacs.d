;;; package --- Summary:
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t);; リストの先頭にmelpaを追加するためのt
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 現在行にハイライトを設定
(global-hl-line-mode t)
(custom-set-faces
 '(hl-line ((t (:background "grey20")))))

;; キーバインド
(bind-key "C-h" 'backward-delete-char)
(bind-key "s-z" 'undo)
(bind-key "s-Z" 'undo-redo)
(bind-key "C-;" 'comment-line)
(bind-key "C-c C-c" 'scroll-down-command)

;; yes noで答えるのを y nにする
(fset 'yes-or-no-p 'y-or-n-p)

;; markdown-modeの設定
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;; mermaid-modeの設定
(use-package mermaid-mode
  :mode (("\\.mmd\\'" . mermaid-mode)
	 ("\\.md\\'" . mermaid-mode))
  :config
  (setq mermaid-mode-map
	(let ((map mermaid-mode-map))
	  (define-key map (kbd "C-c C-c") nil)
	  (define-key map (kbd "C-c C-f") nil)
	  (define-key map (kbd "C-c C-b") nil)
	  (define-key map (kbd "C-c C-r") nil)
	  (define-key map (kbd "C-c C-o") nil)
	  (define-key map (kbd "C-c C-d") nil)
	  (define-key map (kbd "C-c C-d c") 'mermaid-compile)
	  (define-key map (kbd "C-c C-d c") 'mermaid-compile)
	  (define-key map (kbd "C-c C-d f") 'mermaid-compile-file)
	  (define-key map (kbd "C-c C-d b") 'mermaid-compile-buffer)
	  (define-key map (kbd "C-c C-d r") 'mermaid-compile-region)
	  (define-key map (kbd "C-c C-d o") 'mermaid-open-browser)
	  (define-key map (kbd "C-c C-d d") 'mermaid-open-doc)
	  map)))

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'"
  :config
  (add-hook 'fish-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode nil)
	      (setq fish-indent-offset 2))))

(use-package prog-mode
  :hook(prog-mode . copilot-mode))

;; copilotの設定
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :init
  (defun my-tab ()
    (interactive "*")
    (or (copilot-accept-completion)
	(company-indent-or-complete-common nil)))
  (bind-key "TAB" 'my-tab)
  :bind(("M-]" . copilot-next-completion)
	("M-[" . copilot-previous-completion)))

;; pytonの設定
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setq elpy-rpc-backend "jedi"))

(use-package company-jedi
  :ensure t
  :init
  (add-to-list 'company-backends 'company-jedi))

(use-package python
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq python-indent-offset 2))))

;; 定期的に変えないといけない
(setq openai-key "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")

; dep key (setq openai-key "[YOUR API KEY]")
(use-package openai
  :straight (:host github :repo "emacs-openai/openai"))

(use-package chatgpt
  :straight (:host github :repo "emacs-openai/chatgpt")
  :bind
  ("C-x C-g" . chatgpt))

;; (add-hook 'window-size-change-functions 'my-resize-buffer)

(use-package codegpt
  :straight (:host github :repo "emacs-openai/codegpt"))
(use-package dall-e
  :straight (:host github :repo "emacs-openai/dall-e"))

(use-package hydra
  :ensure t)

(defhydra hydra-hoge (global-map "C-x")
  "hoge"
  ("a" mc/mark-all-like-this "選択したワードをbuffer内の全て変更")
  ("d" mc/mark-all-like-this-in-defun "選択した関数内のワードを変更")
  ("e" mc/edit-ends-of-lines "選択した範囲の末尾を修正")
  ("r" mc/mark-all-in-region-regexp "regexp"))

(use-package multiple-cursors
  :ensure t
  :config
  (define-key mc/keymap (kbd "C-h") 'delete-backward-char)
  (define-key global-map (kbd "C-x C-a") 'mc/mark-all-like-this)
  (define-key global-map (kbd "C-x C-d") 'mc/mark-all-like-this-in-defun)
  (define-key global-map (kbd "C-x C-e") 'mc/edit-ends-of-lines)
  (define-key global-map (kbd "C-x C-r") 'mc/mark-all-in-region-regexp))

(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
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
        doom-modeline-checker-simple-format t
        doom-modeline-indent-info t)
  (set-face-attribute 'mode-line nil
                      :background "#4b0082"
                      :foreground "white")
  (set-face-attribute 'mode-line-inactive nil
                      :background "black"
                      :foreground "gray"))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner "~/.emacs.d//ascii-logo.txt")
;;   (setq dashboard-items '((recents . 20)
;;                           (projects . 20)
;;                           (agenda . 20)
;;                           (registers . 20)))
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-navigator t)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-show-shortcuts nil)
;;   (setq dashboard-set-footer nil)
;;   (setq dashboard-banner-logo-title "↑ My TellPhone Number! Call Me!"))

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
    (setq treemacs-silent-filewatch 'post-command-hook)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

(use-package emacs
  :custom
  (backup-inhibited t)
  (ring-bell-function 'ignore)
  (make-backup-files nil)
  :config
  (electric-pair-mode t)
  (setq inhibit-startup-message t))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

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

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay 1.0))

(use-package flycheck-posframe
  :ensure t
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-background-color "white")
  (flycheck-posframe-border-color "black")
  (flycheck-posframe-parameters '((internal-border-width . 1)
                                  (font . "Monospace-10")
                                  (foreground-color . "#ffffff")
                                  (background-color . "#1f1f1f")
                                  (border-color . "#1f1f1f")
                                  (border-width . 1)))
  :config
  (setq flycheck-posframe-position 'window-bottom-center
	flycheck-posframe-border-width 1))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

(use-package dired
  :custom
  (dired-use-ls-dired nil)
  :hook
  (dired-mode . (lambda () (display-line-numbers-mode -1))))

(use-package frame
  :config
  (toggle-frame-maximized)
  (set-frame-parameter nil 'alpha 85)
  (if (>= (frame-width) 543)
      (set-face-attribute 'default (selected-frame) :height 180)))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode)
  :custom
  (cursor-type 'bar))

(use-package tool-bar
  :if (display-graphic-p)
  :config
  (tool-bar-mode -1))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 1000))

(use-package ivy
  :bind
  (("C-s" . swiper))
  :custom
  (ivy-use-vertual-buffers t)
  :config
  (ivy-mode t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 25))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . counsel-switch-buffer)
   ("C-x C-b" . counsel-recentf)
   ("C-x C-i" . counsel-git))
  :config
  (define-key counsel-find-file-map (kbd "C-h") 'delete-backward-char)
  (define-key counsel-find-file-map (kbd "C-b") 'counsel-up-directory)
  (define-key counsel-find-file-map (kbd "C-f") 'counsel-down-directory))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-dabbrev-downcase nil)
  (yas-global-mode 1)
  :bind
  (:map company-active-map
        ("C-h" . 'backward-delete-char)))

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

(use-package ruby-mode
  :ensure t
  :mode ("\\.rb\\'" . ruby-mode)
  :custom
  (lsp-solargraph-use-bundler nil)
  (lsp-solargraph-extra-options '("--plugin" "rubocop")))
;; この設定をonにするとymlモードでも自動整形されて嫌な感じになる
;; :config
;; ;; 自動インデントの設定
;; (add-hook 'before-save-hook (lambda ()
;;                               (when (eq major-mode 'ruby-mode)
;; 				  ;; 整形機能があまり良くないので無駄なスペースを削除するだけにしておく
;;                                 ;; (indent-region (point-min) (point-max))
;;                                 ;; (untabify (point-min) (point-max))
;;                                 (whitespace-cleanup)))))

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
  ;; 一時保存ごとにインデントが入るのが嫌な時があるのでOff
  ;; (add-hook 'before-save-hook (lambda ()
  ;; 	    (when (eq major-mode 'web-mode)
  ;; 	      (web-mode-buffer-indent))))
  (add-hook 'web-mode-hook
            (lambda ()
	      (setq web-mode-enable-auto-indentation nil)
              (setq-local indent-tabs-mode nil))))

(use-package yaml-mode
  :ensure t)

(use-package haml-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package js
  :mode (("\\.js\\'" . js-mode)
         ("\\.json\\'" . js-mode))
  :config
  (setq-default js-indent-level 2))

(use-package lsp-mode
  :hook
  ((ruby-mode . lsp)
   (web-mode . lsp)
   (typescript-mode . lsp)
   (csharp-mode . lsp))
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

;;; init.el ends here
