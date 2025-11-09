# Emacs Configuration

モジュール化されたEmacs設定ファイル

## 構成

```
~/.emacs.d/
├── init.el              # エントリーポイント
└── lisp/
    ├── init-base.el     # 基本設定・キーバインド
    ├── init-ui.el       # UI・テーマ
    ├── init-completion.el  # 補完システム
    ├── init-editor.el   # エディタ機能
    ├── init-navigation.el  # ナビゲーション
    ├── init-lsp.el      # LSP設定
    ├── init-languages.el   # 言語別設定
    └── init-tools.el    # ツール
```

## 依存関係

### 必須

- **Emacs 29+** (tree-sitter, eglotの組み込みサポートが必要)
- **Git** (パッケージマネージャelpacaのインストールに必要)

### 推奨パッケージ

#### LSP (eglot用)

各言語のLSPサーバーをインストールすることで、コード補完・診断が動作します：

**TypeScript/JavaScript:**
```bash
npm install -g typescript typescript-language-server
```

**Rust:**
```bash
rustup component add rust-analyzer
```

**Ruby:**
```bash
gem install solargraph
```

**Markdown:**
```bash
# Homebrew (macOS)
brew install marksman

# または直接ダウンロード
# https://github.com/artempyanykh/marksman/releases
```

**YAML:**
```bash
npm install -g yaml-language-server
```

**Dockerfile:**
```bash
npm install -g dockerfile-language-server-nodejs
```

#### vterm用

**CMake** (vterm-moduleのビルドに必要):
```bash
brew install cmake
```
