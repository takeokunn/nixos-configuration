---
description: 直前の実装・変更を多角的にレビューし、品質を保証するコマンド（並列実行でタイムアウト回避）
feedback_agents:
  - name: quality
    description: コード品質レビュー（命名規則、責務分離、DRY原則、可読性）
  - name: security
    description: セキュリティレビュー（OWASP Top 10、入力検証、認証/認可）
  - name: design
    description: アーキテクチャ整合性レビュー（既存設計との整合性、依存関係）
readonly_tools:
  - name: serena
    description: シンボル分析、メモリ確認、依存関係追跡に使用
  - name: context7
    description: ライブラリのベストプラクティス確認に使用
---

# feedback - 変更レビュー専用コマンド

## 目的

直前に行った実装・変更を多角的にレビューし、品質を保証するコマンド。
**3つのサブエージェントを並列実行**することで、タイムアウトを回避しつつ効率的なレビューを実施。

## 基本方針

- **並列実行**: 3つのTaskツールを**1メッセージで同時起動**（タイムアウト回避の必須要件）
- **焦点を絞る**: 6観点ではなく3観点（quality/security/design）に限定
- **客観的判断**: 忖度なく指摘
- **具体的指摘**: 抽象論ではなく具体的修正案を提示

## レビュー実施手順

### 1. 変更内容の把握

**変更ファイルの確認**:

```bash
git diff HEAD --name-only
git status --short
```

**変更差分の確認**（必要に応じて）:

```bash
git diff HEAD
```

### 2. 並列レビューの実行（最重要）

**タイムアウト回避の必須要件**:

- 3つのTaskツールを**1メッセージで同時に呼び出す**
- 逐次実行は絶対に行わない

**具体的な実行方法**:

```python
# 正しい並列実行（1メッセージで3つ同時）
Task(subagent_type="quality", description="Code quality review",
     prompt="以下のファイルのコード品質をレビュー:\n- {変更ファイル一覧}\n\nserenaのfind_symbol、get_symbols_overviewでシンボル分析し、\n命名規則、責務分離、DRY原則、可読性を評価。\n改善提案とスコア(0-100)を報告。")

Task(subagent_type="security", description="Security review",
     prompt="以下のファイルのセキュリティをレビュー:\n- {変更ファイル一覧}\n\nOWASP Top 10、入力検証、認証/認可を確認。\n脆弱性を高/中/低で分類し、具体的な修正案を提示。")

Task(subagent_type="design", description="Architecture review",
     prompt="以下のファイルのアーキテクチャをレビュー:\n- {変更ファイル一覧}\n\nserenaのlist_memories、read_memoryで既存パターン確認。\nfind_referencing_symbolsで依存関係分析。\n設計原則からの逸脱を指摘。")
```

**❌ 誤った実行方法（タイムアウトの原因）**:

```python
# 逐次実行 - これはタイムアウトを引き起こす
Task(subagent_type="quality", ...)
# エージェント完了を待つ
Task(subagent_type="security", ...)
# エージェント完了を待つ
Task(subagent_type="design", ...)
```

### 3. 各レビュー観点の詳細

#### a) コード品質 (quality)

**チェック項目**:

- 命名規則、責務分離、DRY原則
- 可読性、不要コメント・デッドコード

**使用ツール**:

- `serena`: `find_symbol`, `get_symbols_overview`でシンボル分析
- `context7`: フレームワークのベストプラクティス確認

#### b) セキュリティ (security)

**チェック項目**:

- OWASP Top 10（SQLi, XSS, CSRF等）
- 入力検証、認証/認可
- 機密情報の扱い（ハードコード、ログ出力）

**使用ツール**:

- `serena`: `find_referencing_symbols`で認証/認可フロー追跡
- `grep`: シークレット情報のハードコード検索

#### c) アーキテクチャ (design)

**チェック項目**:

- 既存設計との整合性
- 依存関係の方向性
- Serenaメモリに記録されたパターン遵守

**使用ツール**:

- `serena`: `list_memories`, `read_memory`で既存パターン確認
- `serena`: `find_referencing_symbols`で依存関係分析

### 4. レビュー結果の統合

各サブエージェントからの結果を統合し、以下の形式で報告:

```markdown
# 📊 レビュー結果

## 📈 評価スコア

- コード品質: XX/100
- セキュリティ: XX/100
- アーキテクチャ: XX/100

**総合スコア: XX/100**

## 🔴 Critical（即座に修正が必要）

- [セキュリティ] SQLインジェクションの脆弱性: `UserController.login()` L42
  - **問題**: ユーザー入力を直接クエリに埋め込み
  - **修正案**: プリペアドステートメントを使用

## 🟡 Warning（修正を推奨）

- [品質] 過度に長い関数: `DataProcessor.transform()` L156-284
  - **問題**: 128行の関数で責務が混在
  - **推奨**: 5つの独立関数に分割

## 🟢 Good Practice（良い実装）

- [アーキテクチャ] 依存性注入パターンの適切な使用
- [品質] 適切なエラーハンドリング

## 📝 推奨アクション（優先順位順）

1. [優先度: 高] SQLインジェクション脆弱性の修正（`UserController.login`）
2. [優先度: 中] `DataProcessor.transform`の関数分割
3. [優先度: 低] 変数名の統一（snake_case vs camelCase混在）
```

## 重要な制約

- **タイムアウト回避**: 3つのTaskを**1メッセージで同時起動**（逐次実行禁止）
- **焦点を絞る**: 6観点ではなく3観点に限定（quality/security/design）
- **具体的指摘**: 抽象論ではなく具体的修正案
- **忖度排除**: 品質優先
- **Serena活用**: promptに明示

## パラメータ

- `対象`: レビュー対象ファイル/ディレクトリ（省略時は直前変更）
- `--focus=<項目>`: 特定観点のみ（quality/security/design）
