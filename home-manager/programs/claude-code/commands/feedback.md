---
description: Claude Codeが直前に対応した内容をレビューするコマンド
modes:
  - name: define
    description: /define後の実行計画を網羅的にレビュー
    agents:
      - name: plan
        description: 実行計画レビュー（ステップ粒度、依存関係、リスク識別、完全性、実行可能性）
      - name: estimation
        description: 見積もり妥当性レビュー（タスク粒度、リソース配分）
  - name: execute
    description: /execute後の作業内容を網羅的にレビュー
    agents:
      - name: quality
        description: コード品質レビュー（命名規則、責務分離、DRY原則、可読性）
      - name: security
        description: セキュリティレビュー（OWASP Top 10、入力検証、認証/認可）
      - name: design
        description: アーキテクチャ整合性レビュー（既存設計との整合性、依存関係）
      - name: docs
        description: ドキュメント品質レビュー（正確性、可読性、網羅性）
      - name: performance
        description: パフォーマンスレビュー（効率性、最適化の余地）
      - name: test
        description: テストカバレッジレビュー（テスト網羅性、品質）
      - name: accessibility
        description: アクセシビリティレビュー（WCAG準拠性）
      - name: error-handling
        description: エラー処理レビュー（例外処理の適切性）
  - name: general
    description: 直前のClaude Code作業内容をレビュー
    agents:
      - name: review
        description: 直前作業の網羅的レビュー
      - name: complexity
        description: コード複雑度レビュー（循環的複雑度、認知的複雑度）
      - name: memory
        description: 知識ベース管理。既存パターン・規約との整合性確認
readonly_tools:
  - name: serena
    description: シンボル分析、メモリ確認、依存関係追跡に使用
  - name: context7
    description: ライブラリのベストプラクティス確認に使用
---

# feedback - レビューコマンド

<purpose>
同一セッション内でClaude Codeが対応した内容を多角的にレビュー。
直前のコマンドに応じて適切なレビューモードを自動選択し、並列実行で効率的にフィードバックを実施。
</purpose>

<mode_selection>
会話履歴から直前のコマンドを判定してモードを選択:

| 判定条件 | モード | 説明 |
|---------|--------|------|
| `/define`コマンドが直前に実行された | defineモード | 実行計画を網羅的にフィードバック |
| `/execute`コマンドが直前に実行された | executeモード | 作業内容を網羅的にフィードバック |
| その他（上記以外の作業） | generalモード | 直前に行ったClaude Codeの作業をフィードバック |
</mode_selection>

<execution>
## 並列実行（タイムアウト回避の必須要件）

**重要**: Taskツールを**1メッセージで同時起動**。逐次実行は絶対に行わない。

## defineモード（1エージェント）

**対象**: 会話履歴から実行計画（ステップ分解）を抽出

**レビュー観点**:
- ステップ粒度: 適切な大きさに分解されているか
- 依存関係: 順序・並列化が適切か
- リスク識別: 技術的課題の見落としがないか
- 完全性: 全ての要件をカバーしているか
- 実行可能性: 各ステップが実行可能か
- 網羅性: 必要な作業が漏れていないか
- 明確性: 各ステップの内容が具体的か

**使用ツール**:
- `serena`: `find_symbol`, `get_symbols_overview`で既存コード構造確認
- `serena`: `find_referencing_symbols`で依存関係分析
- `serena`: `list_memories`, `read_memory`で既存パターン・規約確認

## executeモード（4エージェント並列）

**対象特定**:
1. 会話履歴からEdit/Writeツールの対象ファイルを抽出
2. フォールバック: git statusで未コミット変更を検出
3. 不明な場合: ユーザーに確認

**重要**: 変更されたコードのみをレビュー対象とし、既存コードの品質問題は指摘しない

### a) コード品質 (quality)

**観点**: 命名規則、責務分離、DRY原則、可読性、不要コメント・デッドコード

**使用ツール**:
- `serena`: `find_symbol`, `get_symbols_overview`でシンボル分析
- `context7`: フレームワークのベストプラクティス確認

### b) セキュリティ (security)

**観点**: OWASP Top 10（SQLi, XSS, CSRF等）、入力検証、認証/認可、機密情報の扱い

**使用ツール**:
- `serena`: `find_referencing_symbols`で認証/認可フロー追跡
- `grep`: シークレット情報のハードコード検索

### c) アーキテクチャ (design)

**観点**: 既存設計との整合性、依存関係の方向性、Serenaメモリに記録されたパターン遵守

**使用ツール**:
- `serena`: `list_memories`, `read_memory`で既存パターン確認
- `serena`: `find_referencing_symbols`で依存関係分析

### d) ドキュメント (docs)

**観点**:
- 正確性・整合性: コードとの乖離、参照リンク有効性
- 可読性・構成: 見出し階層、Markdown構文、フォーマット統一
- 網羅性: 必要セクション、パラメータ説明の完全性

**使用ツール**:
- `Read`: .mdファイル内容確認
- `Grep`: コードとの整合性確認
- `context7`: ドキュメントベストプラクティス確認

## generalモード（1エージェント）

**対象**: 会話履歴から直前のClaude Code作業内容を特定

**作業種別に応じたレビュー観点**:

| 作業種別 | チェック観点 |
|---------|-------------|
| 調査・分析 | 調査範囲の網羅性、分析の正確性、情報の整理 |
| ドキュメント作成 | 正確性、可読性、網羅性 |
| コード生成・修正 | 品質、セキュリティ、整合性 |
| その他 | 目的達成度、品質、改善点 |

**使用ツール**:
- `serena`: `list_memories`, `read_memory`で既存パターン・規約確認
- 作業内容に応じて適切なツールを選択
</execution>

<output_format>
各サブエージェントからの結果を統合し、以下の形式で報告:

```markdown
# 📊 {モード名} フィードバック結果

## 📈 評価スコア

- {評価項目1}: XX/100
- {評価項目2}: XX/100
...

**総合スコア: XX/100**

## 🔴 Critical（即座に修正が必要）

- [カテゴリ] 問題の詳細: 場所
  - **問題**: 問題の説明
  - **修正案**: 具体的な修正提案

## 🟡 Warning（修正を推奨）

- [カテゴリ] 改善点の詳細: 場所
  - **問題**: 問題の説明
  - **推奨**: 改善提案

## 🟢 Good Practice（良い実装/作業）

- [カテゴリ] 評価できる点

## 📝 推奨アクション（優先順位順）

1. [優先度: 高] 最優先の改善点
2. [優先度: 中] 次の改善点
3. [優先度: 低] 余裕があれば対応
```

### 各モード固有の評価項目

**defineモード**:
- 実行計画品質: XX/100

**executeモード**:
- コード品質: XX/100
- セキュリティ: XX/100
- アーキテクチャ: XX/100
- ドキュメント: XX/100

**generalモード**:
- 作業品質: XX/100
</output_format>

<constraints>
- **タイムアウト回避**: Taskを**1メッセージで同時起動**（逐次実行禁止）
- **コンテキスト判定**: 直前のコマンド（/define、/execute、その他）に応じてモードを自動選択
- **具体的指摘**: 抽象論ではなく具体的修正案を提示
- **忖度排除**: 品質優先
- **Serena活用**: promptに明示
- **セッションベース**: git diffではなく、セッション内のClaude Code操作を対象
- **変更箇所限定**: executeモードでは変更されたコードのみを評価対象とし、既存コードの品質問題は指摘しない
</constraints>

## パラメータ

- `対象`: レビュー対象（省略時はセッション内の変更を自動検出）
- `--mode=<モード>`: 明示的にモードを指定（define/execute/general）
