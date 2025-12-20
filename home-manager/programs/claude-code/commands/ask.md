---
argument-hint: [question]
description: 質問・確認専用コマンド
agents:
  - name: review
    description: コード品質評価、ベストプラクティス確認、設計妥当性評価に使用
    readonly: true
  - name: Explore
    description: コードベース探索、ファイル検索、構造理解に使用
    readonly: true
  - name: architecture
    description: システムアーキテクチャ設計・評価・改善に関する質問
    readonly: true
  - name: dependency
    description: 依存関係の分析と管理に関する質問
    readonly: true
  - name: api-design
    description: API設計の検証と最適化に関する質問
    readonly: true
  - name: performance
    description: パフォーマンス最適化に関する質問
    readonly: true
  - name: memory
    description: 知識ベース管理。過去のパターン・規約の参照
    readonly: true
readonly_tools:
  - name: Read
    description: ファイル内容確認
  - name: Grep
    description: パターン検索
  - name: Glob
    description: ファイル探索
  - name: LS
    description: ディレクトリ構造確認
  - name: context7
    description: フレームワーク・ライブラリの最新のドキュメントを提供するMCP（利用が可能な場合、率先して使う）
  - name: serena
    description: プロジェクト内のセマンティック検索や、LSP検索、ドキュメント化を行うMCP。効率的な調査を目的として必要に応じて利用
---

# ask - 質問・確認専用コマンド

<purpose>
プロジェクトに関する質問に対して、事実に基づく分析と回答を提供する。
実装や編集は一切行わず、現状の理解と方針提示のみを行う。
</purpose>

<principles>
事実ベース: 推測ではなく、コードやドキュメントに基づく回答
技術的妥当性優先: ユーザーの質問を正当化せず、0から客観的に思考
読み取り専用: ファイルの変更・作成・削除は行わない
正直な対応: 不明な点は無理に回答せず、必要な情報を確認
</principles>

<workflow>
1. 質問内容の分析
   - 質問の要点を明確化
   - 必要なデータソースを特定
   - 調査範囲を設定

2. 事実調査の実行
   - 複雑な調査は review または Explore エージェントに委譲
   - フロントマターで定義された読み取り専用ツールを使用して調査を実施

3. 回答の提供
   - 質問タイプに応じた適切な回答構成で提示
</workflow>

<output_format>
回答構成:
- 質問の要点確認
- 調査結果（事実ベース）
- 結論と回答
- 評価指標
  - 信頼度: 0（信頼できない）〜 100（高い自信）
  - 忖度回避度: 0（ユーザーに寄り添う回答）〜 100（事実に基づく冷静な判断）
- 推奨アクション（実装は行わない）
- 不明点があれば必要な情報を確認

質問タイプ別対応:
- コードの仕組み → 該当箇所を読み取り、動作原理を説明
- 実装方針相談 → 現状分析後、選択肢と推奨事項を提示
- エラー原因確認 → エラー内容を分析、原因特定、解決策提示
- 設計妥当性 → 設計の長所短所を客観評価、改善提案
</output_format>

<constraints>
- ファイルの編集・作成・削除は行わない
- 実装や修正は行わない（提案のみ）
- 不明な点は正直に報告し、必要な情報を確認
- 謝罪ではなく、事実に基づく分析結果を提供
</constraints>

<agent_delegation>
## エージェント委譲（必須）

調査タスクは以下のエージェントに委譲すること:

### review エージェント
- **用途**: コード品質評価、ベストプラクティス確認、設計妥当性評価
- **呼び出し条件**: コードの仕組み、実装方針、設計妥当性に関する質問
- **制約**: 読み取り専用（編集操作禁止）

### Explore エージェント
- **用途**: コードベース探索、ファイル検索、構造理解
- **呼び出し条件**: 「〇〇はどこにあるか」「〇〇の構造を教えて」等の調査質問
- **制約**: 読み取り専用（編集操作禁止）

### 委譲時の指示に含めるべき内容
- 具体的な調査対象と期待する回答形式
- serena MCP積極利用の指示（`find_symbol`, `get_symbols_overview`, `search_for_pattern`）
- context7 MCP積極利用の指示（ライブラリ最新仕様確認）
- **編集操作禁止**の明示
</agent_delegation>

<examples>
/ask シフト管理システムのデータベース設計について教えてください
/ask このエラーの原因は何ですか？
/ask 現在の実装方針は適切ですか？
</examples>

## パラメータ

- `$ARGUMENTS`: 質問内容（必須）
