---
argument-hint: [task-description]
description: タスク遂行コマンド
agents:
  - name: quality
    description: コード品質保証。構文検証、型検証、フォーマット検証を実施
    readonly: false
  - name: security
    description: セキュリティ脆弱性検出。OWASP Top 10、入力検証、認証/認可を確認
    readonly: false
  - name: test
    description: テスト戦略・品質管理。テスト作成、カバレッジ改善を実施
    readonly: false
  - name: refactor
    description: リファクタリング。技術的負債解消、コード改善を実施
    readonly: false
  - name: docs
    description: ドキュメント管理。API仕様、README、変更履歴の更新
    readonly: false
  - name: review
    description: コードレビュー。変更後のコード品質評価
    readonly: false
  - name: debug
    description: デバッグ支援。実装中のエラー調査
    readonly: false
  - name: performance
    description: パフォーマンス最適化の自動分析と改善
    readonly: false
  - name: clean
    description: 不要コードの削除とデッドコード検出
    readonly: false
  - name: error-handling
    description: エラー処理パターンの検証と改善
    readonly: false
  - name: migration
    description: マイグレーション計画と実行支援
    readonly: false
  - name: i18n
    description: 国際化・多言語対応（i18n/L10n）
    readonly: false
  - name: accessibility
    description: Webアクセシビリティ（WCAG）準拠検証
    readonly: false
  - name: database
    description: データベース設計・クエリ最適化・スキーマ管理
    readonly: false
  - name: infrastructure
    description: インフラストラクチャ設計・IaCコード管理
    readonly: false
  - name: ci-cd
    description: CI/CDパイプラインの設計と最適化
    readonly: false
  - name: observability
    description: ロギング・監視・トレーシングの設計
    readonly: false
  - name: git
    description: Gitワークフロー・ブランチ戦略設計
    readonly: false
  - name: merge
    description: 競合解決
    readonly: false
  - name: memory
    description: 知識ベース管理。パターン・規約の参照と記録
    readonly: false
---

# execute - タスク遂行コマンド

<purpose>
サブエージェントを活用してタスクを遂行する。
親エージェントは方針決定・判断・要件定義・仕様検討に専念し、詳細な実作業（調査・ドキュメント作成・コード生成）はサブエージェントに委譲する。
</purpose>

<workflow>
1. **タスク分析**: 要求内容を理解し、作業範囲を特定
2. **タスク分割**: 大きすぎる場合は適切な粒度に分割
3. **依存関係整理**: 並列可能なタスクと順序実行が必要なタスクを特定
4. **サブエージェントへ依頼**: 詳細な指示を付けて並列実行可能なタスクは同時依頼
5. **結果統合**: 各サブエージェントの成果を統合・検証し、必要に応じて追加指示
</workflow>

<subagent_instructions>
各サブエージェントへの指示に含めるべき内容:

- **具体的な作業内容と期待する成果物**: 何を、どのように実装・調査するか
- **対象ファイルパス**: 操作対象の具体的なパス
- **serena MCP積極利用の指示**: `find_symbol`, `get_symbols_overview`, `search_for_pattern`等を活用
- **context7 MCP積極利用の指示**: ライブラリ使用時は最新仕様を確認
- **参照すべき既存実装**: ある場合は具体的なパスを明示
- **メモリ確認の指示**: `list_memories`で関連するパターン・規約を確認
  </subagent_instructions>

<codex_usage>

## codex MCP と カスタムエージェントの役割分担

### codex MCP（コード生成専用）

**許可用途**:

- コード生成（新規ファイル・新規関数の作成）
- コード修正（既存コードの編集・リファクタリング）

**禁止用途**（カスタムエージェントまたは基本ツールを使用）:

- 調査・分析 → Explore エージェント、serena MCP
- 品質検証 → quality エージェント
- セキュリティ検証 → security エージェント
- テスト作成 → test エージェント
- ドキュメント作成 → docs エージェント
- コードレビュー → review エージェント

**実行原則**:

- 基本ツール優先: Read/Edit/Write/Grep/Glob等で完結する場合はcodexを使用しない
- 最小粒度の原則: 1回の呼び出しは1つの明確で小さなタスク
- 段階的実行: 複雑な作業は「調査」「設計検討」「実装」の各フェーズを分離
- 複数ファイル編集の禁止: ファイルごとに別の呼び出しに分割
- タイムアウトリスクがある場合は不使用

### カスタムエージェント（調査・レビュー・専門タスク用）

タスク種別に応じて適切なエージェントを選択:

| タスク種別                 | 使用エージェント |
| -------------------------- | ---------------- |
| コード品質検証             | quality          |
| セキュリティ検証           | security         |
| テスト作成・カバレッジ改善 | test             |
| リファクタリング           | refactor         |
| ドキュメント作成・更新     | docs             |
| コードレビュー             | review           |
| バグ調査・デバッグ         | debug            |
| コードベース探索           | Explore          |

</codex_usage>

<agent_delegation>

## エージェント委譲（必須）

実装タスクは以下のエージェントを積極活用すること:

### 品質保証フェーズ

- **quality エージェント**: 構文検証、型検証、フォーマット検証
- **security エージェント**: セキュリティ脆弱性検出

### 実装フェーズ

- **test エージェント**: テスト作成、カバレッジ改善
- **refactor エージェント**: リファクタリング、技術的負債解消
- **docs エージェント**: ドキュメント更新

### レビューフェーズ

- **review エージェント**: 実装後のコードレビュー

### 委譲時の指示に含めるべき内容

1. 具体的な作業内容と期待する成果物
2. 対象ファイルパス
3. serena MCP積極利用の指示
   - `find_symbol`, `get_symbols_overview`: シンボル分析
   - `find_referencing_symbols`: 依存関係確認
   - `list_memories`, `read_memory`: 既存パターン確認
4. context7 MCP積極利用の指示（ライブラリ最新仕様確認）
5. 参照すべき既存実装のパス

### 並列実行

独立したタスクは複数エージェントに並列で委譲すること:

- quality + security: 品質とセキュリティの同時検証
- test + docs: テスト作成とドキュメント更新の同時実行
  </agent_delegation>

<constraints>
- 与えられた指示に集中すること
- 過去の実装を示唆するコメント、不必要なコメントは避けること
- プロジェクトの統括・方針決定に注力し、サブエージェントを最大限活用すること
</constraints>
