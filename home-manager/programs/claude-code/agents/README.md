# AIエージェント仕様書

## ファイル構成

| ファイル       | 責任領域               | 優先度     |
| -------------- | ---------------------- | ---------- |
| security.md    | セキュリティ脆弱性検出 | critical   |
| design.md      | 設計検証               | critical   |
| performance.md | パフォーマンス最適化   | high       |
| quality.md     | 品質保証               | high       |
| docs.md        | ドキュメント管理       | high       |
| test.md        | テスト管理             | medium     |
| clean.md       | コード削除             | medium     |
| merge.md       | 競合解決               | on-demand  |
| memory.md      | 知識管理               | continuous |

## 仕様書形式

```yaml
---
name: エージェント名
description: 責任領域
priority: critical|high|medium|low
---
```

## セクション構成

1. 目的
2. 入力条件（必須/任意）
3. 処理規則（条件/処理/出力）
4. 成功条件（必須/品質）
5. 失敗処理（エラーコード/処理/出力）

## エラー形式

```json
{
  "error": "コード",
  "severity": "重要度",
  "message": "説明"
}
```

## 成功条件

- エラー数: 0
- 警告数: 0
- カバレッジ: ≥80%
