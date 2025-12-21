---
argument-hint: [file-path]
description: markdownテキスト更新コマンド
agents:
  - name: docs
    description: ドキュメント管理。正確性、可読性、網羅性の検証
    readonly: false
  - name: memory
    description: 知識ベース管理。Serenaメモリへの記録
    readonly: false
---

# markdown - テキスト更新コマンド

<purpose>
他コマンド（/define, /ask, /bug 等）の実行結果をmarkdownファイルとして出力する補助コマンド。
</purpose>

<output-rules>
## 出力ファイル

| 実行コンテキスト   | 出力ファイル   |
| ------------------ | -------------- |
| `/define` の後     | `EXECUTION.md` |
| `/ask` `/bug` の後 | `RESEARCH.md`  |
| それ以外           | `MEMO.md`      |

※ファイルパス指定時はそちらを優先
</output-rules>

<workflow>
1. 直前のコマンド実行結果を取得
2. コンテキストに応じた出力ファイル名を決定
3. Write/Edit toolでファイル出力
</workflow>

<guidelines>
## 記載禁止

- 修正履歴・変更ログ
- 検討過程・議論経緯
  </guidelines>

<examples>
/define 認証機能の追加
/markdown
→ EXECUTION.md に要件定義・実行計画を出力

/ask エラーの原因は？
/markdown
→ RESEARCH.md に調査結果を出力

/ask APIの仕様を教えて
/markdown
→ MEMO.md に調査結果を出力
</examples>
