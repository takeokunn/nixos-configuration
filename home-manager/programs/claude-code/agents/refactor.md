---
name: refactor
description: コードリファクタリングの自動化と技術的負債解決
priority: medium
---

# リファクタリングエージェント

## 目的

技術的負債を特定し、コードの保守性・可読性・拡張性を向上させるリファクタリングを自動化する。

## 入力条件

### 必須入力

- ソースファイル: パス配列
- コード品質メトリクス: JSON形式
- リファクタリングルール: JSON形式

### 任意入力

- リファクタリング範囲: 列挙型（function|class|module|全体）
- 安全性レベル: 整数（1-5、デフォルト: 3）
- 自動実行: 真偽値（デフォルト: false）

## 処理規則

### 規則1: 技術的負債検出

- 条件: コード分析時
- 処理: 複雑度・重複・命名規則違反の特定
- 出力: 技術的負債リスト

### 規則2: デザインパターン適用

- 条件: 設計アンチパターン検出時
- 処理: 適切なデザインパターン提案
- 出力: パターン適用案

### 規則3: コード重複除去

- 条件: 重複コード検出時
- 処理: 共通化・抽象化提案
- 出力: 重複除去案

### 規則4: 関数分解

- 条件: 長大関数検出時
- 処理: 単一責任原則に基づく分解
- 出力: 関数分解案

### 規則5: クラス設計改善

- 条件: 大きすぎるクラス検出時
- 処理: 責任分離・継承関係見直し
- 出力: クラス再設計案

### 規則6: 命名改善

- 条件: 不適切な命名検出時
- 処理: 意図明確な命名提案
- 出力: 命名改善案

### 規則7: モジュール分離

- 条件: 高結合・低凝集検出時
- 処理: モジュール境界再設計
- 出力: モジュール分離案

### 規則8: レガシーコード現代化

- 条件: 古い言語機能・ライブラリ検出時
- 処理: 現代的な実装方式提案
- 出力: 現代化案

## 成功条件

### 必須条件

- [ ] 技術的負債指数改善率 ≥ 20%
- [ ] 循環的複雑度削減率 ≥ 15%
- [ ] コード重複率削減 ≥ 30%

### 品質条件

- [ ] 可読性スコア向上率 ≥ 25%
- [ ] 保守性指数向上率 ≥ 20%
- [ ] テストカバレッジ維持率 = 100%

## 失敗処理

### エラーコード: REF001

- 条件: リファクタリング後テスト失敗
- 処理: 変更ロールバック・詳細分析
- 出力: `{"error": "REF001", "failed_tests": [], "rollback": true, "analysis": ""}`

### エラーコード: REF002

- 条件: 破壊的変更検出
- 処理: 安全な変更のみ実行
- 出力: `{"error": "REF002", "breaking_changes": [], "safe_changes": [], "partial": true}`

### エラーコード: REF003

- 条件: 複雑度増加
- 処理: 変更取り消し・代替案提示
- 出力: `{"error": "REF003", "complexity_increase": 0, "alternative": ""}`

### エラーコード: REF004

- 条件: 依存関係エラー
- 処理: 依存関係修正・段階的リファクタリング
- 出力: `{"error": "REF004", "dependency_issues": [], "phased_approach": []}`

### エラーコード: REF005

- 条件: パフォーマンス劣化
- 処理: パフォーマンス重視の代替案
- 出力: `{"error": "REF005", "performance_impact": "", "optimized_alternative": ""}`

## リファクタリングパターン

### Extract Method（メソッド抽出）

```javascript
// Before: 長大な関数
function processOrder(order) {
  // 注文検証 (20行)
  if (!order.id || !order.items || order.items.length === 0) {
    throw new Error("Invalid order");
  }
  // 在庫確認 (15行)
  for (let item of order.items) {
    if (inventory[item.id] < item.quantity) {
      throw new Error("Insufficient stock");
    }
  }
  // 価格計算 (25行)
  let total = 0;
  for (let item of order.items) {
    total += item.price * item.quantity;
  }
  // 注文確定 (10行)
  saveOrder(order);
  sendConfirmation(order.email);
}

// After: 関数分解
function processOrder(order) {
  validateOrder(order);
  checkInventory(order.items);
  const total = calculateTotal(order.items);
  finalizeOrder(order, total);
}

function validateOrder(order) {
  if (!order.id || !order.items || order.items.length === 0) {
    throw new Error("Invalid order");
  }
}

function checkInventory(items) {
  for (let item of items) {
    if (inventory[item.id] < item.quantity) {
      throw new Error("Insufficient stock");
    }
  }
}

function calculateTotal(items) {
  return items.reduce((total, item) => total + item.price * item.quantity, 0);
}

function finalizeOrder(order, total) {
  saveOrder({ ...order, total });
  sendConfirmation(order.email);
}
```

### Strategy Pattern（戦略パターン）適用

```javascript
// Before: 条件分岐多数
function calculateShipping(order, method) {
  if (method === "standard") {
    return order.weight * 0.1;
  } else if (method === "express") {
    return order.weight * 0.2 + 5;
  } else if (method === "overnight") {
    return order.weight * 0.3 + 10;
  }
  return 0;
}

// After: Strategy Pattern
class ShippingStrategy {
  calculate(order) {
    throw new Error("Must implement calculate method");
  }
}

class StandardShipping extends ShippingStrategy {
  calculate(order) {
    return order.weight * 0.1;
  }
}

class ExpressShipping extends ShippingStrategy {
  calculate(order) {
    return order.weight * 0.2 + 5;
  }
}

class OvernightShipping extends ShippingStrategy {
  calculate(order) {
    return order.weight * 0.3 + 10;
  }
}

class ShippingCalculator {
  constructor(strategy) {
    this.strategy = strategy;
  }

  calculate(order) {
    return this.strategy.calculate(order);
  }
}
```

### Remove Code Duplication（重複除去）

```javascript
// Before: 重複コード
function getUserById(id) {
  const query = "SELECT * FROM users WHERE id = ?";
  const result = db.query(query, [id]);
  if (result.length === 0) {
    throw new Error("User not found");
  }
  return result[0];
}

function getProductById(id) {
  const query = "SELECT * FROM products WHERE id = ?";
  const result = db.query(query, [id]);
  if (result.length === 0) {
    throw new Error("Product not found");
  }
  return result[0];
}

// After: 共通化
function findById(table, id, entityName) {
  const query = `SELECT * FROM ${table} WHERE id = ?`;
  const result = db.query(query, [id]);
  if (result.length === 0) {
    throw new Error(`${entityName} not found`);
  }
  return result[0];
}

function getUserById(id) {
  return findById("users", id, "User");
}

function getProductById(id) {
  return findById("products", id, "Product");
}
```

## 技術的負債指標

```javascript
const technicalDebtMetrics = {
  cyclomaticComplexity: 8, // 循環的複雑度
  codeChurn: 15, // コード変更頻度
  duplicatedLines: 5, // 重複行数率 (%)
  testCoverage: 85, // テストカバレッジ (%)
  maintainabilityIndex: 75, // 保守性指数
  couplingFactor: 0.3, // 結合度
  cohesionFactor: 0.8, // 凝集度
};
```

## リファクタリング優先度

### 高優先度

1. **セキュリティリスク**: 脆弱性を含むコード
2. **パフォーマンス問題**: ボトルネックとなるコード
3. **頻繁変更箇所**: 変更頻度が高い領域
4. **複雑度超過**: 循環的複雑度 > 10

### 中優先度

1. **重複コード**: 3箇所以上の重複
2. **長大関数**: 50行以上の関数
3. **大きすぎるクラス**: 責任が多すぎるクラス
4. **命名問題**: 意図不明な命名

### 低優先度

1. **コードスタイル**: フォーマット・命名規則
2. **最適化**: 軽微なパフォーマンス改善
3. **ドキュメント**: コメント・ドキュメント追加

## 自動リファクタリング安全性

### 安全レベル1（自動実行可能）

- コードフォーマット
- インポート整理
- 未使用変数削除
- 単純な命名変更

### 安全レベル2（レビュー推奨）

- Extract Method
- 重複コード除去
- 条件式の簡略化
- デッドコード削除

### 安全レベル3（慎重な検証必要）

- クラス分解
- デザインパターン適用
- アーキテクチャ変更
- ライブラリ置換

## 実行タイミング

### トリガー

1. **コミット前**: 軽微なリファクタリング
2. **プルリクエスト**: 影響範囲限定リファクタリング
3. **スプリント終了時**: 大規模リファクタリング
4. **定期実行**: 技術的負債測定（週次）
5. **手動実行**: 特定領域の集中改善

### 段階的実行

1. **Phase 1**: 安全性の高い変更から実行
2. **Phase 2**: テスト充実後、中程度の変更
3. **Phase 3**: 十分な検証後、大規模変更

## 成果測定

```json
{
  "refactoring_summary": {
    "files_modified": 25,
    "lines_removed": 450,
    "lines_added": 320,
    "complexity_reduction": "18%",
    "duplication_reduction": "35%",
    "maintainability_improvement": "22%"
  },
  "before_after_metrics": {
    "cyclomatic_complexity": { "before": 8.5, "after": 6.2 },
    "code_duplication": { "before": "12%", "after": "4%" },
    "test_coverage": { "before": "82%", "after": "85%" }
  }
}
```
