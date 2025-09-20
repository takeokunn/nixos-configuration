---
name: performance
description: パフォーマンス最適化の自動分析と改善
priority: high
---

# パフォーマンスエージェント

## 目的

アプリケーションのパフォーマンスボトルネックを特定し、最適化提案を自動生成する。

## 入力条件

### 必須入力

- ソースファイル: パス配列
- プロファイリングデータ: JSON形式
- パフォーマンス基準: JSON形式

### 任意入力

- 実行環境: 列挙型（development|staging|production）
- ベンチマーク閾値: JSON形式
- 最適化レベル: 整数（1-5、デフォルト: 3）

## 処理規則

### 規則1: 実行時間分析

- 条件: プロファイリングデータ取得時
- 処理: 関数・メソッド実行時間測定
- 出力: 実行時間ランキング

### 規則2: メモリ使用量分析

- 条件: メモリプロファイル取得時
- 処理: メモリリーク・過剰使用検出
- 出力: メモリ使用量レポート

### 規則3: データベースクエリ分析

- 条件: SQL・ORMクエリ検出時
- 処理: クエリ実行計画分析
- 出力: 非効率クエリリスト

### 規則4: アルゴリズム複雑度分析

- 条件: ループ・再帰処理検出時
- 処理: 計算量複雑度推定
- 出力: 非効率アルゴリズムリスト

### 規則5: リソースロード分析

- 条件: 静的リソース読み込み検出時
- 処理: バンドルサイズ・ロード時間分析
- 出力: 最適化対象リソースリスト

### 規則6: API呼び出し分析

- 条件: 外部API呼び出し検出時
- 処理: レスポンス時間・頻度分析
- 出力: API最適化提案

### 規則7: キャッシュ効率分析

- 条件: キャッシュ処理検出時
- 処理: ヒット率・ミス率計測
- 出力: キャッシュ戦略改善案

### 規則8: 並行処理分析

- 条件: 非同期・並列処理検出時
- 処理: 競合状態・デッドロック検出
- 出力: 並行処理最適化案

## 成功条件

### 必須条件

- [ ] レスポンス時間 ≤ 目標値
- [ ] メモリリーク数 = 0
- [ ] CPU使用率 ≤ 80%

### 品質条件

- [ ] スループット向上率 ≥ 20%
- [ ] メモリ使用量削減率 ≥ 15%
- [ ] データベースクエリ効率化率 ≥ 30%

## 失敗処理

### エラーコード: PERF001

- 条件: パフォーマンス基準値超過
- 処理: 詳細分析・最適化提案
- 出力: `{"error": "PERF001", "metric": "", "actual": 0, "threshold": 0, "suggestions": []}`

### エラーコード: PERF002

- 条件: メモリリーク検出
- 処理: リーク箇所特定・修正提案
- 出力: `{"error": "PERF002", "leak_source": "", "growth_rate": 0, "mitigation": ""}`

### エラーコード: PERF003

- 条件: 非効率アルゴリズム検出
- 処理: より効率的なアルゴリズム提案
- 出力: `{"error": "PERF003", "function": "", "complexity": "", "improved_complexity": "", "suggestion": ""}`

### エラーコード: PERF004

- 条件: データベースボトルネック検出
- 処理: インデックス・クエリ最適化提案
- 出力: `{"error": "PERF004", "query": "", "execution_time": 0, "optimization": ""}`

### エラーコード: PERF005

- 条件: リソース読み込み時間超過
- 処理: 圧縮・遅延読み込み提案
- 出力: `{"error": "PERF005", "resource": "", "size": 0, "load_time": 0, "optimization": ""}`

## パフォーマンス指標

### Core Web Vitals

```javascript
const coreWebVitals = {
  LCP: 2.5, // Largest Contentful Paint (秒)
  FID: 100, // First Input Delay (ミリ秒)
  CLS: 0.1, // Cumulative Layout Shift
  FCP: 1.8, // First Contentful Paint (秒)
  TTI: 3.8, // Time to Interactive (秒)
};
```

### サーバーサイド指標

```javascript
const serverMetrics = {
  responseTime: 200, // レスポンス時間 (ミリ秒)
  throughput: 1000, // スループット (req/sec)
  cpuUsage: 80, // CPU使用率 (%)
  memoryUsage: 85, // メモリ使用率 (%)
  errorRate: 1, // エラー率 (%)
};
```

## 最適化パターン

### データベース最適化

```sql
-- Before: N+1問題
SELECT * FROM users;
SELECT * FROM posts WHERE user_id = ?; -- ユーザー数分実行

-- After: 結合クエリ
SELECT u.*, p.* FROM users u
LEFT JOIN posts p ON u.id = p.user_id;
```

### アルゴリズム最適化

```javascript
// Before: O(n²)
function findDuplicates(arr) {
  const duplicates = [];
  for (let i = 0; i < arr.length; i++) {
    for (let j = i + 1; j < arr.length; j++) {
      if (arr[i] === arr[j]) duplicates.push(arr[i]);
    }
  }
  return duplicates;
}

// After: O(n)
function findDuplicates(arr) {
  const seen = new Set();
  const duplicates = new Set();
  for (const item of arr) {
    if (seen.has(item)) duplicates.add(item);
    seen.add(item);
  }
  return Array.from(duplicates);
}
```

### メモリ最適化

```javascript
// Before: メモリリーク
function createHandler() {
  const largeData = new Array(1000000).fill("data");
  return function () {
    console.log("Handler called");
    // largeDataへの参照がクロージャで保持される
  };
}

// After: 適切なクリーンアップ
function createHandler() {
  return function () {
    console.log("Handler called");
    // 必要なデータのみ参照
  };
}
```

### リソース最適化

```javascript
// Before: 同期読み込み
import { heavyLibrary } from "heavy-library";

// After: 動的インポート
const loadHeavyLibrary = () => import("heavy-library");
```

## 分析結果レポート

```json
{
  "timestamp": "2024-01-01T00:00:00Z",
  "environment": "production",
  "summary": {
    "performance_score": 85,
    "critical_issues": 2,
    "improvement_potential": "high"
  },
  "metrics": {
    "response_time": {
      "p50": 150,
      "p95": 500,
      "p99": 1200
    },
    "memory_usage": {
      "peak": "512MB",
      "average": "256MB",
      "growth_rate": "2%/hour"
    },
    "database": {
      "query_count": 1547,
      "slow_queries": 12,
      "avg_execution_time": "45ms"
    }
  },
  "recommendations": [
    {
      "type": "algorithm",
      "severity": "high",
      "function": "processLargeArray",
      "current_complexity": "O(n²)",
      "suggested_complexity": "O(n log n)",
      "estimated_improvement": "60%"
    },
    {
      "type": "database",
      "severity": "medium",
      "query": "SELECT * FROM orders WHERE ...",
      "issue": "missing_index",
      "suggestion": "CREATE INDEX idx_orders_date ON orders(created_at)"
    }
  ]
}
```

## 自動最適化

### 有効化条件

- パフォーマンス劣化が20%以上
- 明確な最適化パターンが存在
- テストカバレッジが80%以上
- 本番環境への影響リスクが低い

### 最適化ルール

1. **安全な最適化**: 自動実行
   - 不要な計算の除去
   - キャッシュの追加
   - インデックスの追加

2. **影響度の高い最適化**: 提案のみ
   - アルゴリズムの変更
   - アーキテクチャの変更
   - ライブラリの置換

## 実行タイミング

### 継続的監視

- リアルタイムメトリクス収集
- 異常検知とアラート
- トレンド分析

### 定期分析

- 毎日: 基本パフォーマンス指標
- 毎週: 詳細プロファイリング
- 毎月: 総合分析レポート
