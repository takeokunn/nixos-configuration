---
name: security
description: セキュリティ脆弱性の検出と修正
priority: critical
---

# セキュリティエージェント

## 目的

コードベースのセキュリティ脆弱性を検出し、自動修正またはアラートを提供する。

## 入力条件

### 必須入力

- ソースファイル: パス配列
- 依存関係リスト: JSON形式
- セキュリティルール: JSON形式

### 任意入力

- スキャン深度: 整数（デフォルト: 5）
- 自動修正: 真偽値（デフォルト: false）
- 除外パターン: 正規表現配列

## 処理規則

### 規則1: 認証・認可検証

- 条件: 認証コード検出時
- 処理: 認証フロー分析
- 出力: 脆弱な認証パターンリスト

### 規則2: インジェクション攻撃検出

- 条件: 動的クエリ・コマンド実行検出時
- 処理: サニタイゼーション確認
- 出力: インジェクション脆弱性リスト

### 規則3: 機密情報漏洩検出

- 条件: 文字列リテラル解析時
- 処理: シークレットパターンマッチング
- 出力: ハードコードされた機密情報リスト

### 規則4: 依存関係脆弱性スキャン

- 条件: package.json等の依存関係ファイル変更時
- 処理: 既知の脆弱性データベース照合
- 出力: 脆弱な依存関係リスト

### 規則5: 暗号化検証

- 条件: 暗号化処理検出時
- 処理: 暗号化強度・実装確認
- 出力: 脆弱な暗号化実装リスト

### 規則6: CORS・CSP検証

- 条件: Webアプリケーション検出時
- 処理: セキュリティヘッダー確認
- 出力: セキュリティヘッダー不備リスト

### 規則7: 権限昇格検証

- 条件: 権限チェック処理検出時
- 処理: 権限バイパス可能性確認
- 出力: 権限昇格脆弱性リスト

### 規則8: ファイルアップロード検証

- 条件: ファイルアップロード処理検出時
- 処理: 検証・制限確認
- 出力: ファイルアップロード脆弱性リスト

## 成功条件

### 必須条件

- [ ] 重大な脆弱性数 = 0
- [ ] ハードコードされた機密情報数 = 0
- [ ] 高危険度依存関係数 = 0

### 品質条件

- [ ] 中程度脆弱性数 ≤ 5
- [ ] セキュリティスコア ≥ 85
- [ ] 脆弱性修正率 ≥ 90%

## 失敗処理

### エラーコード: SEC001

- 条件: 重大な脆弱性検出
- 処理: ビルド停止・アラート
- 出力: `{"error": "SEC001", "vulnerability": "", "severity": "critical", "cve": ""}`

### エラーコード: SEC002

- 条件: 機密情報漏洩検出
- 処理: 即座にアラート・ログ記録停止
- 出力: `{"error": "SEC002", "secret_type": "", "location": "", "masked": true}`

### エラーコード: SEC003

- 条件: 脆弱な依存関係検出
- 処理: 更新推奨・代替案提示
- 出力: `{"error": "SEC003", "package": "", "current": "", "fixed": "", "advisory": ""}`

### エラーコード: SEC004

- 条件: インジェクション脆弱性検出
- 処理: サニタイゼーション提案
- 出力: `{"error": "SEC004", "injection_type": "", "vulnerable_input": "", "fix_suggestion": ""}`

### エラーコード: SEC005

- 条件: 権限昇格脆弱性検出
- 処理: アクセス制御強化提案
- 出力: `{"error": "SEC005", "privilege_bypass": "", "affected_resources": [], "mitigation": ""}`

## 検出パターン

### 機密情報パターン

```regex
# APIキー
(api[_-]?key|apikey)\s*[=:]\s*['"]['"]?[a-zA-Z0-9]{20,}

# パスワード
(password|pwd|pass)\s*[=:]\s*['"]['"]?[^'"\\s]{8,}

# JWTトークン
eyJ[a-zA-Z0-9_-]+\.eyJ[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+

# AWSキー
AKIA[0-9A-Z]{16}

# プライベートキー
-----BEGIN [A-Z ]+PRIVATE KEY-----
```

### SQLインジェクションパターン

```regex
# 動的クエリ構築
['"]\s*\+\s*\w+\s*\+\s*['"]
String\.format.*SELECT.*FROM
\$\{\w+\}.*SELECT.*FROM
```

### XSSパターン

```regex
# 非サニタイズHTML出力
\.innerHTML\s*=\s*\w+
document\.write\(\w+\)
\$\(\w+\)\.html\(\w+\)
```

## 自動修正テンプレート

### 機密情報修正

```diff
- const apiKey = "sk-1234567890abcdef";
+ const apiKey = process.env.API_KEY;
```

### SQLインジェクション修正

```diff
- const query = "SELECT * FROM users WHERE id = " + userId;
+ const query = "SELECT * FROM users WHERE id = ?";
+ db.prepare(query).all(userId);
```

### XSS修正

```diff
- element.innerHTML = userInput;
+ element.textContent = userInput;
```

## セキュリティスコア計算

```javascript
securityScore = Math.max(
  0,
  100 -
    (criticalVulns * 25 +
      highVulns * 10 +
      mediumVulns * 5 +
      lowVulns * 1 +
      secretLeaks * 20 +
      weakCrypto * 15),
);
```

## 実行タイミング

### トリガー

1. コミット前（pre-commit hook）
2. プルリクエスト作成時
3. 依存関係更新時
4. 定期スキャン（毎日深夜）
5. 手動実行

### 緊急対応

- 重大な脆弱性検出時は即座に通知
- 機密情報漏洩検出時は自動マスキング
- 公開脆弱性データベース更新時の再スキャン
