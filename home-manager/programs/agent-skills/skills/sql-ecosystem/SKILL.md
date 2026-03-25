---
name: sql-ecosystem
description: "Use when working with SQL databases, writing queries involving SELECT, INSERT, UPDATE, DELETE, CREATE TABLE, JOIN, INDEX, EXPLAIN, transactions, or database migrations. Provides patterns for schema design, query optimization, and transaction management across PostgreSQL, MySQL, and SQLite."
---

Comprehensive SQL patterns for database operations, schema design, query optimization, transaction management, and migrations across PostgreSQL, MySQL, and SQLite. The agent should use parameterized queries for all user input and follow ANSI SQL standards with database-specific extensions where needed.

## Critical Rules

- Use parameterized queries for ALL user input — NEVER use string concatenation
- Create indexes on foreign key columns
- Use explicit transaction boundaries for multi-statement operations
- Escape wildcards in LIKE patterns when using user input
- Analyze query plans with EXPLAIN before optimizing
- Use appropriate isolation levels for transaction requirements

## Workflow

1. **Analyze** — Identify data model, relationships, query patterns, and access frequency; review existing schema and indexes
2. **Implement** — Design normalized schema (3NF baseline), write queries with appropriate indexes, use transactions for data integrity
3. **Validate** — Analyze with EXPLAIN, test with production-like data volume, verify transaction isolation

## SQL Fundamentals

### Data Types

**ANSI standard types** (portable across databases):

```sql
-- Numeric: INTEGER, SMALLINT, BIGINT, DECIMAL(p,s), NUMERIC(p,s), REAL, DOUBLE PRECISION
-- String: CHAR(n), VARCHAR(n), TEXT
-- Date/Time: DATE, TIME, TIMESTAMP, INTERVAL
-- Boolean: BOOLEAN
```

**Database-specific types:**
- PostgreSQL: UUID, JSONB, ARRAY, INET, SERIAL/BIGSERIAL, range types
- MySQL: TINYINT, MEDIUMINT, ENUM, SET, JSON
- SQLite: Type affinity system (TEXT, INTEGER, REAL, BLOB, NULL)

### DDL Patterns

```sql
-- Table creation with constraints
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  name VARCHAR(100) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT email_format CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$')
);

CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL,
  total DECIMAL(10, 2) NOT NULL,
  status VARCHAR(20) DEFAULT 'pending',
  CONSTRAINT fk_user FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);
```

### Index Patterns

```sql
CREATE INDEX idx_users_email ON users(email);                          -- B-tree (default)
CREATE UNIQUE INDEX idx_users_email_unique ON users(email);            -- Unique
CREATE INDEX idx_orders_user_status ON orders(user_id, status);        -- Composite
CREATE INDEX idx_active_users ON users(email) WHERE active = true;     -- Partial (PostgreSQL)
CREATE INDEX idx_users_lower_email ON users(LOWER(email));             -- Expression (PostgreSQL)
```

**Index selection:** B-tree for equality/range lookups, GIN with tsvector for full-text search (PostgreSQL), GIN for JSON containment, GiST for geospatial.

### DML Patterns

```sql
-- Upsert (PostgreSQL)
INSERT INTO users (email, name) VALUES ('user@example.com', 'Updated Name')
ON CONFLICT (email) DO UPDATE SET name = EXCLUDED.name;

-- Upsert (MySQL)
INSERT INTO users (email, name) VALUES ('user@example.com', 'Updated Name')
ON DUPLICATE KEY UPDATE name = VALUES(name);

-- Soft delete (preferred for audit trails)
UPDATE users SET deleted_at = NOW() WHERE id = 1;
```

## Security: Parameterized Queries

```sql
-- PostgreSQL with pg (Node.js)
client.query('SELECT * FROM users WHERE email = $1 AND status = $2', [userEmail, status])

-- MySQL with mysql2 (Node.js)
connection.execute('SELECT * FROM users WHERE email = ? AND status = ?', [userEmail, status])

-- Python (psycopg2/mysql-connector/sqlite3)
cursor.execute("SELECT * FROM users WHERE email = %s AND status = %s", (user_email, status))
```

**NEVER** use string concatenation: `query = f"SELECT * FROM users WHERE email = '{user_input}'"` enables SQL injection.

**Safe LIKE patterns:** Escape `%` and `_` in user input before use. For exact matching, prefer `position($1 in name) > 0`.

**Dynamic SQL:** Whitelist allowed table/column names. Use `quote_ident()` in PostgreSQL.

## Query Patterns

### Joins

- **INNER JOIN** — Only matching rows from both tables
- **LEFT JOIN** — All rows from left table, matching from right
- **FULL OUTER JOIN** — All rows from both tables (not supported in MySQL; use UNION of LEFT and RIGHT JOINs)
- **Self join** — Join table with itself (e.g., employees and managers)

### CTEs and Window Functions

```sql
-- Recursive CTE for hierarchical data
WITH RECURSIVE org_tree AS (
  SELECT id, name, manager_id, 1 as level FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, e.manager_id, ot.level + 1
  FROM employees e INNER JOIN org_tree ot ON e.manager_id = ot.id
)
SELECT * FROM org_tree ORDER BY level, name;

-- Window functions for analytics
SELECT date, revenue,
  SUM(revenue) OVER (ORDER BY date) as cumulative_revenue,
  AVG(revenue) OVER (ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) as moving_avg_7d
FROM daily_sales;
```

### Subqueries

Use `EXISTS` over `IN` for large datasets (EXISTS stops at first match). Use derived tables or CTEs for complex multi-step queries. Avoid correlated subqueries when a JOIN achieves the same result.

## Schema Design

### Normalization

- **1NF** — Atomic values, no repeating groups (no comma-separated values)
- **2NF** — No partial dependencies on composite key
- **3NF** — No transitive dependencies

Normalize to 3NF for data integrity. Consider denormalization for read-heavy hot paths or reporting/analytics (star schema).

### Common Patterns

- **Surrogate keys** — Use auto-generated IDs; natural keys can change
- **Soft deletes** — `deleted_at TIMESTAMP NULL` with partial unique constraint
- **Audit columns** — `created_at`, `updated_at`, `created_by`, `updated_by` with auto-update triggers
- **Junction tables** — Many-to-many with composite primary key
- **Enum tables** — Reference tables over ENUM type for flexibility

## Query Optimization

```sql
-- Analyze query plans
EXPLAIN ANALYZE SELECT * FROM users WHERE email = 'test@example.com';  -- PostgreSQL
EXPLAIN ANALYZE SELECT * FROM users WHERE email = 'test@example.com';  -- MySQL 8.0.18+
EXPLAIN QUERY PLAN SELECT * FROM users WHERE email = 'test@example.com'; -- SQLite
```

**Key optimizations:**
- Select only needed columns (never `SELECT *` in production)
- Use `EXISTS` over `COUNT(*)` for existence checks
- Batch inserts/updates (10K-100K rows per batch)
- Keyset pagination (`WHERE id > last_id`) over OFFSET for large datasets
- Use UNION instead of OR on different columns for index usage
- Covering indexes to avoid table lookups

## Transactions

### Isolation Levels

| Level | Dirty Reads | Non-repeatable | Phantoms | Use Case |
|-------|-------------|----------------|----------|----------|
| READ UNCOMMITTED | Yes | Yes | Yes | Approximate analytics |
| READ COMMITTED | No | Yes | Yes | Most OLTP (PostgreSQL default) |
| REPEATABLE READ | No | No | Maybe | Financial transactions (MySQL default) |
| SERIALIZABLE | No | No | No | Critical financial/inventory |

### Locking Patterns

```sql
-- Row-level lock for update
SELECT * FROM accounts WHERE id = 1 FOR UPDATE;

-- Skip locked rows (queue processing)
SELECT * FROM jobs WHERE status = 'pending' FOR UPDATE SKIP LOCKED LIMIT 1;

-- Advisory locks (PostgreSQL)
SELECT pg_advisory_xact_lock(12345);  -- Auto-released on commit
```

**Optimistic locking:** Add `version INTEGER` column, check version on update. **Pessimistic locking:** `SELECT ... FOR UPDATE` before modifying. **Deadlock prevention:** Always acquire locks in consistent order; set `lock_timeout`.

## Migrations

### Zero-Downtime Patterns

1. **Add column:** Add nullable first, backfill in batches, then add NOT NULL constraint
2. **Rename column:** Add new column, copy data, deploy dual-write code, drop old column
3. **Add index:** Use `CREATE INDEX CONCURRENTLY` (PostgreSQL) to avoid table locks
4. **Drop column:** Stop writing in application, deploy, then DROP COLUMN

```sql
-- Idempotent migration
CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, email VARCHAR(255) NOT NULL);
CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
ALTER TABLE users ADD COLUMN IF NOT EXISTS name VARCHAR(100);
```

### Batch Data Migration (PostgreSQL)

```sql
DO $$
DECLARE batch_size INTEGER := 1000; rows_updated INTEGER;
BEGIN
  LOOP
    UPDATE users SET email_normalized = LOWER(email)
    WHERE email_normalized IS NULL
      AND id IN (SELECT id FROM users WHERE email_normalized IS NULL LIMIT batch_size);
    GET DIAGNOSTICS rows_updated = ROW_COUNT;
    EXIT WHEN rows_updated = 0;
    COMMIT;
    PERFORM pg_sleep(0.1);
  END LOOP;
END $$;
```

## Formatting Style (Emacs sql-indent)

The agent should follow these conventions:
- Right-align SQL keywords; content starts at consistent column
- Leading commas aligned with first column name after SELECT
- UPPERCASE for all SQL keywords; lowercase for identifiers
- snake_case for all identifiers; meaningful table aliases with explicit AS

```sql
SELECT c.customer_id
       , c.customer_name
       , COUNT(o.order_id) AS order_count
       , COALESCE(SUM(o.total), 0) AS total_spent
  FROM customers AS c
  LEFT JOIN orders AS o
    ON c.customer_id = o.customer_id
 WHERE c.status = 'active'
   AND c.created_at >= '2024-01-01'
 GROUP BY c.customer_id
        , c.customer_name
HAVING COUNT(o.order_id) > 5
 ORDER BY total_spent DESC
 LIMIT 100;
```

## Anti-Patterns to Avoid

- **SELECT \*** in production — explicitly list required columns
- **Missing indexes** on filter/join columns
- **N+1 queries** — use JOIN or IN clause instead
- **String concatenation** for SQL — use parameterized queries
- **Implicit type conversion** — prevents index usage
- **Cartesian joins** — always use explicit JOIN with ON clause
- **Over-normalization** — balance with read performance needs

## Error Escalation

- **Low:** Missing index on infrequently queried column — note for future optimization
- **Medium:** Query performance degradation — analyze EXPLAIN output, propose optimization
- **High:** Deadlock or lock timeout — stop, analyze lock patterns, present resolution options
- **Critical:** Data corruption or SQL injection vulnerability — block operation, require immediate remediation
