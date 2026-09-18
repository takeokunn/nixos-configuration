---
name: sql-ecosystem
description: Use when working with SQL databases (SELECT/INSERT/UPDATE/DELETE, CREATE TABLE, JOIN, INDEX, EXPLAIN, transactions, or migrations) across PostgreSQL, MySQL, and SQLite.
metadata:
  version: "3.0.0"
---

Cross-engine SQL guidance focused on where PostgreSQL, MySQL, and SQLite diverge, and where
correct-looking SQL fails silently. Skip anything you'd get from a general SQL reference; this
file exists for the parts that surprise a competent developer moving between engines.

## Engine divergence traps

- **SQLite non-STRICT affinity is not a type constraint.** An `INTEGER` column can retain text
  that cannot be converted to an integer. `STRICT` tables enforce their declared types after
  permitted lossless conversions; check the table definition before assuming either behavior.
- **Upsert syntax differs and is not interchangeable**: PostgreSQL uses
  `INSERT ... ON CONFLICT (col) DO UPDATE SET x = EXCLUDED.x`; MySQL uses
  `INSERT INTO t (id, x) VALUES (1, 2) AS new ON DUPLICATE KEY UPDATE x = new.x` in MySQL 8.4.
  Its older `VALUES(x)` reference is deprecated. Verify syntax for the deployed version rather
  than translating the conflict clause alone.
- **`FULL OUTER JOIN` does not exist in MySQL.** Rewrite as
  `LEFT JOIN ... UNION SELECT ... RIGHT JOIN ...`; there is no direct substitute keyword.
- **Foreign-key columns are auto-indexed by MySQL but not by PostgreSQL.** A `REFERENCES` clause
  in PostgreSQL creates no index: joins and cascading deletes on that column do full scans until
  you add `CREATE INDEX` explicitly. This is the single most common missing-index bug when a
  schema is ported from MySQL to PostgreSQL.
- **`EXPLAIN ANALYZE` is MySQL 8.0.18+ only**; earlier MySQL has plain `EXPLAIN`. SQLite's
  equivalent is `EXPLAIN QUERY PLAN`, not `EXPLAIN` (bare `EXPLAIN` in SQLite dumps VDBE
  bytecode, not a query plan: a frequent tool-invocation mistake).

## Version-sensitive behavior

Check the deployed version and its documentation before selecting DDL or relying on a planner feature.
PostgreSQL 17 supports only stored generated columns and requires `STORED`; PostgreSQL 18 adds
`VIRTUAL` and makes it the default when neither keyword is present. This is not a changed default
for previously valid omitted-keyword DDL. State the intended storage explicitly in migrations.

## Query patterns that hide bugs

- **CTE materialization is a silent performance cliff, not just a readability choice.**
  PostgreSQL (12+) inlines a CTE referenced once but materializes it if referenced more than
  once: the same query can regress hard after an innocuous second reference is added. Force the
  behavior explicitly instead of relying on the reference-count heuristic:
  ```sql
  WITH expensive_calc AS MATERIALIZED (
    SELECT user_id, SUM(total) AS lifetime_value FROM orders GROUP BY user_id
  )
  SELECT * FROM expensive_calc WHERE lifetime_value > 1000;
  ```
- **`LAST_VALUE()` silently returns the wrong row without an explicit frame.** The default window
  frame ends at the current row, so `LAST_VALUE(total) OVER (PARTITION BY user_id ORDER BY
  order_date)` returns the *current* row's value, not the partition's actual last value. It needs
  `ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING` to mean what the name implies.
- **`ROWS`, `RANGE`, and `GROUPS` framing are not interchangeable** when `ORDER BY` has
  duplicate values: `ROWS` counts physical rows, `RANGE` counts by value distance (e.g. a 7-day
  window by timestamp), `GROUPS` (PG 11+) counts peer groups of tied rows. Picking the wrong one
  silently double- or under-counts tied rows in a moving aggregate.
- **`LATERAL` lets a subquery in `FROM` reference preceding `FROM` items** (PostgreSQL, MySQL
  8.0.14+): the standard way to do top-N-per-group without a window-function-plus-filter:
  ```sql
  SELECT u.name, t.total FROM users u
  LEFT JOIN LATERAL (
    SELECT total FROM orders WHERE user_id = u.id ORDER BY total DESC LIMIT 3
  ) t ON true;
  ```
- **Leading columns affect composite B-tree efficiency, not absolute usability.** An index on
  `(user_id, status)` generally favors filters on `user_id`, but PostgreSQL 18 can use skip scan
  for some `status`-only filters. Check the actual plan, cardinality, and engine version.
- **`OR` does not necessarily force a table scan.** PostgreSQL can combine separate indexes with
  bitmap OR. Inspect the plan before rewriting as `UNION`; a rewrite must preserve duplicate
  and ordering semantics as well as improve the measured plan.

## Schema design traps

- **Polymorphic association (`commentable_type` + `commentable_id`) cannot carry a foreign-key
  constraint.** Referential integrity is enforced only at the application layer: an orphaned or
  mistyped `commentable_id` is not caught by the database, ever.
- **Monetary values stored as `FLOAT`/`DOUBLE` accumulate rounding error** (`0.1 + 0.2 != 0.3`
  in IEEE 754). Use `DECIMAL`/`NUMERIC`, or store integer minor units (cents).
- **`GENERATED ... STORED` vs `VIRTUAL`**: PostgreSQL supported only `STORED` through v17 (18+
  adds `VIRTUAL`, see version note above); MySQL and SQLite support both. `VIRTUAL` recomputes on
  read. Index support and expression restrictions depend on the engine and version; do not
  infer either persistence or indexability solely from the term "generated column."

## Transactions and isolation

- **Isolation-level defaults differ by engine and by spec-compliance**: PostgreSQL defaults to
  `READ COMMITTED`; MySQL defaults to `REPEATABLE READ`. Standard SQL `REPEATABLE READ` still
  permits phantom reads, but **PostgreSQL's `REPEATABLE READ` prevents phantoms anyway** (it's
  closer to snapshot isolation): code tested against Postgres at that isolation level can see new
  phantom-read failures purely from being pointed at MySQL, with no code change.
- **`FOR UPDATE SKIP LOCKED`** is the standard queue-worker pattern (skip rows another worker
  already locked rather than blocking): `SELECT * FROM jobs WHERE status='pending' FOR UPDATE
  SKIP LOCKED LIMIT 1`. `FOR UPDATE NOWAIT` fails immediately instead of blocking: useful to
  distinguish real contention from a hang.
- **Advisory locks (PostgreSQL) have two different lifetimes**: `pg_advisory_lock`/
  `pg_advisory_unlock` are session-scoped and outlive the transaction unless explicitly released;
  `pg_advisory_xact_lock` auto-releases at commit. Using the session variant inside a connection
  pool leaks locks across borrowed connections if the unlock call is ever skipped on an error path.
- **Deadlock avoidance is ordering, not detection**: acquire locks on multiple rows in a
  consistent order (e.g., always lower ID first) across every code path that touches those rows
  together; a `lock_timeout` (PG) / `innodb_lock_wait_timeout` (MySQL) bounds the blast radius
  when ordering discipline still gets violated somewhere.

## Migrations

- **A fast default is conditional.** PostgreSQL's metadata-only path for adding a column with a
  default requires a non-volatile expression; a volatile default can require a table rewrite.
  Avoid equating "no rewrite" with "no lock or cost". Check the exact DDL and engine version,
  including MySQL's operation-specific instant-DDL restrictions.
- **`CREATE INDEX CONCURRENTLY` (PostgreSQL) can fail and leave an invalid index behind** rather
  than rolling back cleanly: it cannot run inside a transaction, so a failure mid-build does not
  undo. Always check `pg_index.indisvalid` after a concurrent build and `DROP INDEX` + retry if
  it's false; don't assume "the command returned" means "the index is usable."
- **Renaming a column with zero downtime is expand-contract, not `RENAME COLUMN`**: add the new
  column, establish dual writes or change capture for every writer, then backfill with a protocol
  that cannot overwrite newer values. Validate convergence before switching reads; drain old
  readers and writers before dropping the old column. Backfilling before capturing concurrent
  writes loses updates. A bare `RENAME COLUMN` breaks deployments still using the old name.
- **Dropping a column safely requires the drain step**: stop writing from the application first,
  let old code paths fully roll off, only then `DROP COLUMN`: dropping while old code still
  references the column errors mid-request rather than failing at deploy time.

## SQL injection surfaces beyond parameterized values

Parameterized queries close the value-injection path but leave two others open:

- **Wildcard injection in `LIKE`**: if user input is interpolated as the pattern, a value of `%`
  matches every row even through a parameterized query, because `%`/`_` are pattern metacharacters
  the parameterization doesn't escape. For literal substring search, escape the escape character
  itself first (`!` to `!!`), then `%` to `!%` and `_` to `!_`, bind that transformed value, and
  use `... LIKE '%' || $1 || '%' ESCAPE '!'` in PostgreSQL. `ESCAPE` declares the character;
  it does not transform an unescaped parameter.
- **Identifier injection**: parameters bind values, not table/column names: `f"SELECT
  {column_name} FROM {table_name}"` is unparameterizable by definition. The only safe pattern is
  whitelisting identifiers against a known set before interpolating them, plus quoting
  (`quote_ident()` in PostgreSQL) as defense in depth.

## Tooling

- `pg_format --spaces 2 --keyword-case 2 input.sql`: PostgreSQL-aware formatter (pgFormatter).
- `sqlfluff lint --dialect postgres query.sql` / `sqlfluff fix --dialect postgres query.sql`:
  multi-dialect linter/autofixer (postgres, mysql, sqlite, bigquery, ...).
- `pgTAP`: TAP-protocol unit tests written in SQL (`SELECT has_table(...)`, `SELECT
  has_column(...)`), runs inside PostgreSQL itself rather than an external test runner.

## Style convention (Emacs sql-indent)

Right-align keywords so clause bodies start at a consistent column, and put commas at line start:

```sql
SELECT c.customer_id
       , c.customer_name
       , COUNT(o.order_id) AS order_count
  FROM customers AS c
  LEFT JOIN orders AS o
    ON c.customer_id = o.customer_id
 WHERE c.status = 'active'
 GROUP BY c.customer_id
        , c.customer_name
HAVING COUNT(o.order_id) > 5
 ORDER BY order_count DESC
 LIMIT 100;
```

Uppercase keywords, snake_case identifiers, explicit `AS` on aliases, one column per line.

## Related

- [serena-usage](../serena-usage/SKILL.md): navigate schema definitions and find existing query
  patterns across a codebase before adding a new one.
- [investigation-patterns](../investigation-patterns/SKILL.md): for tracing a query-performance
  regression back to a plan change rather than guessing from the query text.
