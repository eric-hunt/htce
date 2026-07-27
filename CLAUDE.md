@AGENTS.md

## DuckDB backend: DBI → adbcdrivermanager (branch `refactor/adbc-duckdb-backend`)

`dkdb_execute()`/`dkdb_collect()` (R/db_query.R) and `with_duckdb_connection()`
(R/db_connection.R) were migrated off {DBI} onto {adbcdrivermanager}. Key facts,
verified against the installed packages (not just inferred from docs):

- **Connection construction**: `with_duckdb_connection()` now builds a real ADBC
  connection instead of a DBI one:

  ```r
  db  <- adbcdrivermanager::adbc_database_init(duckdb::duckdb_adbc(), path = db_file_path,
           access_mode = if (read_only) "READ_ONLY" else "READ_WRITE")
  con <- adbcdrivermanager::adbc_connection_init(db)
  ```

  `duckdb::duckdb_adbc()` is the exported driver handle the `duckdb` package ships
  for exactly this; `access_mode` (`"READ_ONLY"`/`"READ_WRITE"`) is duckdb's own
  config key, forwarded through as a database option — there is no separate
  `read_only` ADBC option name.
- **Cleanup**: uses `adbcdrivermanager::local_adbc(x, .local_envir = ...)` (the
  package's own withr-style helper) instead of `withr::defer(DBI::dbDisconnect(...))`.
  Register the database before the connection so LIFO release order tears down
  the connection (child) before the database (parent) — ADBC errors if a parent
  is released with live children.
- **`DBI::dbIsValid()` has no ADBC equivalent** — use
  `adbcdrivermanager::adbc_xptr_is_valid(x)`.
- **`glue::glue_sql(..., .con = <adbc connection>)` does not work.** glue_sql's
  quoting transformer calls `DBI::dbQuoteIdentifier()`/`dbQuoteLiteral()` on
  `.con`'s class, and no such S4 methods are registered for `adbc_connection`/
  `adbc_database` — confirmed via `methods()`. Fix: pass `.con = DBI::ANSI()`
  instead of the live connection. `ANSI()` is a connection-free ANSI-SQL quoting
  stub, and DuckDB's quoting (double-quote identifiers, single-quote literals)
  is ANSI-compliant. This pattern is needed everywhere `glue_sql()` is called
  against a DuckDB connection, not just in db_query.R — `dkdb_lookup()`
  (R/db_helpers.R) had the same call and got the same fix.
- **`glue_sql()` returns an object of class `SQL`** (subclass of character).
  `adbcdrivermanager::adbc_statement_set_sql_query()` cannot consume it directly
  (`Can't convert classed object to const char*`) — call sites must
  `as.character()` the glued statement before passing it to `execute_adbc()`/
  `read_adbc()`.
- `adbcdrivermanager` needed adding to DESCRIPTION Imports (was used via `::` but
  undeclared).
- Out of scope for this pass (per Eric): `ebaser:::.return_fn_factory()` in
  `dkdb_collect()` — still assumes a DBI-style default of `data.frame`; the
  ADBC-native default is a `nanoarrow_array`/`nanoarrow_array_stream`. Not
  touched yet.
- `with_duckdb_connection()` was changed in place (same exported name/signature,
  return type changed from `DBIConnection` to `adbc_connection`) rather than
  adding a parallel function — decided explicitly since only in-package call
  sites (db_query.R, db_helpers.R) depend on it.
- `DBI` stays in DESCRIPTION Imports for `DBI::ANSI()` alone (the only remaining
  `DBI::` call in R/). Decided to keep it explicit rather than drop it: it's a
  near-zero-weight dependency (no compiled code) and `duckdb` already pulls it
  in transitively, so declaring it directly is more honest than relying on that
  transitive import.
