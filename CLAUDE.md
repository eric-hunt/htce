@AGENTS.md

## DuckDB backend: DBI → adbi/adbcdrivermanager (branch `refactor/adbc-duckdb-backend`)

`dkdb_execute()`/`dkdb_collect()` (R/db_query.R), `with_duckdb_connection()`
(R/db_connection.R), and `dkdb_lookup()` (R/db_helpers.R) were migrated off
{DBI}+{duckdb} onto an ADBC-backed connection. This went through two designs;
the codebase now uses the second one.

### Current design: {adbi} (DBI-compliant wrapper around {adbcdrivermanager})

`with_duckdb_connection()` builds the connection via:

```r
con <- DBI::dbConnect(
  adbi::adbi(duckdb::duckdb_adbc()),
  uri = db_file_path,
  access_mode = if (read_only) "READ_ONLY" else "READ_WRITE"
)
```

**`uri` vs `path`**: both are accepted by the duckdb ADBC driver and both
correctly point the connection at the given file (verified: neither is
silently ignored). But they are not just two spellings of the same thing —
confirmed by passing both at once, in both argument orders — `uri` always
wins and `path` is dropped. `dbdir` (the old DBI/duckdb arg name) errors
outright (`options were not recognized: dbdir`), confirming it isn't carried
over from the pre-ADBC API. Standardized on `uri` since it's the one that
takes priority whenever both could apply.

`duckdb::duckdb_adbc()` is the exported driver handle the `duckdb` package ships;
there is no separate `adbcduckdb` driver package installed/needed here (Eric
initially wrote `adbi::adbi("adbcduckdb")` from a template but confirmed that
package isn't the one in use — `duckdb::duckdb_adbc()` is correct). `adbi()`
accepts any `adbcdrivermanager::adbc_driver` object and wraps it as a real
`DBI::DBIConnection` subclass (`AdbiConnection`), so `dbIsValid()`,
`dbDisconnect()`, `dbExecute()`, and `glue::glue_sql(.con = <the connection>)`
quoting all work natively again — no more `DBI::ANSI()` placeholder or manual
`as.character()` coercion needed. `dkdb_collect()` fetches via
`DBI::dbGetQueryArrow()` to keep the `nanoarrow_array_stream` default (the
whole point of moving off DBI's `dbGetQuery()`/data.frame default).
Cleanup is a plain `withr::defer(DBI::dbDisconnect(con), envir = .local_envir)`.

**Why not raw {adbcdrivermanager} connections (the first design)?** That
approach (`adbcdrivermanager::adbc_database_init()` +
`adbc_connection_init()`, no {adbi}) produces an `adbc_connection` object with
no registered DBI S4 methods — confirmed no `dbQuoteIdentifier`/`dbQuoteLiteral`
methods exist for it, and **{dbplyr} has no registered methods for it either**,
so `dplyr::tbl(con, ...)` doesn't work. That broke a primary workflow purpose
of this package (notebook use via dbplyr). `AdbiConnection` and a raw
`adbc_connection` are *not* interchangeable — confirmed passing an
`AdbiConnection` into `adbcdrivermanager::execute_adbc()` directly errors
(`assert_adbc()` rejects it), and `adbcdrivermanager::local_adbc()` errors on
an `AdbiConnection` too (it's a different class hierarchy). Since
`with_duckdb_connection()` is the one exported entry point shared by
`dkdb_execute()`/`dkdb_collect()`/`dkdb_lookup()`, switching had to happen here
rather than by having notebooks build a second, separate adbi connection
alongside the package's internal one.

Notebook usage now looks like:

```r
library(adbi); library(dplyr); library(dbplyr)
con <- withr::local_db_connection(
  DBI::dbConnect(adbi::adbi(duckdb::duckdb_adbc()), path = "data.duckdb")
)
tbl(con, "your_table_name")  # dbplyr verbs work directly
```

- **Gotcha (confirmed by reproduction): printing an `AdbiConnection` leaks a
  child object.** `AdbiConnection`'s `show`/`dbGetInfo` method creates an ADBC
  stream that's only released by the R garbage collector, not eagerly. If
  `print(db)` runs (the `!.quiet` diagnostic path in `dkdb_execute()`/
  `dkdb_collect()`) and `dbDisconnect()` fires shortly after (e.g. via a
  `withr::defer()` in the same frame) with no GC in between, the disconnect
  silently fails to fully release the connection — `dbIsValid()` stays `TRUE`
  after the "disconnect". Fix: call `gc()` right after `print(db)` in both
  functions. Without this, verbose (`.quiet = FALSE`) calls leak a live
  connection every time.
- `DESCRIPTION` Imports: `adbi` (new), `adbcdrivermanager` dropped (no longer
  called via `::` anywhere in R/ — `adbi`/`duckdb`/`DBI` cover everything now).
  `DBI` stays for `DBI::dbConnect()`/`dbExecute()`/`dbGetQueryArrow()`/
  `dbIsValid()`/`dbDisconnect()` — no longer just for `ANSI()`.
- `with_duckdb_connection()` was changed in place both times (same exported
  name/signature; return type went `DBIConnection` → `adbc_connection` →
  `AdbiConnection`/`DBIConnection`) rather than adding parallel functions —
  only in-package call sites (db_query.R, db_helpers.R) depend on it.
- Out of scope for this pass (per Eric): `ebaser:::.return_fn_factory()` in
  `dkdb_collect()` — still assumes a DBI-style default of `data.frame`; the
  ADBC-native default is a `nanoarrow_array`/`nanoarrow_array_stream`. Not
  touched yet.
