# CLAUDE.md

## References

@AGENTS.md

Ecosystem-wide architecture: `/Users/hunt/GitHub/nebundle/docs/ARCHITECTURE.md`.
`htce`'s own deep dives (origin, rejected designs, settled gotchas):
[docs/ARCHITECTURE.md](docs/ARCHITECTURE.md).

## Key facts

- DuckDB access is DBI-compliant via {adbi} wrapping
  `duckdb::duckdb_adbc()` — not raw {adbcdrivermanager}, because dbplyr has no
  registered methods for a raw `adbc_connection`. Build connections with
  `with_duckdb_connection()`; don't construct a second one in notebooks.
  Details: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md).
- Call `gc()` right after `print()`-ing an `AdbiConnection` and before
  disconnecting it, or the disconnect silently fails to release the
  connection. Already done in `dkdb_execute()`/`dkdb_collect()`.
