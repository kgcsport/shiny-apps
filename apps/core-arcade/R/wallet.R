# apps/core-arcade/R/wallet.R
# Wallet ledger helpers. Amounts stored as half-units (integer) so we can
# represent .5 flex passes without floating-point drift.
# e.g., delta_half = 1 => +0.5 flex pass, delta_half = 2 => +1.0 flex pass.

wallet_post <- function(con, player_id, delta_half,
                        kind, ref_type = NULL, ref_id = NULL, note = NULL) {
  if (is.null(con)) return(invisible(NULL))
  DBI::dbExecute(con, "
    INSERT INTO wallet_ledger(ts, player_id, delta_half, kind, ref_type, ref_id, note)
    VALUES(?, ?, ?, ?, ?, ?, ?);
  ", list(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    as.character(player_id),
    as.integer(delta_half),
    as.character(kind),
    if (is.null(ref_type)) NA_character_ else as.character(ref_type),
    if (is.null(ref_id))   NA_character_ else as.character(ref_id),
    if (is.null(note))     NA_character_ else as.character(note)
  ))
  invisible(NULL)
}

wallet_balance <- function(con, player_id) {
  if (is.null(con)) return(0L)
  row <- DBI::dbGetQuery(con,
    "SELECT COALESCE(SUM(delta_half), 0) AS bal FROM wallet_ledger WHERE player_id = ?;",
    list(as.character(player_id)))
  as.integer(row$bal[1])
}

# Format half-unit integer as a human-readable flex string.
# e.g., 3 -> "1.5",  2 -> "1",  0 -> "0"
format_flex <- function(balance_half) {
  whole <- balance_half %/% 2L
  half  <- balance_half %% 2L
  if (half == 0L) as.character(whole) else paste0(whole, ".5")
}
