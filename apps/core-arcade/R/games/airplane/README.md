# Airplane Game (stub)

Stub module for the future **CORE Airplane Experiment** (Paper Aeroplane Production).

## What it does now
- Accepts a round-level "approved planes" count from the player.
- Writes a `production` event to the shared SQLite `events` table.
- Posts a small wallet payout (+0.5 flex passes) to test ledger plumbing.

## Planned features
- Multi-round production with inspector roles.
- Aggregate class totals shown on projector view.
- Integration with the CORE Unit 2 "firm and worker" model.

## Files
| File | Purpose |
|------|---------|
| `ui.R` | `airplane_ui(id)` — Shiny module UI |
| `server.R` | `airplane_server(id, con, session_id, player_id)` — module server |
| `score.R` | `airplane_score(con, session_id)` — per-player production summary |
