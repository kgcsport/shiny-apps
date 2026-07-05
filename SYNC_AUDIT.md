# Google Drive/Sheets Sync Audit

Small Reclaim/Docker efficiency audit, July 2026. Goal was to keep Drive/Sheets
work out of hot submit paths where practical.

| App | File | Sync path | Hot path? | Change / finding |
|---|---|---|---|---|
| class-job-picker | `apps/class-job-picker/app.R` | `read_sheet()` historical import, `write_sheet()` summary write-back | No routine student submit path found | Left unchanged; SQLite is the active data layer and Sheets is summary/import only. |
| coordination-games | `apps/coordination-games/app.R` | `backup_to_sheets()`, `backup_db_to_drive()` | No student submit path found | Admin backup buttons remain synchronous; session-end backup has an explicit best-effort comment. |
| flex_pass_actions | `apps/flex_pass_actions/app.R` | Sheets backup/admin sync, Drive backup/restore, session-end backup | Submit actions are DB-only; session end can still sync | Added comment that session-end backup is best-effort and submit actions stay DB-only. Admin backup/restore remains explicit and synchronous. |
| flex_pass_actions | `apps/flex_pass_actions/helpers.R` | Legacy Drive/Sheets backup helpers | Helper-level only | Left helper behavior unchanged; callers control when it runs. |
| price-index | `apps/price-index/app.R` | `backup_async()` on logout/session end/manual button | No, uses async backup | Added comment on session-end async backup; price submissions remain DB-only. |
| supply-auction-game | `apps/supply-auction-game/app.R` | Optional Drive backup after accept/admin actions/session end | Yes, accept button had a debounced synchronous backup | Kept DB write first, documented best-effort debounce, and increased accept backup debounce from 10s default to 60s. Admin/manual backups remain explicit. |

Remaining hot-path caveat: `supply-auction-game` still may run a synchronous
Drive backup after an accept if Drive backup is enabled, the DB changed, and
the 60-second debounce window has elapsed. This preserves the existing backup
behavior while making it much less frequent during rapid classroom clicks.
