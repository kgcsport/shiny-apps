# Future: self-serve multi-tenant platform on Reclaim Cloud

Status: not started — planning placeholder on branch `feature/multi-tenant-self-serve`.

## Goal

Let other professors run their own classes on Kyle's single Reclaim Cloud
deployment, with **minimal setup input** from the professor. Today's setup
requires per-deployment Google Cloud plumbing that only makes sense for one
person running one instance — it does not scale to "many professors, one
shared server."

> Direct ask: "It should require minimal inputs by the prof using the
> system -- I had way too much extra crap."

## Why today's model doesn't scale

Every app reads its config from environment variables. A single instance
today needs all of these set:

| Env var | Used by | Purpose |
|---|---|---|
| `GOOGLE_SERVICE_ACCOUNT_JSON` / `GOOGLE_APPLICATION_CREDENTIALS` | class-job-picker, coordination-games, flex_pass_actions, price-index, supply-auction-game | Google service-account auth |
| `FLEX_PASS_SHEET_ID` | coordination-games, flex_pass_actions | Credentials Google Sheet |
| `FLEX_PASS_FOLDER_ID` | coordination-games, flex_pass_actions, price-index, supply-auction-game | Drive backup folder |
| `AUCTION_FOLDER_ID` | supply-auction-game | Drive backup folder |
| `CLASS_JOB_SHEET_ID` | class-job-picker | Summary write-back sheet |
| `PUB_ECON_FOLDER_ID`, `FINALQ_SHEET_ID`, `CRED_CSV`, `CRED_PATH` | flex_pass_actions | Legacy credential sources |
| `DB_PATH_OVERRIDE` | coordination-games | DB path override |
| `SHINY_PASSWORD` | bonus-entry, class-job-picker, club-insurance-game | Shared admin password |
| `CONNECT_CONTENT_DIR` | nearly every app | Where the shared `data/` dir lives |

A second professor would need their **own** Google Cloud project, service
account, credentials Sheet, and Drive folder just to get an instance running.
That's the "extra crap" to design away.

## Target shape (rough — not final)

- One shared Reclaim Cloud deployment, one shared DB (or one DB per tenant —
  TBD), serving multiple classes.
- A `class`/`tenant` concept: every row in `users`, `sections`, `ledger`,
  `job_log`, etc. scoped by `class_id`. This extends the same pattern as the
  `active` flag added for archiving prior semesters (see git history on
  `main` around the `final_question_reveal` → `flex_pass_actions` rename) —
  every per-app login query and admin dropdown needs the same `class_id`
  scoping treatment across all 7 apps that touch `users`.
- Professor onboarding without Google Cloud: sign up → name a class → get an
  admin login, instantly. No service account, no Sheet ID, no Folder ID.
- Backups become Kyle's own ops concern (one centralized backup job for the
  whole server), not a per-professor Google Drive integration.
- Each professor's admin view/queries scoped to their own class only — must
  not see or manage another professor's roster.

## Open questions to resolve before building

- SQLite (file-per-tenant?) vs. moving to Postgres for safer concurrent
  multi-tenant writes.
- How a new professor actually signs up — invite-only (Kyle approves) vs.
  fully self-serve.
- What happens to the existing Google Sheets/Drive-based credential and
  backup flow for Kyle's own classes — keep as a legacy path, or migrate
  everyone (including Kyle) to the new model?
- Whether `SHINY_PASSWORD`-style shared admin passwords (bonus-entry,
  club-insurance-game) need the same per-class scoping or can stay global.

## Non-goal for now

This file exists to hold the idea so it isn't lost — no code changes yet.
Pick this up deliberately, scope a first milestone, and confirm the design
direction before implementing.
