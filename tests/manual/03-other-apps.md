# Manual Test Script — All Other Apps

Covers: coordination-games, review-quiz, price-index, supply-auction-game,
class-job-picker, bonus-entry, club-insurance-game, theory-of-firm, excise-tax-game,
sloman-trading-game, airplanes-game.

**Shared auth:** All apps that require login use `class-job-market.sqlite`. Seed
users via class-job-market admin panel before running these tests.

---

## Coordination Games (`apps/coordination-games`)

### CG-01 — Student login

1. Open app. Enter credentials for student Alice.

**Expected:** Welcome message with name and section. Student UI shows current game type and round status.

### CG-02 — Price War (PD): submit action

**Precondition:** Admin has set game=`pd`, status=`open`.

1. Log in as Alice. Select **High** and submit.
2. Log in as Bob (different browser/incognito). Select **Low** and submit.

**Expected:**
- Both see "Saved" confirmation.
- Admin sees 2 submissions in the round view.

### CG-03 — Admin: close PD round and apply payouts

1. Admin sets status → `closed`.
2. Admin sets status → `revealed`.
3. Admin clicks **Apply payouts to token ledger**.

**Expected:**
- Alice (High vs Bob's Low): payoff 10 pts × pd_scale = 1.0 token
- Bob (Low vs Alice's High): payoff 70 pts × pd_scale = 7.0 token
- `token_ledger` rows: `source_type='coordination_grant'`, `round_id=<round>`, `earning=1`
- Verify in class-job-market's token ledger audit.

### CG-04 — Bonus Pot: student contribution

**Precondition:** Admin has set game=`bonus`, status=`open`. Alice has at least 3 tokens.

1. Alice logs in. Slider shows current balance.
2. Slide contribution to 2.0 → Submit.

**Expected:**
- Submission saved (no debit yet — debit happens on close).
- Alice can change contribution while round is open.

### CG-05 — Bonus Pot: admin closes and reveals

1. Admin sets status → `closed`.
   - **Expected:** Contribution debits post to `token_ledger` (`source_type='coordination_contrib'`, `earning=0`, `amount=-2.0` for Alice).
2. Admin sets status → `revealed`.
3. Admin clicks **Apply payouts to token ledger**.
   - **Expected:** Payout credits for each student in section: `pot / class_size`, rounded to 0.5.

### CG-06 — Contribution exceeds balance is blocked

**Precondition:** Alice has 1.0 spendable balance.

1. Alice tries to set contribution slider to 3.0 → Submit.

**Expected:** Error "Not enough participation tokens." Submission rejected.

### CG-07 — Admin: clear round reverses debits

1. With contribution debits posted, admin clicks **Clear (this round + section)**.

**Expected:**
- `token_ledger` rows for `coordination_contrib` in this round are deleted.
- Alice's spendable balance is restored.

### CG-08 — Admin: switch section

1. Admin switches active section from `S01` to `S02`.

**Expected:** Submissions and payouts scoped to S02 only.

---

## Review Quiz (`apps/review-quiz`)

### RQ-01 — Student: answer a question

**Precondition:** Admin has created questions and set one as active.

1. Student logs in → sees current active question.
2. Student selects answer and submits.

**Expected:** Confirmation shown. Cannot resubmit same question (form locks or warns).

### RQ-02 — Leaderboard alias

1. Student sets a display alias on the **Leaderboard** tab.

**Expected:** Alias stored in `quiz_aliases` table. Leaderboard shows alias, not real name.

### RQ-03 — Admin: advance question

1. Admin logs in → **Control** tab.
2. Clicks **Next Question**.

**Expected:** Active question increments. Student views update to new question.

### RQ-04 — Admin: submit a question for review

1. Student opens **Submit Question** tab.
2. Fills in question text and answer options → Submit.

**Expected:** Row inserted in `quiz_submissions` with status `pending`. Visible to admin.

### RQ-05 — Admin: approve submitted question

1. Admin opens **Submissions** tab.
2. Finds the pending question → **Approve**.

**Expected:** Question added to `quiz_questions` pool.

### RQ-06 — Auth: wrong password rejected

1. Student enters wrong password.

**Expected:** "Login failed" — no session created.

---

## Price Index (`apps/price-index`)

### PI-01 — Student: add basket item

1. Log in as student. Open **My Basket** tab.
2. Add item: name `Coffee`, store `Starbucks`, category `Beverages`, frequency `5 times/month`.
3. Submit.

**Expected:** Item saved in `basket_items`. Appears in basket list.

### PI-02 — Student: record a price

**Precondition:** Item `Coffee` is in basket (PI-01).

1. Open **Record Prices** tab. Select current wave.
2. Enter price `6.50` for Coffee from Starbucks.
3. Submit.

**Expected:** Row in `price_records`. Item price stored.

### PI-03 — Update basket item (ON CONFLICT upsert)

1. Re-add the same item with a new category → Submit.

**Expected:** Existing basket item is updated, not duplicated. "Saved" notification (not "added").

### PI-04 — Admin: advance wave

1. Admin opens **Admin** tab → **Advance Wave** → confirm.

**Expected:** `app_state.current_wave` increments. Previous wave's prices are locked.

### PI-05 — Admin: view class index

1. Open **Class Price Index** tab.

**Expected:** Table shows average price by category and wave. Index calculated from base wave.

### PI-06 — Admin: export DB backup

1. Admin clicks **Backup DB to Drive** (or download button).

**Expected:** Backup completes without error. Filename includes timestamp (`shared_db_<timestamp>.zip`).

---

## Supply Auction Game (`apps/supply-auction-game`)

### SA-01 — Student login

1. Open app. Log in with class-job-market credentials.

**Expected:** Student sees current auction state (price, units remaining).

### SA-02 — Student: accept supply at current price

**Precondition:** Admin has started an auction with a falling price.

1. When price reaches a level Alice wants, click **Accept**.

**Expected:**
- Acceptance recorded in `accepts` table with `round`, `user_id`, `price`.
- Alice's button disables or changes to "Accepted".

### SA-03 — Student: cannot accept twice in same round

1. After Alice has accepted, she tries to accept again.

**Expected:** Second acceptance blocked or ignored. Only one row per student per round.

### SA-04 — Admin: start new auction round

1. Admin opens **Controls** tab.
2. Sets item description, starting price, and tick interval.
3. Clicks **Start Auction**.

**Expected:**
- Price begins falling at the configured interval.
- Student views update in real time.

### SA-05 — Admin: stop auction

1. Admin clicks **Stop Auction** mid-round.

**Expected:** Price stops falling. Current acceptances are preserved.

### SA-06 — Admin: export results

1. Admin clicks **Export Results CSV**.

**Expected:** CSV with columns: `round, user_id, display_name, price, accepted_at`.

---

## Class Job Picker (`apps/class-job-picker`)

### JP-01 — Admin login and section setup

1. Log in as admin.
2. On **Settings** tab, ensure section `S01` is active and roster shows enrolled students.

**Expected:** Students from `class-job-market.sqlite` users table are listed.

### JP-02 — Job draw for the day

1. Admin opens **Draw Jobs** tab.
2. Select date and section S01.
3. Click **Draw**.

**Expected:**
- Each draw job (Last Class Summary, Materials Summary, Note Taker) is assigned to a student.
- Assignment is logged in `job_log`.
- Weighted draw: students with fewer prior jobs are more likely to be drawn.

### JP-03 — Absent students excluded from draw

1. Mark `Alice` as absent.
2. Click **Draw**.

**Expected:** Alice does not appear in any draw result.

### JP-04 — Max commits cap exclusion

**Precondition:** Bob has been assigned 10 jobs (at the configured max).

1. Draw for today.

**Expected:** Bob is excluded from draw (overcap exclusion). Other students fill all jobs.

### JP-05 — Materials summary rotation fairness

**Precondition:** Alice has done Materials Summary 3 times; others have done it 1 time.

1. Draw for today.

**Expected:** Alice is excluded from Materials Summary draw until others catch up.

### JP-06 — Volunteer / cold call assignment

1. Admin opens **Cold Call** tab.
2. Selects a student manually → Record.

**Expected:** Row inserted in `job_log` with `job='cold call'`.

### JP-07 — Undo last draw entry

1. After a draw, admin clicks **Undo Last**.

**Expected:** The most recent `job_log` row for today is deleted. Student is available for redraw.

### JP-08 — View history

1. Open **History** tab. Select date range.

**Expected:** Table shows all job assignments in that range, grouped by date and section.

### JP-09 — Admin: manual override — assign specific student to job

1. On **Manual** tab, select job `Note Taker` → select `Carol` → assign.

**Expected:** `job_log` row inserted. Carol shows as assigned for that job today.

---

## Bonus Entry (`apps/bonus-entry`)

### BE-01 — Admin password required

1. Open app. Try to access admin section without a password.

**Expected:** Password prompt shown. Blank password rejected with "Admin login not configured."

### BE-02 — Admin login with correct password

1. Enter the correct `BONUS_ENTRY_PASSWORD` (set via env var).

**Expected:** Admin panel unlocks.

### BE-03 — Submit a bonus entry (student view)

1. Student fills in name and score → Submit.

**Expected:** Entry recorded. Student sees confirmation.

### BE-04 — Admin: view all entries

1. Admin views the entries table.

**Expected:** All submissions visible. Can export to CSV.

---

## Club Insurance Game (`apps/club-insurance-game`)

### CI-01 — Instructor setup: set initial income and premium

1. Instructor logs in (password-protected). Sets `initial_income=10`, `premium=2`.

**Expected:** Settings saved. Student UI reflects income.

### CI-02 — Student join with token

1. Student opens app. Enters room token → Join.

**Expected:** Student appears in roster with starting balance = `initial_income`.

### CI-03 — Loss event triggered

1. Instructor triggers a loss event for selected students.

**Expected:** Affected students lose the configured loss amount. Uninsured lose full amount; insured lose only the deductible.

### CI-04 — Insurance purchase

1. Student clicks **Buy Insurance** before loss event.

**Expected:** Premium deducted from balance. Student marked as insured.

### CI-05 — Round reset

1. Instructor clicks **Reset Round**.

**Expected:** All balances reset to `initial_income`. Insurance status cleared.

---

## Theory of the Firm (`apps/theory-of-firm`)

### TF-01 — Default cost curves display

1. Open app with default parameters.

**Expected:** ATC, AVC, and MC curves all render without error. AVC is `NA` at Q=0 (no divide-by-zero point plotted).

### TF-02 — AVC starts from Q=1

1. In the plot, check the AVC curve.

**Expected:** No data point at Q=0. AVC begins at Q=1.

### TF-03 — Parameter sliders update curves live

1. Adjust the `B` (fixed cost) slider.

**Expected:** ATC shifts up/down. AVC unchanged. MC unchanged.

### TF-04 — Equilibrium text label

1. Check the equilibrium text below the chart.

**Expected:** Shows numeric value computed from `log10(abs(p$B))` — not a blank/error.

---

## Excise Tax Game (`apps/excise-tax-game`)

### ET-01 — Room creation

1. Open app. Click **New Room**.

**Expected:** 6-character room code generated. Code not already in use (retry logic runs up to 10 times).

### ET-02 — Student join room

1. Student enters room code → Join.

**Expected:** Student appears in room. Supply/demand game loads.

### ET-03 — Tax policy slider

1. Instructor changes tax rate slider.

**Expected:** Equilibrium price and quantity update. Deadweight loss area shown in chart.

### ET-04 — Data persistence across sessions

1. Instructor creates room with students. Closes browser. Reopens.

**Expected:** Room state persists (SQLite in `appdata_root(getwd())/data/`).

---

## Sloman Trading Game (`apps/sloman-trading-game`)

### ST-01 — Timer uses input value

1. Set timer to 3 minutes → Start.

**Expected:** Countdown shows "3:00" and begins decrementing.

2. Set timer to `abc` (invalid) → Start.

**Expected:** Timer defaults to 5 minutes (invalid input falls back to 5).

### ST-02 — Round badge shows current round

1. Start game at round 1.

**Expected:** Badge shows "Round 1" (not "Round 1 of 3").

2. Advance to round 2.

**Expected:** Badge shows "Round 2".

### ST-03 — Trade recording

1. Two students complete a trade.

**Expected:** Trade logged with price and quantity. Round summary table updates.

---

## Airplanes Game (`apps/airplanes-game`)

### AG-01 — DT cell edit: valid numeric

1. Open the editable table. Click a numeric cell. Enter `42`.

**Expected:** Value saved. Row updates to 42.

### AG-02 — DT cell edit: out-of-bounds value rejected

1. Click a cell that expects a value between 1–100. Enter `999`.

**Expected:** Error notification. Cell reverts to previous value. No crash.

### AG-03 — DT cell edit: non-numeric rejected

1. Enter `abc` in a numeric cell.

**Expected:** Error notification. Cell reverts. No crash.
