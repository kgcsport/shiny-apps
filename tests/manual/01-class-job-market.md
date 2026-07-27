# Manual Test Script — Class Job Market

**App:** `apps/class-job-market`  
**Shared DB:** `class-job-market.sqlite` (all auth and token_ledger data lives here)

---

## Setup

| Item | Value |
|---|---|
| Admin user | `instructor` / `<admin password>` |
| Student A | `alice` / `test123` |
| Student B | `bob` / `test123` |
| Student C | `carol` / `test123` |
| Section | `S01` |

Seed users via the admin panel or directly in SQLite before starting.

---

## TC-01 — Admin: create a round and post jobs

**Precondition:** Admin is logged in.

1. Open the **Job Market** tab.
2. Click **New Round** — enter label `Week 1` → Save.
3. On the **Job Categories** tab, add category `Note Taker` with 2 slots and wage 5.0.
4. In the round view, click **Post Jobs** and add 2 `Note Taker` slots.
5. Set round status to **Open**.

**Expected:**
- Round `Week 1` appears in the round list with status `open`.
- 2 `Note Taker` job posts are visible to students.

---

## TC-02 — Student: submit a wage bid

**Precondition:** TC-01 complete. Alice is logged in as a student.

1. Open the **Bid** tab.
2. Select category `Note Taker`.
3. Enter wage `4.50` → Submit bid.

**Expected:**
- Confirmation message "Bid saved".
- Bid appears in Alice's bid summary with wage 4.50.

4. Change bid to `3.00` and resubmit.

**Expected:**
- Previous bid is replaced. Only one bid per student per category per round.

---

## TC-03 — Student: submit an application bid (ranked preference)

**Precondition:** TC-01 complete. Bob is logged in.

1. On the **Apply** tab, drag `Note Taker` to rank 1.
2. Submit.

**Expected:** "Application saved" — rank 1 for `Note Taker` stored.

---

## TC-04 — Admin: clear wage market and assign jobs

**Precondition:** TC-02 and TC-03 complete. Admin is logged in.

1. Open round `Week 1` → **Clear Market** → confirm.
2. Check the **Assignments** panel.

**Expected:**
- The 2 highest-wage bidders fill the 2 slots.
- Market wage = the lower of the two accepted bids.
- Students below the market wage are not assigned.

---

## TC-05 — Admin: award tokens for job completion

**Precondition:** TC-04 complete. Alice is assigned as Note Taker.

1. In **Assignments**, find Alice's row.
2. Set outcome to `Complete` and token award to `5.0` → Close assignment.

**Expected:**
- `token_ledger` row created: `source_type='job_assignment'`, `earning=1`, `amount=5.0`.
- Alice's balance shows `lifetime_earned=5.0`, `spendable_balance=5.0`.

---

## TC-06 — Student: view token balance

**Precondition:** TC-05 complete. Alice is logged in.

1. Open **My Tokens** tab.

**Expected:**
- Lifetime earned: 5.0
- Spendable balance: 5.0
- Transaction history shows one row: `job_assignment` for 5.0.

---

## TC-07 — Student: spend tokens on extension

**Precondition:** TC-06 complete. Alice has 5.0 spendable.

1. Open **Extensions** tab.
2. Select problem set `PS1` → Cost: 3.0 tokens → Confirm purchase.

**Expected:**
- `token_ledger` spend row: `earning=0`, `amount=-3.0`, `source_type='extension_purchase'`.
- Spendable balance drops to 2.0. Lifetime earned stays at 5.0.

---

## TC-08 — Student: spend tokens fails when balance is insufficient

**Precondition:** Alice has spendable balance 2.0 (from TC-07).

1. Attempt to purchase an extension costing 5.0 tokens.

**Expected:**
- Error notification: "Insufficient spendable balance."
- No ledger row added.

---

## TC-09 — Admin: live participation event (wage award)

**Precondition:** Admin is logged in. Carol is a student with 0 tokens.

1. Open **Live Tracker** tab.
2. Select student `carol` and event type `cold_call`.
3. Enter wage `2.0` → Award.

**Expected:**
- `token_ledger` row: `source_type='live_participation'`, `earning=1`, `amount=2.0`.
- Carol's balance shows 2.0.

---

## TC-10 — Admin: token ledger audit

**Precondition:** TC-05, TC-07, TC-09 complete.

1. Open **Token Ledger** tab (admin only).

**Expected:**
- Table shows all transactions across all students.
- Alice: earning 5.0 (job), spending -3.0 (extension) → net 2.0.
- Carol: earning 2.0 (participation) → net 2.0.

---

## TC-11 — Admin: download token_ledger CSV

1. Click **Download → token_ledger.csv**.

**Expected:**
- CSV downloads with columns: `user_id, display_name, source_type, amount, earning, note, created_at`.
- Values match the audit table.

---

## TC-12 — Public goods game (if enabled)

**Precondition:** Admin has enabled public goods module.

1. Admin opens a public goods round.
2. Alice contributes 2 tokens.
3. Admin closes the round and distributes payouts.

**Expected:**
- Contribution rows with `earning=0, amount=-2.0` for Alice.
- Payout rows with `earning=1` for all participants.

---

## TC-13 — Section filter

**Precondition:** Students in sections S01 and S02 both have bids.

1. Admin selects section `S02` in the section filter.

**Expected:**
- Only S02 students' bids and assignments are visible.
- Token balances shown are section-scoped.

---

## TC-14 — Admin: CSV export of assignments

1. On the **Assignments** tab, click **Export CSV**.

**Expected:** CSV with round, user_id, display_name, category, wage, tokens, outcome.
