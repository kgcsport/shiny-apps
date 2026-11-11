# Insurance games: classroom rehearsal

Run this once against staging after a deployment and again from the classroom
network before the first live use. Use an instructor laptop, a student phone or
second browser, and a second student account in another section. Do not use the
production class until the staging pass is clean.

## Automated gate

From the repository root:

```sh
Rscript --vanilla tests/critical-insurance-market.R
Rscript --vanilla tests/risk-insurance-game.R
```

Both commands must end in `PASS`. They exercise the real Shiny observers and a
disposable shared SQLite database, including duplicate clicks, fresh browser
sessions, late purchases, repeat passes, repeat settlement, section isolation,
and token-ledger reconciliation.

## Five-minute preflight

- Confirm the instructor and both student accounts show the intended class and
  section. The other-section account must not see the active round.
- Confirm all devices can load Class Job Market and Risk & Insurance over the
  classroom Wi-Fi. Keep the instructor view open on the projector separately.
- Record the three students' starting token balances. Use a small test policy
  and a new test round/date so every resulting ledger row is easy to identify.

## Pass Insurance in Class Job Market

1. Open sales with a 2-token premium and 2-token benefit. Student A buys and
   taps **Buy policy** twice. Expected: one purchase and one 2-token debit.
2. Refresh Student A's browser. Expected: the purchased policy remains visible
   and the balance remains debited only once.
3. Student B buys. Close sales. Student C attempts to buy. Expected: Student C
   is rejected without a debit.
4. Draw Student A and choose **Pass & Redraw**. Expected: a 2-token insurance
   payment and a new cold-call draw. Force/draw Student A again and pass again.
   Expected: the second pass is recorded but the policy does not pay twice.
5. Draw insured Student B and choose **Recession — Fire & Redraw**. Expected:
   a 2-token insurance payment, a persisted zero-token recession firing, and an
   immediate redraw that does not select Student B.
6. Settle, then click settle again. Expected: one dividend per active in-scope
   student, including Student C if profit is positive; no other-section payment;
   no duplicate dividend; all premium, claim, and dividend rows are non-earning.
7. Refresh the instructor and student browsers. Expected: settlement, balances,
   and audit history survive the new sessions.

## Risk & Insurance

1. Activate Risk & Insurance and open a percent-loss round with a 50% bad-state
   probability. Student A saves an insured choice; Student B saves an uninsured
   choice. Student A taps **Save choice** twice and refreshes. Expected: one
   saved choice per student, with the saved choice restored after refresh.
2. Confirm the other-section account cannot submit to the round. Close the
   round and attempt another student save. Expected: no new or changed choice.
3. Settle, click settle again, and reveal. Expected: one outcome per submitted
   student, one set of token cashflows, and the projector changes from sealed
   totals to revealed results only after **Reveal**.
4. On two student devices, confirm the shown net change equals the change in
   token balance. Confirm the instructor's total ledger cashflow equals the sum
   of student net changes.
5. Repeat with a fixed-token-loss round and **Apply premiums and losses to real
   token balances** off. Expected: outcomes and analytics appear, but no token
   balances or ledger rows change.

## Stop conditions and recovery

Stop the activity before settlement if the scope, premium, benefit, probability,
or token toggle is wrong. Do not compensate students by clicking again. Preserve
the round and ledger rows, note the class/section, round/date, affected user, and
time, and use an audited Token Admin correction only after identifying the
missing or duplicate source row. A page refresh is safe; creating a replacement
round is preferable to reopening or rewriting a closed contract.

## Pass criteria

The games are classroom-ready only when the automated gate passes and the live
rehearsal shows: no cross-section records, no duplicate debits or payouts, state
survives refresh/reconnect, closed choices stay closed, settlement is idempotent,
and every insurance cashflow is excluded from participation earnings.
