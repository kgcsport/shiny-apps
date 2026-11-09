# Pass Insurance classroom run sheet

Pass Insurance adds single-use passes to the existing job market. It does not change bidding, job draws, cold-call wages, ordinary answers, board work, absences, or the student's standing option to pass live.

## Contract

- A student buys at most one policy for the selected class date through **Spend → Pass Insurance**.
- Students can buy during class while sales are open. Close sales manually or settle the market to end purchases.
- If an insured student is cold-called and chooses **Pass & Redraw**, the policy pays its stated benefit immediately. A policy pays at most once.
- Passing without insurance remains allowed and pays zero.
- Premiums, claims, and dividends change spendable token balances but never gross participation earnings.
- Settlement ends coverage for the selected class period. Positive profit after paid claims is divided equally among every active, non-admin, non-demo student in the selected course/section scope. A loss produces no negative dividend.
- The suggested next premium is cumulative claims cost divided by cumulative policies sold in that scope, rounded to the selected increment. This is a zero-profit pricing rule, not a claim that the price is actuarially fair for each student.

## Instructor workflow

1. Select the course, section, and class date as usual.
2. In **Settings → Spending Options**, make sure student spending is enabled.
3. In **Settings → Spending Options → Pass Insurance**, choose the benefit, starting premium, and repricing increment. Click **Create / Open Sales**.
4. Students buy from **Spend → Pass Insurance** while sales are open. The pass appears immediately in Account and Spend and applies only to the selected class period.
5. Run cold calls normally. When a called student passes, click **Pass & Redraw** in either Live Tracker or the slide panel. The pass is recorded, any eligible claim is paid, and a different student is drawn when possible.
6. After class, click **Settle, Dividend & Reprice**. The result is frozen, profit after paid claims is returned as equal class dividends, and the next zero-profit pooled premium is displayed.

Premium and benefit are frozen after the first purchase. Settlement is idempotent: repeated clicks cannot pay another claim or dividend.

## Four-class sequence

- **Class 1:** Open the initial pooled contract. Do not distinguish students by expected pass risk.
- **Class 2:** Use the first suggested premium. Let students observe who remains in the pool and how often coverage is used.
- **Class 3:** Use the next suggested premium. Keep the contract and ordinary class routine otherwise unchanged.
- **Class 4:** Use the final iteration, then run a separate class poll on the preferred response to selection (for example, mandatory participation, a subsidy, or differentiated pricing). The existing polling tools can run the vote; insurance settlement remains in Class Job Market.

For discussion, distinguish selection into the pool from behavior after buying coverage. The app exposes purchasers, claims, revenue, claim cost, pre-dividend profit, equal dividends, net profit, and the suggested next premium without assigning those theoretical labels to students in advance.
