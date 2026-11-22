# Multi-Course Class-Job Market: Corrected Mechanism Draft

Date: 2026-11-12

Status: design draft for instructor review. This changes neither the live app
nor any course policy.

## Existing Economic Loop

The app already has the intended structure:

1. Students submit the minimum token wage they require for a posted job.
2. In wage-bidding mode, the lowest bidders receive the job. The instructor can
   use pay-as-bid or a uniform reverse-second-price wage.
3. Completing the job earns tokens.
4. Students spend tokens on class perks.
5. Gross earned tokens can determine participation credit; spending reduces
   the wallet balance but never erases participation credit already earned.

The perk menu gives the token purchasing power, so wage bids reveal the token
compensation students require for jobs. No job-buyout product belongs in this
mechanism.

The first intended setting is a larger introductory course taught in two
sections. A possible second setting is a 200-level data-science course. The
mechanism should be common across courses, with section-scoped markets and
course-specific job and perk menus.

## Token Numeraire

Do not convert tokens into exam points or final-grade points. Anchor their value
in a stable, announced menu of class perks.

Use the two ledgers already in the app:

- **Gross earned tokens:** determine participation credit, if participation is
  graded.
- **Spendable balance:** buys perks and falls when a purchase is made.

Spending one token on one perk means it cannot be spent on another perk. Gross
participation accounting is deliberately outside that consumption tradeoff so
students are not penalized for using the market.

For bids to be interpretable, students need:

- at least two valued token uses;
- prices and eligibility rules announced before wage bidding begins;
- a perk menu available throughout the bidding period;
- no surprise token inflation or discretionary ex-post prices; and
- a meaningful late-semester use, so token value does not collapse near the
  end of the course.

A clean nominal anchor can be the posted cost of a 24-hour automatic extension.
It is a reference price, not a token-to-grade conversion. Calibrate prices from
expected token earnings and the number of perks a student can reasonably buy.

Do not carry ECON 342's 25-token full-credit target automatically into Intro or
Data Science. Calibrate each course after specifying job supply, typical wages,
and perk prices.

## Perk Menu

The app currently offers:

- assignment extensions;
- grade-reweight requests;
- pooled candidate-question reveals; and
- Pass Insurance.

For the first Intro pilot, the cleanest core is:

1. **Automatic assignment extensions.** These provide an individualized
   benefit, have visible prices, and avoid excuses or negotiation.
2. **Candidate-question funding or another section public good.** This creates
   a collective use and a different willingness-to-pay margin.

Grade reweighting can be a later or tightly capped perk because it is most
directly tied to final grades. It is not needed to give tokens value.

Required materials, ordinary help, normally released solutions, and
accessibility accommodations must never depend on tokens.

Publish the purchase calendar at launch and retain a genuine perk after the
last ordinary job market. Candidate review questions or another capped review
public good can fill this role. Otherwise the last tokens will rationally lose
value and wages will drift for a mechanical reason.

Unspent tokens can expire without grade loss. Announce any terminal use at the
start rather than adding a surprise redemption later.

## Insurance

Pass Insurance is a weak insurance product when students already retain a free
option to pass. There is little underlying loss, so the token claim behaves
more like a bet.

Problem-set lateness creates a more meaningful risk, but extension insurance
would introduce claim definitions, strategic delay, solution-release
conflicts, and the tedious explanations the automatic extension product avoids.

Recommendation:

- retire Pass Insurance from this experiment;
- keep problem-set extensions as deterministic purchases, not insurance; and
- use a separate activity with an explicit random loss to teach insurance.

The existing extension workflow is the clean version: students buy time before
the cutoff, the system applies it, and nobody pleads a case.

## Job Auction

Use the app's uniform reverse-second-price rule for the pilot:

1. Post the authentic number of slots for each job.
2. Students submit minimum acceptable wages.
3. Assign slots to the lowest bidders.
4. Pay each winner the first losing bid, with the configured wage as the
   no-loser fallback.

This gives a market-clearing wage for the announced quantity and reduces the
need to shade bids merely to predict payment.

Do not call the raw median bid an equilibrium wage unless demand is actually
about half the bidders. If a more stable displayed wage is desirable, use the
job's clearing wage or a pre-announced moving median of recent clearing wages.

Clear the two Intro sections separately because students cannot work in the
other section and job demand can differ. Use the same auction rule and starting
perk prices. Report section wages separately; two sections alone do not support
a credible treatment comparison.

## Volunteering And Cold Calls

The app sensibly links volunteer pay to the frozen cold-call wage: students bid
once for the underlying contribution type. The unresolved problem is quantity.
If every volunteer is paid without rationing, confident students can repeatedly
supply at the posted wage and capture the token market.

Recommended rule:

- At the start of class, post the frozen wage derived from the relevant
  cold-call/job auction.
- Announce a limited number of **paid live contributions** needed that day,
  separately for answers and board work if useful.
- Pay at most one voluntary live contribution per student per class.
- When volunteers exceed remaining paid slots, select among them randomly,
  prioritizing students with fewer prior paid live contributions.
- Students may continue contributing after paid slots fill, but ordinary
  discussion contributions are unpaid.
- Announce any special bounty before the task, never after seeing the supplier.

This preserves open discussion while preventing paid participation from being
cornered. It makes the wage meaningful because both price and compensated
quantity are specified.

The app should display remaining paid volunteer slots. A volunteer queue or
instructor draw is cleaner than paying whoever speaks first.

## Two-Week Feedback Phase

During the first two weeks there is no bidding, buying, selling, or payout.
Students experience representative jobs and receive one outcome plus at most
two diagnostic tags.

| Outcome | Operational meaning |
|---|---|
| `Success` | Met the job criterion and produced something useful. |
| `Try` | Made a genuine attempt that met part, but not all, of the criterion. |
| `Fail` | Responded, but the output did not perform the assigned job. |
| `Miss` | Made no usable attempt. An excused absence is separate. |

`Try` and `Fail` need job-specific examples. Collapse them if the distinction
cannot be applied consistently.

Diagnostic tags:

- `Get to the point`
- `Nice summary`
- `Lead with the claim`
- `Show the mechanism`
- `Use evidence`
- `Make the connection`
- `Name the assumption`
- `Give an example`
- `Overthinking it`
- `Structure it first`
- `Speak up / slow down`
- `Prepare more`

Tags describe work, not students. Students see their own histories. In a large
class, record whether an attempt was directly observed, reported from a small
group, or submitted as an artifact.

After week two, open the real market without a strategy primer. Students still
need operational instructions: a bid is their minimum acceptable wage, low
bids are more likely to win, how uniform payment works, what completion means,
and what perks tokens buy. Use a short comprehension check without suggesting
a bid.

## Course-Specific Jobs

Intro candidates:

- previous-class recap;
- graph interpreter;
- problem setup or solution explainer;
- reading/example summarizer;
- assumption spotter;
- evidence-versus-opinion checker;
- discussion connector; and
- muddiest-point curator.

Data Science candidates:

- live-code driver;
- debugger;
- code reviewer;
- reproducibility checker;
- data-documentation steward;
- visualization critic;
- results interpreter;
- test-case designer; and
- data-ethics or provenance checker.

Data Science offers more artifact-producing, non-speaking jobs. Completion
criteria should reward reproducible work rather than comfort at the front.

## Pilot Sequence

1. **Weeks 1-2:** random exposure and feedback only; show the future perk menu
   and prices but keep it inactive.
2. **Week 3:** activate sealed minimum-wage bids, uniform pricing, payouts, and
   the announced perk menu.
3. **Weeks 3-6:** hold auction rules and perk prices fixed.
4. **After week 6:** review wages, fill rates, completion, volunteer
   concentration, perk purchases, balances, and confusion before changing
   parameters.

Students do not need free tokens before the first payout. Seeing the perk menu
and prices before bidding is enough to value the wage unit.

## Data To Preserve

For every course, section, class, and job:

- posted slots and fallback wage;
- each sealed minimum-wage bid and submission time;
- allocation, pricing rule, clearing wage, and tie-break;
- completion outcome and payout;
- feedback outcome and diagnostic tags;
- paid volunteer slots announced and remaining;
- paid and unpaid live contributions by student;
- gross earnings and spendable balance;
- perk prices, purchases, contributions, approvals, refunds, and delivery; and
- instructor overrides with a reason.

Core diagnostics are job-specific wage distributions, within-student bid
changes, fill and completion rates, earning concentration, perk demand,
late-semester wage drift, and differences between Intro sections.

If records will support generalizable research rather than teaching and course
improvement alone, settle institutional review, consent, privacy, and grade
separation before launch.

## Decisions Before App Work

1. Approximate size and meeting pattern of each Intro section.
2. Which two or three perks form the launch menu and their initial prices.
3. Whether grade reweighting remains available in Intro.
4. Normal paid answer and board-work quantities per class.
5. Whether `Fail` remains distinct from `Try`.
6. Whether Data Science launches simultaneously or follows later.
7. Whether the data are for teaching only or intended as research.

## Implementation Boundary

The app already has the essential bid, allocation, payout, wallet,
participation-grade, and perk infrastructure. Likely additions are:

- a two-week feedback-only mode with outcomes and tags;
- course-specific job and perk configuration;
- explicit paid-volunteer quantities, caps, and a fair queue;
- stable late-semester perk scheduling; and
- exports joining bids, wages, feedback, earnings, and perk purchases.
