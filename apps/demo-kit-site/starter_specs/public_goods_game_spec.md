# Public Goods Game Starter Spec

## Activity Overview

Build a local classroom public goods game using fake data. Students choose contributions to a group account. The instructor controls rounds and reveals outcomes.

## Minimal Viable Version

- Load fake roster and group assignments.
- Let fake students submit contributions for one round.
- Let the instructor set endowment and multiplier.
- Calculate group totals and payoffs.
- Show a public summary without individual private choices.
- Export raw events and round summary as CSV.

## Data Model

- `participants`: participant_id, display_name, section, active
- `groups`: group_id, participant_id, session_id
- `sessions`: session_id, date, course, activity, notes
- `choices`: timestamp, session_id, round_number, participant_id, group_id, contribution
- `outcomes`: session_id, round_number, group_id, group_total, multiplier, payoff_rule

## Screens Needed

- Setup/configuration screen
- Student contribution screen
- Instructor controls screen
- Public results display
- Data export/reset screen

## Testing Requirements

Test missing contributions, invalid values, payoff formula, refresh behavior, reset behavior, and CSV export.

## Export Requirements

Export raw contribution events and calculated round summaries. Do not store real names or emails.
