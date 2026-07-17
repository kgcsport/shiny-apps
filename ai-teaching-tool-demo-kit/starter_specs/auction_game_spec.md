# Auction Game Starter Spec

## Activity Overview

Build a local classroom auction tool using fake data. Students receive fake values and submit bids. The instructor closes the auction and reveals outcomes.

## Minimal Viable Version

- Load fake participants.
- Assign fake private values.
- Let students submit one bid each.
- Let the instructor close bidding.
- Calculate winner, price, and surplus.
- Export raw bids and summary results.

## Data Model

- `participants`: participant_id, display_name, section, active
- `sessions`: session_id, date, course, activity, notes
- `values`: session_id, round_number, participant_id, value
- `bids`: timestamp, session_id, round_number, participant_id, bid
- `outcomes`: session_id, round_number, winner_id, price, surplus, auction_format

## Screens Needed

- Setup/configuration screen
- Student bidding screen
- Instructor controls screen
- Public outcome display
- Data export/reset screen

## Testing Requirements

Test ties, missing bids, bids above or below allowed limits, price calculation, and whether private values stay hidden.

## Export Requirements

Export raw bid records, assigned values, and outcome summaries as CSV or SQLite tables.
