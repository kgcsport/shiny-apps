# Policy Calculator Starter Spec

## Activity Overview

Build a local classroom policy calculator using fake scenario data. Students or groups propose parameter values and compare calculated outcomes.

## Minimal Viable Version

- Provide a setup screen with baseline assumptions.
- Let users enter policy parameters.
- Calculate outcomes using visible formulas.
- Show a public comparison table or chart.
- Export raw scenario submissions and calculated outputs.

## Data Model

- `sessions`: session_id, date, course, activity, notes
- `scenarios`: timestamp, session_id, scenario_id, participant_id, parameters
- `outcomes`: session_id, scenario_id, outcome_name, outcome_value, formula_version

## Screens Needed

- Setup/configuration screen
- Scenario input screen
- Public comparison display
- Instructor controls screen
- Data export/reset screen

## Testing Requirements

Test invalid values, boundary values, formula changes, visible assumptions, export fields, and reset behavior.

## Export Requirements

Export raw scenario submissions and calculated outcomes. Preserve formula version or notes so results can be interpreted later.
