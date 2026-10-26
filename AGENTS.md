# Project instructions

This repository is for building shiny apps for teaching undergraduate economics.

Before substantive work, read:

`SHINY_APPS_PLAN.md`

Treat that file as the source of truth for project status and sequencing.

When completing work:

1. Work from `Next actions` unless explicitly instructed otherwise.
2. Inspect existing infrastructure before creating new systems.
3. Test/build/render outputs before marking tasks complete.
4. Update relevant checkboxes in `SHINY_APPS_PLAN.md`.
5. Record important decisions in its Decision Log.
6. Add a brief Work Log entry.
7. Replace `Next actions` with the next 1-3 concrete tasks.
8. Put unresolved substantive course decisions under `Questions for Kyle`.

Important:

- Do not silently change substantive economics, grading policies, assignment
  policies, or course content.
- Prefer existing infrastructure and minimal dependencies.
- Avoid making feature branches unless requested. If in doubt, a

## Non-regression protocol for Class Job Market

For any change to apps/class-job-market, treat current instructor and student workflows as a compatibility contract. Before implementation, record: (1) behavior that must remain unchanged, (2) requested new behavior, and (3) concrete verification cases.

Required verification:

1. Add or update a regression test for every production defect. Use a realistic SQLite fixture when state spans dates, sections, bidding modes, or historical records.
2. Test the exact workflow path, not only helper functions or startup. A swallowed database error must be surfaced by the test.
3. Run the focused regression test plus relevant smoke tests before deployment.
4. Stage the immutable image and manually verify the affected instructor and student workflow before production promotion.
5. Do not call a change resolved until the tested behavior, test command, staging image, and deployment commit are stated.

Critical invariants: unresolved assignments and pending scores remain visible and actionable across class dates; course or section scope may filter records but must not strand them; data migrations preserve historical records; and destructive actions require confirmation plus a recovery path.
