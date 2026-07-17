# Privacy Workflow

Use this workflow to build a classroom tool with AI without sharing student personally identifiable information.

## Rules

- Use fake rosters during development.
- Use placeholder IDs such as `S001`, `S002`, `S003`.
- Do not paste student names, emails, grades, accommodations, demographic information, or private notes into AI prompts.
- Keep the mapping between real students and local IDs outside the AI workflow.
- Store classroom data locally or on an instructor-controlled server.
- Export data in inspectable formats such as CSV, JSON, or SQLite.
- Delete test records before live use.
- Review generated code for unexpected external API calls.
- Turn off analytics or telemetry unless intentionally added.
- Document where data are stored.

## Practical Workflow

1. Describe the classroom activity generically.
2. Replace real students with fake participant IDs.
3. Use the fake CSV files in `fake_data/` while building and testing.
4. Ask the AI agent to write down where data are stored and how exports work.
5. Inspect the code for external network calls, analytics scripts, or telemetry.
6. Reset or delete all test records before live use.
7. If using real local data, keep the real roster on an instructor-controlled machine or server.

## Checklist

Before using AI:

[ ] I replaced real students with fake IDs.  
[ ] I removed names, emails, grades, and notes.  
[ ] I described the classroom logic generically.  
[ ] I did not upload a real roster.

Before classroom use:

[ ] I know where the app stores data.  
[ ] I tested with fake data.  
[ ] I can export the data.  
[ ] I can delete or reset records.  
[ ] I reviewed whether the app sends data externally.
