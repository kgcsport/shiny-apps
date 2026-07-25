# Classroom Tool Testing Checklist

Use this checklist before trying a generated tool in class. Test with fake data first.

## Basic Run Test

- [ ] Can I install dependencies?
- [ ] Can I launch the app?
- [ ] Can I load fake data?
- [ ] Can I reset the app?
- [ ] Can I restart without losing saved data?

## Classroom Flow Test

- [ ] Can I start a session?
- [ ] Can students or fake students take actions?
- [ ] Can the instructor advance stages or rounds?
- [ ] Can missing choices be handled?
- [ ] Can scores, payoffs, or outcomes be calculated?
- [ ] Can the student-facing display be shown safely?
- [ ] Can private information stay hidden?

## Data Test

- [ ] Is a raw event log created?
- [ ] Does each record have a timestamp?
- [ ] Does each record have a session or round ID?
- [ ] Are participant IDs stable?
- [ ] Can I export the raw data?
- [ ] Can I export a summary?
- [ ] Can I inspect the exported files manually?

## Failure Test

- [ ] What happens if I refresh the page?
- [ ] What happens if I close and reopen the app?
- [ ] What happens if a student makes no choice?
- [ ] What happens if I enter a mistaken value?
- [ ] Can I correct an error?
- [ ] Can I recover from a crash?

## Classroom Readiness

- [ ] Do I know how to explain the activity?
- [ ] Do I know what students will see?
- [ ] Do I know what I will click during class?
- [ ] Do I know what data will be saved?
- [ ] Do I have a fallback plan if the tool fails?
