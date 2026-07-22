# Shiny App Ideas

## 1. Practice Question Generator
- Call Claude API with a topic list or existing exam questions from `exam_questions` table
- Generate MC or short-answer questions on demand
- Option to save generated questions back to `exam_questions` in `finalqdata.sqlite` for reuse
- Low complexity — straightforward API call in a Shiny app

## 2. Course-Controlled Claude Chatbot
- Claude instance with a custom system prompt grounding it in course materials
- Prevents students from using it for problem set answers; restricts to course-relevant Q&A
- Rate-limit per `user_id` (auth already exists) to control API costs
- Use Haiku for cheap Q&A; escalate to Sonnet for complex queries
- Just an API call in a Shiny `observeEvent` — not infrastructure-intensive

## 3. Flex Pass Auto-Checker
- Student submits flex pass attempt → Claude evaluates against a rubric
- Structured output: `{approved: true/false, reason: "...", confidence: "high/medium/low"}`
- High-confidence approvals → "just approve" list; low-confidence → flagged for manual review
- Hooks into existing `ledger` and `pledges` tables in `finalqdata.sqlite`
- Open question: rubric hardcoded per question or admin-editable in the DB?

## Longer-Term Infrastructure
- Trigger to auto-build SQL DB (seed users table) when a new class roster CSV is uploaded
