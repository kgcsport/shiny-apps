# Manual Test Script — Demo Kit Site (AI Teaching Tool Builder)

**App:** `apps/demo-kit-site`  
**No login required** for Prompt Builder and Documentation tabs.  
**Note:** API key tests require a real key; use a low-budget key or mock the endpoint.

---

## TC-01 — Prompt Builder: generate prompt with all fields filled

1. Open app → **Prompt Builder** tab.
2. Fill in:
   - Course: `ECON 201`
   - Class size: `28`
   - Activity type: `Auction`
   - Learning goal: `Students understand how supply and demand set prices`
   - Student actions: `Submit bids each round`
   - Instructor actions: `Open/close rounds, reveal market price`
   - Public display: `Live price chart and round history`
   - Scoring / payoff rules: `Highest bidder wins; pays their own bid`
   - Preferred stack: `R Shiny + SQLite`
   - Known constraints: `Must run offline, no external APIs`
3. Click **Generate prompt**.

**Expected:**
- Right column shows a formatted prompt containing all filled values verbatim.
- Copy to clipboard button appears.

---

## TC-02 — Prompt Builder: generate prompt with minimal fields

1. Leave all fields blank (or use defaults).
2. Click **Generate prompt**.

**Expected:**
- Prompt appears with placeholder text: `[your course]`, `[class size]`, etc.
- Requirements section always present regardless of input.

---

## TC-03 — Copy to clipboard button

**Precondition:** TC-01 complete, prompt is generated.

1. Click **Copy to clipboard**.

**Expected:**
- Button text changes to "Copied!" for 2 seconds then reverts.
- Clipboard contains the full prompt text (paste into a text editor to verify).

---

## TC-04 — Option A: CLI command — Claude Code

**Precondition:** TC-01 complete.

1. In the "Use your own subscription" box, select **Claude Code** from the dropdown.

**Expected:**
- Command block shows: `claude -p '<prompt text>'`
- Single quotes in the prompt are shell-escaped (`'` → `'\''`).

2. Click **Copy command**.

**Expected:** Button says "Copied!" — paste into terminal to verify the command is syntactically valid.

---

## TC-05 — Option A: CLI command — Codex CLI

1. Select **Codex CLI** from the dropdown.

**Expected:** Command block shows: `codex -q '<prompt text>'`

---

## TC-06 — Option A: CLI command — Gemini CLI

1. Select **Gemini CLI** from the dropdown.

**Expected:** Command block shows: `gemini -m gemini-2.0-flash '<prompt text>'`

---

## TC-07 — Option A: Just the prompt

1. Select **Just the prompt** from the dropdown.

**Expected:** Command block shows the raw prompt text with no CLI wrapper.

---

## TC-08 — Option B: Generate with API key — Anthropic (live key)

**Precondition:** Valid Anthropic API key available.

1. Select provider **Anthropic**, model **Claude Haiku 4.5 (fast)**.
2. Enter API key.
3. Click **Generate app code**.

**Expected:**
- Status shows "Generating…" while request is in flight.
- Dark code block appears with `=== app.R ===`, `=== README.md ===`, `=== install.R ===` sections.
- Download buttons appear: Download app.R, README.md, install.R.

---

## TC-09 — Option B: Generate with API key — invalid key

1. Enter provider **Anthropic**, model **Claude Haiku 4.5 (fast)**.
2. Enter key `sk-ant-INVALID`.
3. Click **Generate app code**.

**Expected:**
- Status shows an error notification (not a crash).
- Code block does not appear.

---

## TC-10 — Option B: Generate with OpenRouter — Gemini Flash 1.5

**Precondition:** Valid OpenRouter key available.

1. Select provider **OpenRouter**, model **Gemini Flash 1.5**.
2. Enter key, click **Generate app code**.

**Expected:**
- Successful response with same section markers.
- No 404 error (`google/gemini-flash-1.5` is a valid live model).

---

## TC-11 — Option B: Email delivery (mailto link)

**Precondition:** TC-08 complete (code generated).

1. Enter email `test@example.com` in the email field.
2. Regenerate or if email was filled before generation, check after.

**Expected:**
- A "Email this result" link appears after generation.
- Clicking opens default mail client with subject pre-filled and body containing the first 1800 chars of generated code.

---

## TC-12 — Documentation tab: navigate sidebar

1. Click **Documentation** tab.

**Expected:**
- Left sidebar shows groups: "Kit files", "Examples", "Starter specs".
- First file is auto-selected and rendered as HTML.

2. Click `01_prompt_template` in the sidebar.

**Expected:**
- Doc panel renders the markdown content of `01_prompt_template.md`.
- Active sidebar link is highlighted.

---

## TC-13 — Documentation tab: download single file

1. With a doc open, click **Download this file**.

**Expected:** `.md` file downloads matching the currently-open doc.

---

## TC-14 — Documentation tab: download all as ZIP

1. Click **Download all (ZIP)**.

**Expected:** ZIP file downloads containing all kit `.md` files.

---

## TC-15 — Submit an App: upload valid R file

1. Click **Submit an App** tab.
2. Fill in title `Test App`, URL `https://example.shinyapps.io/test`.
3. Click **Attach app files** → upload a valid `app.R` file (any working Shiny app).

**Expected:**
- File info area shows: `✓ app.R — valid R`

---

## TC-16 — Submit an App: upload invalid R file

1. Create a file `bad.R` containing: `x <- (1 + `  (unclosed paren)
2. Upload via the file input.

**Expected:**
- File info area shows: `✗ bad.R — <parse error message>`

---

## TC-17 — Submit an App: submit form

**Precondition:** Title and URL are filled.

1. Click **Submit**.

**Expected:**
- Success notification: "Submission received…"
- Form fields reset.
- Row inserted in `gallery_submissions` SQLite table with `status='pending'`.

---

## TC-18 — Submit an App: missing required fields

1. Leave title or URL blank.
2. Click **Submit**.

**Expected:** Validation error — submission not sent.

---

## TC-19 — Prompt generated before no fields filled (regression)

1. Click **Generate prompt** immediately without filling any fields.

**Expected:** Prompt appears with all placeholder values — no crash, no blank output.
