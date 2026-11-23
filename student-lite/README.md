# Student Lite (demo)

A fast phone page for the two things students do most in Class Job Market:
check their points and enter wage bids. It is a demo on a branch, not a
replacement for the Shiny app.

## What it is

- One small R process (`server.R`) that answers plain HTTP requests, plus one
  static page (`public/index.html`). There is no Shiny session, no websocket,
  and no per-student polling loop on the server.
- It uses the same SQLite database as the Shiny app and loads the Shiny app's
  own code. Whether bidding is open, which jobs can be bid on, the no-anchor
  switch, and the password check are the app's functions, not a second copy.
- Bids are written to the same `job_wage_bids` rows the Shiny app reads, so the
  instructor's draw sees them with no sync step.

## What it does not do

- Google sign-in. It accepts a username and password only, so it cannot be
  used by a class that signs in with Google until a sign-in handoff is built.
- Assignments, reveals, spending, grades, games, or anything for instructors.
  Those stay in the Shiny app.
- Students enrolled in more than one course see their primary course only.

## Run it

```
Rscript student-lite/server.R
```

Settings, all optional environment variables:

| Variable | Default | Meaning |
|---|---|---|
| `LITE_PORT` | `8090` | Port to listen on |
| `LITE_HOST` | `0.0.0.0` | Address to bind |
| `LITE_SECURE_COOKIE` | `0` | Set to `1` behind HTTPS |
| `LITE_HUB_APP` | found automatically | Path to `class-job-market/app.R` |
| `CONNECT_CONTENT_DIR` | as for the Shiny app | Where the database lives |

`docker-compose.student-lite.yml` is an untested sketch of running it beside
the staging stack.

## Tests

```
Rscript tests/student-lite.R
```

## Measured on one machine, one user (2026-11-23)

Phone-sized Chromium against both services on the same scratch database:

| | Student Lite | Shiny app |
|---|---|---|
| Page weight to the sign-in form | about 11 KB, 2 requests | about 530 KB, 8 requests |
| Time to a usable sign-in form | about 50 ms | 1 to 8 s across three loads |
| Sign in to bid form on screen | about 140 ms | about 1.1 s |
| Save bids | about 180 ms | not timed |
| Process memory | about 130 MB | about 260 MB with one session |

Fifty simultaneous state requests to Student Lite: median 370 ms, slowest
750 ms. These are local numbers with no network and no real class load; they
show the shape of the difference, not what a classroom will see.
