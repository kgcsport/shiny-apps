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

## Sign-in

Students sign in with Google through the hub's existing login. That flow
already ends by storing a session token in the hub database and setting it as
the `arcade_token` cookie for the whole site. Student Lite accepts the same
cookie, so:

- a student already signed in to the Shiny app is signed in here, and the
  reverse;
- the "Sign in with your school Google account" button goes to the existing
  `/auth/login`, which now may return to `/student-lite/`;
- signing out here ends the shared session.

A username and password also work, tucked under a disclosure, and issue the
same kind of session.

Sharing the cookie requires Student Lite to be served from the same domain as
the Shiny app. `nginx.conf` on this branch routes `/student-lite/` to the
service for that reason.

## What it does not do

- Assignments, reveals, spending, grades, games, or anything for instructors.
  Those stay in the Shiny app.
- Students enrolled in more than one course see their primary course only.

## To put it in front of a class

1. Run the `student-lite` service next to the Shiny container, with the same
   image, the same data volume, and the same Google environment variables
   (`docker-compose.student-lite.yml` is an untested sketch; set
   `LITE_SECURE_COOKIE=1`).
2. Deploy this branch's `nginx.conf` (adds the `/student-lite/` route).
3. Rebuild and deploy the demo-kit image, which hosts the Google login relay,
   so it accepts `/student-lite/` as a place to return to.
4. Send students to `https://<shiny domain>/student-lite/`.

Not verified: a real Google sign-in end to end, and the Docker and nginx
pieces. Verified in a browser: a session issued the way the relay issues one
opens the page with no further sign-in, under the `/student-lite/` path.

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

## Resource use under a class-sized load (2026-11-23)

Students signed in and left on the bid page, each app measured from a fresh
start on the same machine and scratch database. CPU is the share of one core
over a 30-second window; the Shiny app runs on a single core.

| Students on the bid page | Student Lite memory | Student Lite CPU | Shiny memory | Shiny CPU |
|---:|---:|---:|---:|---:|
| 0 | 134 MB | – | 156 MB | – |
| 1 | 134 MB | 0.2% | 211 MB | 1.2% |
| 10 | 134 MB | 1.4% | 317 MB | 10.1% |
| 25 | 134 MB | 3.7% | 586 MB | 22.8% |

Student Lite costs a fixed 134 MB whether or not anyone uses it. Each student
on the Shiny bid page costs about 17 MB and about 1% of a core, so it is a net
memory saving once roughly eight students use it instead of the Shiny page,
and a net cost if students keep both open. The Shiny process did not hand
memory back after sessions closed in an earlier run.

