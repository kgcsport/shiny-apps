# Server-side bridge from class-job-market extension purchases to the private
# Cloudflare assignment API. Never send ASSIGNMENT_ADMIN_TOKEN to browser code.
ASSIGNMENT_API_ORIGIN <- sub("/+$", "", Sys.getenv(
  "ASSIGNMENT_API_ORIGIN",
  "https://econ342-self-grading.kyle-g-coombs.workers.dev"))
ASSIGNMENT_ADMIN_TOKEN <- trimws(Sys.getenv("ASSIGNMENT_ADMIN_TOKEN", ""))

extension_sync_configured <- function() {
  nzchar(ASSIGNMENT_API_ORIGIN) && nzchar(ASSIGNMENT_ADMIN_TOKEN) &&
    requireNamespace("httr2", quietly = TRUE)
}

extension_sync_payload <- function(row) {
  list(
    source = "shiny-class-job-market",
    sourcePurchaseId = as.character(row$id[1]),
    assignmentId = as.character(row$cloudflare_assignment_id[1]),
    userId = as.character(row$user_id[1]),
    target = as.character(row$extension_target[1]),
    hours = as.numeric(row$hours[1]),
    purchasedAt = as.character(row$purchased_at[1])
  )
}

sync_extension_purchase <- function(purchase_id) {
  row <- db_query(
    "SELECT ep.id, ep.user_id, ep.hours, ep.purchased_at,
            ps.cloudflare_assignment_id, COALESCE(ps.extension_target,'submission') extension_target
     FROM extension_purchases ep
     JOIN problem_sets ps ON ps.id=ep.problem_set_id
     WHERE ep.id=?;",
    list(as.integer(purchase_id)))
  if (!nrow(row)) return(list(ok=FALSE, status="failed", error="Extension purchase not found"))
  if (!nzchar(trimws(row$cloudflare_assignment_id[1] %||% ""))) {
    error <- "Assignment is not mapped to a Cloudflare assignment ID"
    db_exec("UPDATE extension_purchases SET sync_status='not_configured',sync_error=? WHERE id=?;",
            list(error, as.integer(purchase_id)))
    return(list(ok=FALSE, status="not_configured", error=error))
  }
  if (!extension_sync_configured()) {
    error <- "Set ASSIGNMENT_ADMIN_TOKEN in the Shiny server environment"
    db_exec("UPDATE extension_purchases SET sync_status='not_configured',sync_error=? WHERE id=?;",
            list(error, as.integer(purchase_id)))
    return(list(ok=FALSE, status="not_configured", error=error))
  }
  result <- tryCatch({
    response <- httr2::request(paste0(ASSIGNMENT_API_ORIGIN, "/api/admin/extensions/sync")) |>
      httr2::req_headers(Authorization=paste("Bearer", ASSIGNMENT_ADMIN_TOKEN)) |>
      httr2::req_body_json(extension_sync_payload(row), auto_unbox=TRUE) |>
      httr2::req_timeout(12) |>
      httr2::req_error(is_error=function(resp) FALSE) |>
      httr2::req_perform()
    body <- tryCatch(httr2::resp_body_json(response, simplifyVector=TRUE),
                     error=function(e) list())
    if (httr2::resp_status(response) >= 300)
      stop(body$error %||% paste("Cloudflare returned", httr2::resp_status(response)))
    list(ok=TRUE, status="synced", error="", response=body)
  }, error=function(e) list(ok=FALSE, status="failed", error=conditionMessage(e)))
  db_exec(
    "UPDATE extension_purchases SET sync_status=?,sync_error=?,synced_at=CASE WHEN ?='synced' THEN CURRENT_TIMESTAMP ELSE synced_at END WHERE id=?;",
    list(result$status, result$error, result$status, as.integer(purchase_id)))
  result
}


# Read-only server-side access to the Worker instructor review API. This uses
# the same ADMIN_TOKEN as extension syncing; no credential is exposed to clients.
assignment_review_configured <- function() {
  extension_sync_configured()
}

assignment_review_get <- function(path) {
  if (!assignment_review_configured())
    stop("Set ASSIGNMENT_ADMIN_TOKEN and install httr2")
  response <- httr2::request(paste0(ASSIGNMENT_API_ORIGIN, path)) |>
    httr2::req_headers(Authorization=paste("Bearer", ASSIGNMENT_ADMIN_TOKEN)) |>
    httr2::req_timeout(20) |>
    httr2::req_error(is_error=function(resp) FALSE) |>
    httr2::req_perform()
  body <- tryCatch(httr2::resp_body_json(response, simplifyVector=TRUE),
                   error=function(e) list())
  if (httr2::resp_status(response) >= 300) {
    detail <- body$error %||% body$message %||% "no response body"
    stop(sprintf("Cloudflare assignment API failed: HTTP %s at %s%s — %s", httr2::resp_status(response), ASSIGNMENT_API_ORIGIN, path, detail))
  }
  body
}

assignment_review_assignments <- function() {
  assignment_review_get("/api/admin/review/assignments")
}

assignment_review_assignment <- function(assignment_id) {
  assignment_review_get(paste0("/api/admin/review/assignments/",
                               utils::URLencode(as.character(assignment_id), reserved=TRUE)))
}
