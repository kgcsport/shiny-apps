"%||%" <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
}

normalize_poll_response <- function(x) {
  x <- enc2utf8(trimws(as.character(x %||% "")))
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("\\s+", " ", x)
  tolower(trimws(x))
}

validate_poll_response <- function(x, max_words = 4L, max_chars = 42L) {
  clean <- trimws(as.character(x %||% ""))
  if (!nzchar(clean)) return("Enter a tax base before submitting.")
  if (!nzchar(normalize_poll_response(clean))) {
    return("Enter words rather than punctuation.")
  }
  if (nchar(clean, type = "chars") > max_chars) {
    return(paste0("Keep the response under ", max_chars, " characters."))
  }
  words <- strsplit(clean, "\\s+")[[1]]
  if (length(words) > max_words) {
    return(paste0("Use ", max_words, " words or fewer."))
  }
  NULL
}

cloud_term_specs <- function(dat) {
  stopifnot(all(c("label", "n") %in% names(dat)))
  if (!nrow(dat)) return(dat)
  dat <- dat[order(-dat$n, tolower(dat$label)), , drop = FALSE]
  max_n <- max(dat$n)
  dat$font_rem <- pmin(3.8, 1.1 + 0.9 * log2(dat$n + 1))
  dat$opacity <- 0.72 + 0.28 * sqrt(dat$n / max_n)
  palette <- c("#7A1731", "#0072B2", "#D55E00", "#009E73", "#6A3D9A", "#8C510A")
  dat$colour <- palette[(seq_len(nrow(dat)) - 1L) %% length(palette) + 1L]
  dat
}
