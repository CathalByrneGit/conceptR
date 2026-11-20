#' Compute an audit summary for a concept version
#'
#' Returns counts and the disagreement rate over all (or a filtered subset of)
#' recorded judgments.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer or `NULL`. If `NULL`, aggregates across all versions.
#' @param since POSIXct/character or `NULL`.
#' @param window_days Integer or `NULL`. Restricts to judgments in the last N
#'   days (takes precedence over `since`).
#'
#' @return A named list with `n_judged`, `n_agree`, `n_disagree`,
#'   `disagreement_rate`.
#'
#' @export
adt_summary <- function(ctx, concept_id, scope, version = NULL,
                        since = NULL, window_days = NULL) {
  .assert_ctx(ctx)

  sql  <- "SELECT system_value, reviewer_value, reviewed_at
             FROM audit_judgments
            WHERE concept_id = ? AND scope = ?"
  args <- list(concept_id, scope)

  if (!is.null(version)) {
    sql  <- paste0(sql, " AND version = ?")
    args <- c(args, list(as.integer(version)))
  }

  if (!is.null(window_days)) {
    cutoff <- format(Sys.time() - as.difftime(window_days, units = "days"),
                     "%Y-%m-%d %H:%M:%S")
    sql  <- paste0(sql, " AND reviewed_at >= ?")
    args <- c(args, list(cutoff))
  } else if (!is.null(since)) {
    since_str <- if (inherits(since, "POSIXct")) format(since, "%Y-%m-%d %H:%M:%S") else as.character(since)
    sql  <- paste0(sql, " AND reviewed_at >= ?")
    args <- c(args, list(since_str))
  }

  rows       <- DBI::dbGetQuery(ctx$connection, sql, params = args)
  n_judged   <- nrow(rows)
  n_agree    <- if (n_judged == 0L) 0L else sum(rows$system_value == rows$reviewer_value)
  n_disagree <- n_judged - n_agree
  rate       <- if (n_judged == 0L) NA_real_ else n_disagree / n_judged

  list(n_judged = n_judged, n_agree = n_agree, n_disagree = n_disagree,
       disagreement_rate = rate)
}

#' Compute rolling disagreement rates over time
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer or `NULL`.
#' @param window_days Integer. Width of each rolling window in days (default 30).
#' @param step_days Integer. Step between window starts in days (default 7).
#'
#' @return A data frame with columns: `window_start`, `window_end`, `n_judged`,
#'   `disagreement_rate`.
#'
#' @export
adt_rolling_disagreement <- function(ctx, concept_id, scope, version = NULL,
                                     window_days = 30L, step_days = 7L) {
  .assert_ctx(ctx)
  window_days <- as.integer(window_days)
  step_days   <- as.integer(step_days)

  sql  <- "SELECT system_value, reviewer_value, reviewed_at
             FROM audit_judgments
            WHERE concept_id = ? AND scope = ?"
  args <- list(concept_id, scope)
  if (!is.null(version)) {
    sql  <- paste0(sql, " AND version = ?")
    args <- c(args, list(as.integer(version)))
  }

  rows <- DBI::dbGetQuery(ctx$connection, sql, params = args)
  if (nrow(rows) == 0L) {
    return(data.frame(window_start = character(0), window_end = character(0),
                      n_judged = integer(0), disagreement_rate = numeric(0),
                      stringsAsFactors = FALSE))
  }

  rows$reviewed_at <- as.POSIXct(rows$reviewed_at, tz = "UTC")
  earliest <- min(rows$reviewed_at)
  latest   <- max(rows$reviewed_at)
  starts   <- seq(earliest, latest, by = paste(step_days, "days"))

  windows <- lapply(starts, function(ws) {
    we     <- ws + as.difftime(window_days, units = "days")
    subset <- rows[rows$reviewed_at >= ws & rows$reviewed_at < we, ]
    n      <- nrow(subset)
    rate   <- if (n == 0L) NA_real_ else sum(subset$system_value != subset$reviewer_value) / n
    list(window_start = format(ws, "%Y-%m-%d %H:%M:%S"),
         window_end   = format(we, "%Y-%m-%d %H:%M:%S"),
         n_judged     = n, disagreement_rate = rate)
  })

  do.call(rbind, lapply(windows, as.data.frame, stringsAsFactors = FALSE))
}
