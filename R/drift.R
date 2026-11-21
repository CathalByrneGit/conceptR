#' Check whether a concept version is drifting (pure check, no side effects)
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer or `NULL`.
#' @param threshold Numeric. Disagreement rate above which drift is flagged (default 0.15).
#' @param window_days Integer. Rolling window in days (default 30).
#'
#' @return `TRUE` if drifting, `FALSE` otherwise.
#'
#' @export
adt_check_drift <- function(ctx, concept_id, scope, version = NULL,
                            threshold = 0.15, window_days = 30L) {
  .assert_ctx(ctx)
  smry <- adt_summary(ctx, concept_id, scope, version = version,
                      window_days = window_days)
  if (smry$n_judged < 5L) return(FALSE)
  if (is.na(smry$disagreement_rate)) return(FALSE)
  smry$disagreement_rate > threshold
}

#' Detect drift and create a drift event if newly detected
#'
#' Calls [adt_check_drift()]. If drift is detected and no open drift event
#' exists, creates a row in `drift_events` and logs to `governance_log`. Returns
#' `NULL` when no drift is detected.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer or `NULL`.
#' @param threshold Numeric. Default 0.15.
#' @param window_days Integer. Default 30.
#'
#' @return A data frame row from `drift_events` (one row), or `NULL`.
#'
#' @export
adt_detect_drift <- function(ctx, concept_id, scope, version = NULL,
                             threshold = 0.15, window_days = 30L) {
  .assert_ctx(ctx)

  drifting <- adt_check_drift(ctx, concept_id, scope, version,
                              threshold = threshold, window_days = window_days)
  if (!drifting) return(NULL)

  version_int <- .resolve_version_int(ctx, concept_id, scope, version)
  smry        <- adt_summary(ctx, concept_id, scope, version = version_int,
                             window_days = window_days)

  existing <- DBI::dbGetQuery(
    ctx$connection,
    "SELECT * FROM drift_events
      WHERE concept_id = ? AND scope = ? AND version = ? AND status = 'open'",
    params = list(concept_id, scope, version_int)
  )

  if (nrow(existing) > 0L) {
    cli::cli_inform("Open drift event already exists for {concept_id}/{scope} v{version_int}.")
    attr(existing, "existing") <- TRUE
    return(existing)
  }

  drift_id <- new_id()
  now      <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(
    ctx$connection,
    "INSERT INTO drift_events
       (drift_id, concept_id, scope, version, detected_at, disagreement_rate, threshold, status)
     VALUES (?, ?, ?, ?, ?, ?, ?, 'open')",
    params = list(drift_id, concept_id, scope, version_int,
                  now, smry$disagreement_rate, threshold)
  )

  adt_log(ctx, action = "drift_detected", concept_id = concept_id,
          scope = scope, version = version_int,
          notes = sprintf("disagreement_rate=%.3f threshold=%.3f",
                          smry$disagreement_rate, threshold))

  DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM drift_events WHERE drift_id = ?",
    params = list(drift_id))
}

#' Resolve an open drift event
#'
#' @param ctx A `ConceptContext`.
#' @param drift_id Character. The drift event identifier.
#' @param resolution_notes Character or `NULL`.
#'
#' @return The updated `drift_events` row (invisibly).
#'
#' @export
adt_resolve_drift <- function(ctx, drift_id, resolution_notes = NULL) {
  .assert_ctx(ctx)
  assert_that(is.character(drift_id) && length(drift_id) == 1L,
              "drift_id must be a character scalar")

  event <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM drift_events WHERE drift_id = ?", params = list(drift_id))
  if (nrow(event) == 0L) cli::cli_abort("Drift event {drift_id} not found.")
  if (event$status[1L] != "open")
    cli::cli_abort("Drift event {drift_id} is already {event$status[1L]}.")

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(ctx$connection,
    "UPDATE drift_events
        SET status = 'resolved', resolved_at = ?, resolved_by = ?, resolution_notes = ?
      WHERE drift_id = ?",
    params = list(now, ctx$actor, resolution_notes, drift_id))

  adt_log(ctx, action = "drift_resolved",
          concept_id = event$concept_id[1L], scope = event$scope[1L],
          version    = event$version[1L],    notes = resolution_notes)

  invisible(DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM drift_events WHERE drift_id = ?", params = list(drift_id)))
}

#' Return all open drift events
#'
#' @param ctx A `ConceptContext`.
#'
#' @return A data frame with columns: `concept_id`, `scope`, `version`,
#'   `detected_at`, `disagreement_rate`, `threshold`.
#'
#' @export
adt_drift_status <- function(ctx) {
  .assert_ctx(ctx)
  DBI::dbGetQuery(ctx$connection,
    "SELECT concept_id, scope, version, detected_at, disagreement_rate, threshold
       FROM drift_events
      WHERE status = 'open'
      ORDER BY detected_at DESC")
}
