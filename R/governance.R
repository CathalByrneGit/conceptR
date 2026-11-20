#' Record a governance action in the log
#'
#' @param ctx A `ConceptContext`.
#' @param action Character.
#' @param concept_id Character or `NULL`.
#' @param scope Character or `NULL`.
#' @param version Integer or `NULL`.
#' @param notes Character or `NULL`.
#'
#' @return The `log_id` (invisibly).
#'
#' @export
adt_log <- function(ctx, action, concept_id = NULL, scope = NULL,
                    version = NULL, notes = NULL) {
  .assert_ctx(ctx)
  assert_that(is.character(action) && length(action) == 1L && nzchar(action),
              "action must be a non-empty character scalar")

  log_id        <- new_id()
  now           <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  evidence_json <- NULL

  if (!is.null(concept_id) && !is.null(scope)) {
    smry <- tryCatch(
      adt_summary(ctx, concept_id, scope, version = version),
      error = function(e) NULL
    )
    if (!is.null(smry))
      evidence_json <- jsonlite::toJSON(smry, auto_unbox = TRUE)
  }

  DBI::dbExecute(ctx$connection,
    "INSERT INTO governance_log
       (log_id, action, concept_id, scope, version, actor, performed_at, evidence_json, notes)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      log_id, action, concept_id, scope,
      if (is.null(version)) NA_integer_ else as.integer(version),
      ctx$actor, now, evidence_json, notes
    )
  )

  invisible(log_id)
}

#' Retrieve governance log entries
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character or `NULL`.
#' @param action Character or `NULL`.
#' @param actor Character or `NULL`.
#' @param since POSIXct/character or `NULL`.
#' @param n Integer. Maximum rows to return (default 100).
#'
#' @return A data frame of governance log entries ordered by `performed_at` desc.
#'
#' @export
adt_get_log <- function(ctx, concept_id = NULL, action = NULL,
                        actor = NULL, since = NULL, n = 100L) {
  .assert_ctx(ctx)

  sql  <- "SELECT log_id, action, concept_id, scope, version, actor, performed_at, notes
             FROM governance_log
            WHERE 1 = 1"
  args <- list()

  if (!is.null(concept_id)) { sql <- paste0(sql, " AND concept_id = ?"); args <- c(args, list(concept_id)) }
  if (!is.null(action))     { sql <- paste0(sql, " AND action = ?");     args <- c(args, list(action)) }
  if (!is.null(actor))      { sql <- paste0(sql, " AND actor = ?");      args <- c(args, list(actor)) }
  if (!is.null(since)) {
    since_str <- if (inherits(since, "POSIXct")) format(since, "%Y-%m-%d %H:%M:%S") else as.character(since)
    sql  <- paste0(sql, " AND performed_at >= ?")
    args <- c(args, list(since_str))
  }

  sql <- paste0(sql, " ORDER BY performed_at DESC LIMIT ", as.integer(n))

  if (length(args) > 0L) DBI::dbGetQuery(ctx$connection, sql, params = args)
  else                    DBI::dbGetQuery(ctx$connection, sql)
}

#' Produce a governance report summarising all audited concepts
#'
#' @param ctx A `ConceptContext`.
#'
#' @return A data frame with columns: `concept_id`, `scope`, `last_audit_date`,
#'   `n_judged_total`, `current_disagreement_rate`, `drift_status`,
#'   `last_governance_action`.
#'
#' @export
adt_governance_report <- function(ctx) {
  .assert_ctx(ctx)

  pairs <- DBI::dbGetQuery(ctx$connection,
    "SELECT DISTINCT concept_id, scope FROM audit_judgments")

  if (nrow(pairs) == 0L) {
    return(data.frame(
      concept_id = character(0), scope = character(0),
      last_audit_date = character(0), n_judged_total = integer(0),
      current_disagreement_rate = numeric(0), drift_status = character(0),
      last_governance_action = character(0), stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    cid  <- pairs$concept_id[i]
    sc   <- pairs$scope[i]
    smry <- adt_summary(ctx, cid, sc)

    last_date <- DBI::dbGetQuery(ctx$connection,
      "SELECT MAX(reviewed_at) AS last_date FROM audit_judgments
        WHERE concept_id = ? AND scope = ?",
      params = list(cid, sc))$last_date[1L]

    drift_open <- DBI::dbGetQuery(ctx$connection,
      "SELECT COUNT(*) AS n FROM drift_events
        WHERE concept_id = ? AND scope = ? AND status = 'open'",
      params = list(cid, sc))$n[1L]

    last_action <- DBI::dbGetQuery(ctx$connection,
      "SELECT action FROM governance_log
        WHERE concept_id = ? AND scope = ? ORDER BY performed_at DESC LIMIT 1",
      params = list(cid, sc))

    data.frame(
      concept_id                = cid,
      scope                     = sc,
      last_audit_date           = as.character(last_date),
      n_judged_total            = smry$n_judged,
      current_disagreement_rate = smry$disagreement_rate,
      drift_status              = if (drift_open > 0L) "open" else "clean",
      last_governance_action    = if (nrow(last_action) > 0L) last_action$action[1L] else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
