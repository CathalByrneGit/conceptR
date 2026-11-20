#' Draw an audit sample for a concept version
#'
#' Evaluates the concept version to obtain `system_value` for all instances,
#' draws a random sample, inserts rows into `audit_samples`, and returns the
#' sample as a data frame.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character. The concept identifier.
#' @param scope Character. The concept scope.
#' @param version Integer or `NULL`. If `NULL` the active version is used.
#' @param n Integer. Sample size (default 20).
#' @param stratify Logical. If `TRUE`, draw a balanced sample with equal
#'   representation of `TRUE` and `FALSE` system values. Default `FALSE`.
#'
#' @return A data frame with columns: `sample_id`, `object_key`, `system_value`,
#'   `sampled_at`.
#'
#' @export
adt_sample <- function(ctx, concept_id, scope, version = NULL, n = 20L,
                       stratify = FALSE) {
  .assert_ctx(ctx)
  assert_that(is.character(concept_id) && length(concept_id) == 1L,
              "concept_id must be a character scalar")
  assert_that(is.character(scope) && length(scope) == 1L,
              "scope must be a character scalar")

  n       <- as.integer(n)
  version <- .resolve_version_int(ctx, concept_id, scope, version)

  eval_df <- cpt_evaluate(ctx, concept_id, scope, version)
  eval_df <- .normalise_eval_df(eval_df)

  if (nrow(eval_df) == 0L) {
    cli::cli_warn("No instances found for concept {concept_id}/{scope} v{version}.")
    return(.empty_sample_frame())
  }

  if (isTRUE(stratify)) {
    sampled <- .stratified_sample(eval_df, n)
  } else {
    sampled <- eval_df[sample(nrow(eval_df), min(n, nrow(eval_df))), , drop = FALSE]
  }

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  rows <- lapply(seq_len(nrow(sampled)), function(i) {
    list(
      sample_id    = new_id(),
      concept_id   = concept_id,
      scope        = scope,
      version      = version,
      object_key   = sampled$object_key[i],
      system_value = sampled$system_value[i],
      sampled_at   = now,
      reviewed     = FALSE
    )
  })

  for (r in rows) {
    DBI::dbExecute(
      ctx$connection,
      "INSERT INTO audit_samples
         (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(r$sample_id, r$concept_id, r$scope, r$version,
                    r$object_key, r$system_value, r$sampled_at, r$reviewed)
    )
  }

  do.call(rbind, lapply(rows, function(r) {
    data.frame(
      sample_id    = r$sample_id,
      object_key   = r$object_key,
      system_value = r$system_value,
      sampled_at   = r$sampled_at,
      stringsAsFactors = FALSE
    )
  }))
}

#' Return unreviewed audit samples
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character or `NULL`. Filter by concept.
#' @param scope Character or `NULL`. Filter by scope.
#' @param reviewer_id Reserved for future use; currently ignored.
#'
#' @return A data frame with columns: `sample_id`, `concept_id`, `scope`,
#'   `version`, `object_key`, `system_value`, `sampled_at`.
#'
#' @export
adt_pending_reviews <- function(ctx, concept_id = NULL, scope = NULL,
                                reviewer_id = NULL) {
  .assert_ctx(ctx)

  sql  <- "SELECT sample_id, concept_id, scope, version, object_key, system_value, sampled_at
             FROM audit_samples
            WHERE reviewed = FALSE"
  args <- list()

  if (!is.null(concept_id)) {
    sql  <- paste0(sql, " AND concept_id = ?")
    args <- c(args, list(concept_id))
  }
  if (!is.null(scope)) {
    sql  <- paste0(sql, " AND scope = ?")
    args <- c(args, list(scope))
  }

  sql <- paste0(sql, " ORDER BY sampled_at DESC")

  if (length(args) > 0L) {
    DBI::dbGetQuery(ctx$connection, sql, params = args)
  } else {
    DBI::dbGetQuery(ctx$connection, sql)
  }
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Normalise cpt_evaluate() output to two columns: object_key (TEXT), system_value (LOGICAL).
# cpt_evaluate() returns pk_col | concept_id_col; neither is named "value" or "system_value".
.normalise_eval_df <- function(df) {
  if (!"object_key" %in% names(df)) {
    names(df)[1L] <- "object_key"
  }
  if ("value" %in% names(df) && !"system_value" %in% names(df)) {
    names(df)[names(df) == "value"] <- "system_value"
  } else if (!"system_value" %in% names(df) && ncol(df) >= 2L) {
    names(df)[2L] <- "system_value"
  }
  df$object_key   <- as.character(df$object_key)
  df$system_value <- as.logical(df$system_value)
  df[, c("object_key", "system_value"), drop = FALSE]
}

.stratified_sample <- function(df, n) {
  trues  <- df[df$system_value == TRUE,  , drop = FALSE]
  falses <- df[df$system_value == FALSE, , drop = FALSE]

  n_each  <- ceiling(n / 2L)
  n_true  <- min(n_each, nrow(trues))
  n_false <- min(n - n_true, nrow(falses))
  if (n_true  < n_each) n_false <- min(n - n_true,  nrow(falses))
  if (n_false < n_each) n_true  <- min(n - n_false, nrow(trues))

  rows_true  <- if (n_true  > 0L) trues[sample(nrow(trues),   n_true),  , drop = FALSE] else trues[0L,  , drop = FALSE]
  rows_false <- if (n_false > 0L) falses[sample(nrow(falses), n_false), , drop = FALSE] else falses[0L, , drop = FALSE]

  rbind(rows_true, rows_false)
}

.empty_sample_frame <- function() {
  data.frame(
    sample_id    = character(0),
    object_key   = character(0),
    system_value = logical(0),
    sampled_at   = character(0),
    stringsAsFactors = FALSE
  )
}
