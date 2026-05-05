#' Define a composite score
#'
#' @param ctx A `ConceptContext`.
#' @param score_id Unique score identifier.
#' @param object_type_id Object type this score applies to.
#' @param components List of lists, each with `concept_id`, `scope`, `version`
#'   (or `NULL` for active), and `weight` (default 1.0).
#' @param aggregation One of `"weighted_sum"`, `"average"`, `"max"`, `"min"`.
#' @param thresholds Named list, e.g. `list(low = 30, medium = 60, high = 80)`.
#' @param description Optional description.
#' @return `ctx` invisibly.
#' @export
cpt_define_score <- function(ctx, score_id, object_type_id,
                               components, aggregation = "weighted_sum",
                               thresholds = NULL, description = NULL) {
  .assert_ctx(ctx)
  valid_agg <- c("weighted_sum", "average", "max", "min")
  if (!aggregation %in% valid_agg)
    cli::cli_abort("{.arg aggregation} must be one of {.val {valid_agg}}.")

  existing <- DBI::dbGetQuery(ctx$connection,
    "SELECT score_id FROM composite_scores WHERE score_id = ?",
    params = list(score_id))
  if (nrow(existing) > 0)
    cli::cli_abort("Score {.val {score_id}} already exists.")

  thresh_json <- if (!is.null(thresholds)) .json_encode(thresholds) else NA_character_

  DBI::dbExecute(ctx$connection,
    "INSERT INTO composite_scores
       (score_id, object_type_id, description, aggregation, thresholds_json, created_at)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(score_id, object_type_id,
                  description %||% NA_character_,
                  aggregation, thresh_json, .now()))

  for (comp in components) {
    cpt_add_score_component(ctx, score_id,
      comp$concept_id, comp$scope,
      version = comp$version %||% NULL,
      weight  = comp$weight  %||% 1.0)
  }

  invisible(ctx)
}

#' Evaluate a composite score
#'
#' @param ctx A `ConceptContext`.
#' @param score_id Score identifier.
#' @return A data frame with PK columns, component concept columns, `score`, and
#'   optionally `tier`.
#' @export
cpt_evaluate_score <- function(ctx, score_id) {
  .assert_ctx(ctx)
  score_def <- .get_score_row(ctx, score_id)
  comps     <- .get_score_components(ctx, score_id)
  if (nrow(comps) == 0)
    cli::cli_abort("Score {.val {score_id}} has no components.")

  ot_id <- score_def$object_type_id
  ot    <- .resolve_object_type(ctx, ot_id)
  pk    <- ot$primary_key

  results <- lapply(seq_len(nrow(comps)), function(i) {
    comp <- comps[i, ]
    ver  <- if (is.na(comp$version)) NULL else comp$version
    df   <- cpt_evaluate(ctx, comp$concept_id, comp$scope, version = ver,
                          object_type_id = ot_id)
    df
  })

  merged <- results[[1]][, pk, drop = FALSE]
  for (i in seq_len(nrow(comps))) {
    cid <- comps$concept_id[i]
    merged[[cid]] <- results[[i]][[cid]]
  }

  agg <- score_def$aggregation
  weights <- comps$weight

  merged$score <- switch(agg,
    weighted_sum = {
      mat <- as.matrix(merged[, comps$concept_id, drop = FALSE]) * 1.0
      as.numeric(mat %*% weights)
    },
    average = {
      mat <- as.matrix(merged[, comps$concept_id, drop = FALSE]) * 1.0
      rowMeans(mat, na.rm = TRUE)
    },
    max = {
      mat <- as.matrix(merged[, comps$concept_id, drop = FALSE]) * 1.0
      apply(mat, 1, max, na.rm = TRUE)
    },
    min = {
      mat <- as.matrix(merged[, comps$concept_id, drop = FALSE]) * 1.0
      apply(mat, 1, min, na.rm = TRUE)
    }
  )

  thresholds_json <- score_def$thresholds_json
  if (!is.na(thresholds_json) && !is.null(thresholds_json)) {
    thresholds <- .json_decode(thresholds_json)
    merged$tier <- .assign_tier(merged$score, thresholds)
  }

  merged
}

#' Score distribution summary
#'
#' @param ctx A `ConceptContext`.
#' @param score_id Score identifier.
#' @return A list with summary statistics.
#' @export
cpt_score_distribution <- function(ctx, score_id) {
  .assert_ctx(ctx)
  df <- cpt_evaluate_score(ctx, score_id)
  s  <- df$score
  result <- list(
    min    = min(s, na.rm = TRUE),
    p25    = stats::quantile(s, 0.25, na.rm = TRUE),
    median = stats::median(s, na.rm = TRUE),
    mean   = mean(s, na.rm = TRUE),
    p75    = stats::quantile(s, 0.75, na.rm = TRUE),
    max    = max(s, na.rm = TRUE)
  )
  if ("tier" %in% names(df))
    result$tier_counts <- table(df$tier)
  result
}

#' Add a component to an existing score
#'
#' @param ctx A `ConceptContext`.
#' @param score_id Score identifier.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param version Version number (`NULL` = resolve at evaluation time).
#' @param weight Numeric weight (default 1.0).
#' @return `ctx` invisibly.
#' @export
cpt_add_score_component <- function(ctx, score_id, concept_id, scope,
                                      version = NULL, weight = 1.0) {
  .assert_ctx(ctx)
  score_def <- .get_score_row(ctx, score_id)

  # Resolve version to store (NA means "use active at eval time")
  stored_version <- if (is.null(version)) NA_integer_ else as.integer(version)

  existing <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM composite_score_components
     WHERE score_id = ? AND concept_id = ? AND scope = ?",
    params = list(score_id, concept_id, scope))
  if (nrow(existing) > 0)
    cli::cli_abort("Component {.val {concept_id}} / {.val {scope}} already in score {.val {score_id}}.")

  DBI::dbExecute(ctx$connection,
    "INSERT INTO composite_score_components (score_id, concept_id, scope, version, weight)
     VALUES (?, ?, ?, ?, ?)",
    params = list(score_id, concept_id, scope, stored_version, weight))

  invisible(ctx)
}

#' Remove a component from a score
#'
#' @param ctx A `ConceptContext`.
#' @param score_id Score identifier.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @return `ctx` invisibly.
#' @export
cpt_remove_score_component <- function(ctx, score_id, concept_id, scope) {
  .assert_ctx(ctx)
  DBI::dbExecute(ctx$connection,
    "DELETE FROM composite_score_components
     WHERE score_id = ? AND concept_id = ? AND scope = ?",
    params = list(score_id, concept_id, scope))
  invisible(ctx)
}

# --- internal helpers ---

.get_score_row <- function(ctx, score_id) {
  row <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM composite_scores WHERE score_id = ?",
    params = list(score_id))
  if (nrow(row) == 0)
    cli::cli_abort("Score {.val {score_id}} not found.")
  row[1, , drop = FALSE]
}

.get_score_components <- function(ctx, score_id) {
  DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM composite_score_components WHERE score_id = ?",
    params = list(score_id))
}

.assign_tier <- function(scores, thresholds) {
  # thresholds is a named list, e.g. list(low=30, medium=60, high=80)
  # Assigns a tier label based on the highest threshold not exceeded
  thresh_vals  <- unlist(thresholds)
  thresh_names <- names(thresholds)
  ordered_idx  <- order(thresh_vals)
  sorted_vals  <- thresh_vals[ordered_idx]
  sorted_names <- thresh_names[ordered_idx]

  tiers <- character(length(scores))
  for (i in seq_along(scores)) {
    s <- scores[i]
    tier <- sorted_names[length(sorted_names)]  # highest tier by default
    for (j in seq_along(sorted_vals)) {
      if (s < sorted_vals[j]) {
        tier <- if (j == 1) paste0("below_", sorted_names[j]) else sorted_names[j - 1]
        break
      }
    }
    tiers[i] <- tier
  }
  tiers
}
