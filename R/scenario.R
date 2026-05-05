#' Scenario analysis: compare a proposed SQL expression to the current active version
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param proposed_sql Proposed SQL expression.
#' @param current_version Version to compare against (`NULL` = active version).
#' @return A list with current/proposed counts, newly included/excluded rows, and
#'   agreement rate.
#' @export
cpt_scenario <- function(ctx, concept_id, scope, proposed_sql,
                           current_version = NULL) {
  .assert_ctx(ctx)
  ver_row <- .resolve_version(ctx, concept_id, scope, current_version)
  current_sql <- ver_row$sql_expr[1]

  ot_id <- .get_def_row(ctx, concept_id)$object_type_id
  ot    <- .resolve_object_type(ctx, ot_id)
  pk    <- ot$primary_key

  df_current  <- .eval_sql(ctx, ot$table_name, pk, concept_id, current_sql)
  df_proposed <- .eval_sql(ctx, ot$table_name, pk, paste0(concept_id, "_proposed"), proposed_sql)

  col_curr <- concept_id
  col_prop <- paste0(concept_id, "_proposed")

  ids_current  <- df_current[[pk]][df_current[[col_curr]]  == TRUE]
  ids_proposed <- df_proposed[[pk]][df_proposed[[col_prop]] == TRUE]

  all_ids <- union(df_current[[pk]], df_proposed[[pk]])
  total   <- length(all_ids)

  curr_set <- as.character(df_current[[pk]][df_current[[col_curr]] == TRUE])
  prop_set <- as.character(df_proposed[[pk]][df_proposed[[col_prop]] == TRUE])

  newly_included_ids <- setdiff(prop_set, curr_set)
  newly_excluded_ids <- setdiff(curr_set, prop_set)

  newly_included <- df_current[df_current[[pk]] %in% newly_included_ids, , drop = FALSE]
  newly_excluded <- df_current[df_current[[pk]] %in% newly_excluded_ids, , drop = FALSE]

  both_true  <- length(intersect(curr_set, prop_set))
  both_false <- total - length(union(curr_set, prop_set))
  agreement_rate <- if (total > 0) (both_true + both_false) / total else NA_real_

  list(
    current_sql      = current_sql,
    proposed_sql     = proposed_sql,
    current_count    = length(curr_set),
    proposed_count   = length(prop_set),
    newly_included   = newly_included,
    newly_excluded   = newly_excluded,
    agreement_rate   = agreement_rate
  )
}

#' Compare multiple proposed SQL expressions against the current active version
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param proposals Named list of SQL strings.
#' @return A data frame with one row per scenario.
#' @export
cpt_compare_scenarios <- function(ctx, concept_id, scope, proposals) {
  .assert_ctx(ctx)
  if (length(proposals) == 0)
    cli::cli_abort("{.arg proposals} must be a non-empty named list.")

  rows <- lapply(names(proposals), function(nm) {
    result <- cpt_scenario(ctx, concept_id, scope,
                             proposed_sql    = proposals[[nm]],
                             current_version = NULL)
    data.frame(
      scenario_name    = nm,
      count            = result$proposed_count,
      newly_included   = nrow(result$newly_included),
      newly_excluded   = nrow(result$newly_excluded),
      agreement_rate   = result$agreement_rate,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
