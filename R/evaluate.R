#' Evaluate a concept against its object type
#'
#' When `version = NULL`, resolution prefers `production` over `staging` over
#' `development` (issuing warnings for non-production stages). Supply an explicit
#' `version` to bypass stage-aware resolution.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param version Version number. If `NULL`, uses stage-aware resolution.
#' @param object_type_id Override the stored object type.
#' @return A data frame with PK columns and a logical column named after the concept.
#' @export
cpt_evaluate <- function(ctx, concept_id, scope, version = NULL,
                          object_type_id = NULL) {
  .assert_ctx(ctx)
  ver_row <- if (is.null(version)) {
    .resolve_version_for_eval(ctx, concept_id, scope)
  } else {
    .resolve_version(ctx, concept_id, scope, version)
  }
  sql_expr <- ver_row$sql_expr[1]
  if (is.null(sql_expr) || is.na(sql_expr) || nchar(trimws(sql_expr)) == 0)
    cli::cli_abort("sql_expr for {.val {concept_id}} v{ver_row$version} is empty.")

  ot_id <- object_type_id %||% .get_def_row(ctx, concept_id)$object_type_id
  ot    <- ontologySpecR::bundle_object_type(ctx$bundle, ot_id)

  .eval_sql(ctx, .table_ref(ot), .pk_col(ot), concept_id, sql_expr)
}

#' Evaluate multiple concepts
#'
#' @param ctx A `ConceptContext`.
#' @param concept_ids Character vector of concept identifiers.
#' @param scope Scope string (applied to all concepts).
#' @param version Version number (`NULL` = active).
#' @return Named list of data frames, one per concept.
#' @export
cpt_evaluate_multi <- function(ctx, concept_ids, scope = NULL, version = NULL) {
  .assert_ctx(ctx)
  results <- lapply(concept_ids, function(cid) {
    cpt_evaluate(ctx, cid, scope = scope, version = version)
  })
  names(results) <- concept_ids
  results
}

#' Compare two versions of a concept
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param version_a First version number.
#' @param version_b Second version number.
#' @return A list with diff summary and both result data frames.
#' @export
cpt_compare_versions <- function(ctx, concept_id, scope, version_a, version_b) {
  .assert_ctx(ctx)
  row_a <- .get_version_row(ctx, concept_id, scope, version_a)
  row_b <- .get_version_row(ctx, concept_id, scope, version_b)
  if (nrow(row_a) == 0) cli::cli_abort("Version {version_a} not found.")
  if (nrow(row_b) == 0) cli::cli_abort("Version {version_b} not found.")

  ot_id <- .get_def_row(ctx, concept_id)$object_type_id
  ot    <- ontologySpecR::bundle_object_type(ctx$bundle, ot_id)
  pk    <- .pk_col(ot)

  df_a <- .eval_sql(ctx, .table_ref(ot), pk, concept_id, row_a$sql_expr[1])
  df_b <- .eval_sql(ctx, .table_ref(ot), pk, concept_id, row_b$sql_expr[1])

  col   <- concept_id
  ids_a <- df_a[[pk]][df_a[[col]] == TRUE]
  ids_b <- df_b[[pk]][df_b[[col]] == TRUE]

  list(
    total_a   = sum(df_a[[col]], na.rm = TRUE),
    total_b   = sum(df_b[[col]], na.rm = TRUE),
    match     = length(intersect(ids_a, ids_b)),
    only_a    = length(setdiff(ids_a, ids_b)),
    only_b    = length(setdiff(ids_b, ids_a)),
    results_a = df_a,
    results_b = df_b
  )
}

# --- internal helpers ---

# Push-down SQL evaluation: builds SELECT pk, (expr) AS concept_id FROM table.
# The expression is evaluated entirely in the database; no rows are pulled to R.
.eval_sql <- function(ctx, table_name, pk, concept_id, sql_expr) {
  con <- ctx$connection
  q_table   <- DBI::dbQuoteIdentifier(con, table_name)
  q_pk      <- DBI::dbQuoteIdentifier(con, pk)
  q_concept <- DBI::dbQuoteIdentifier(con, concept_id)
  query <- paste0(
    "SELECT ", q_pk,
    ", CAST((", sql_expr, ") AS BOOLEAN) AS ", q_concept,
    " FROM ", q_table
  )
  df <- DBI::dbGetQuery(con, query)
  df[[concept_id]] <- as.logical(df[[concept_id]])
  df
}
