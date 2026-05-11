.is_concept_context <- function(x) inherits(x, "ConceptContext")

.assert_ctx <- function(ctx) {
  if (!.is_concept_context(ctx))
    cli::cli_abort("{.arg ctx} must be a {.cls ConceptContext}.")
}

.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

.json_encode <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE)

.json_decode <- function(x) {
  if (is.na(x) || is.null(x) || nchar(x) == 0) return(NULL)
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}

.next_version <- function(ctx, concept_id, scope) {
  sql <- "SELECT COALESCE(MAX(version), 0) AS mx FROM concept_versions
          WHERE concept_id = ? AND scope = ?"
  res <- DBI::dbGetQuery(ctx$connection, sql, params = list(concept_id, scope))
  res$mx + 1L
}

.get_version_row <- function(ctx, concept_id, scope, version) {
  sql <- "SELECT * FROM concept_versions
          WHERE concept_id = ? AND scope = ? AND version = ?"
  DBI::dbGetQuery(ctx$connection, sql, params = list(concept_id, scope, version))
}

.get_active_version <- function(ctx, concept_id, scope) {
  sql <- "SELECT * FROM concept_versions
          WHERE concept_id = ? AND scope = ? AND status = 'active'
          ORDER BY version DESC LIMIT 1"
  DBI::dbGetQuery(ctx$connection, sql, params = list(concept_id, scope))
}

.resolve_version <- function(ctx, concept_id, scope, version) {
  if (!is.null(version)) {
    row <- .get_version_row(ctx, concept_id, scope, version)
    if (nrow(row) == 0)
      cli::cli_abort("No version {version} for concept {.val {concept_id}} / scope {.val {scope}}.")
    if (row$status == "deprecated")
      cli::cli_abort(
        "Version {version} of {.val {concept_id}} / {.val {scope}} is deprecated. Supply an explicit version to override.")
    return(row)
  }
  row <- .get_active_version(ctx, concept_id, scope)
  if (nrow(row) == 0)
    cli::cli_abort(
      "No active version for concept {.val {concept_id}} / scope {.val {scope}}. Activate one first or supply explicit version.")
  row
}

.resolve_version_any <- function(ctx, concept_id, scope, version) {
  if (!is.null(version)) {
    row <- .get_version_row(ctx, concept_id, scope, version)
    if (nrow(row) == 0)
      cli::cli_abort("No version {version} for concept {.val {concept_id}} / scope {.val {scope}}.")
    return(row)
  }
  row <- .get_active_version(ctx, concept_id, scope)
  if (nrow(row) == 0)
    cli::cli_abort(
      "No active version for concept {.val {concept_id}} / scope {.val {scope}}.")
  row
}

# Stage-aware resolver used by cpt_evaluate() when version = NULL.
# Priority: production (silent) > staging (warn) > development (stronger warn) > abort.
.resolve_version_for_eval <- function(ctx, concept_id, scope) {
  row <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM concept_versions
     WHERE concept_id = ? AND scope = ? AND stage = 'production' AND status = 'active'
     ORDER BY version DESC LIMIT 1",
    params = list(concept_id, scope))
  if (nrow(row) > 0) return(row)

  row <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM concept_versions
     WHERE concept_id = ? AND scope = ? AND stage = 'staging' AND status = 'active'
     ORDER BY version DESC LIMIT 1",
    params = list(concept_id, scope))
  if (nrow(row) > 0) {
    cli::cli_warn(
      "Evaluating {.val {concept_id}} / {.val {scope}} using a staging version \\
       (v{row$version[1]}). Promote to production for use in automated pipelines.")
    return(row)
  }

  row <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM concept_versions
     WHERE concept_id = ? AND scope = ? AND stage = 'development' AND status = 'active'
     ORDER BY version DESC LIMIT 1",
    params = list(concept_id, scope))
  if (nrow(row) > 0) {
    cli::cli_warn(
      "Evaluating {.val {concept_id}} / {.val {scope}} using a development version \\
       (v{row$version[1]}). Not suitable for production pipelines. \\
       Promote to staging then production before use in downstream decisions.")
    return(row)
  }

  cli::cli_abort(
    "No active version for concept {.val {concept_id}} / scope {.val {scope}}. \\
     Activate one first or supply an explicit version number.")
}

.get_def_row <- function(ctx, concept_id) {
  DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM concept_definitions WHERE concept_id = ?",
    params = list(concept_id))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
