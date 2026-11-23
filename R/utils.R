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

# Resolve an active version integer (for audit functions that need an integer, not a row).
.resolve_version_int <- function(ctx, concept_id, scope, version) {
  if (!is.null(version)) return(as.integer(version))
  row <- .get_active_version(ctx, concept_id, scope)
  if (nrow(row) == 0L)
    cli::cli_abort("No active version for {concept_id}/{scope}.")
  as.integer(row$version[1L])
}

.get_def_row <- function(ctx, concept_id) {
  DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM concept_definitions WHERE concept_id = ?",
    params = list(concept_id))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# Object-type helpers (bundle reading is delegated to ontologySpecR)
# ---------------------------------------------------------------------------

# The one place conceptR builds table references. Query-backed types abort in
# ontologySpecR::object_table_id() and are not supported yet.
.table_ref <- function(ot) {
  id <- ontologySpecR::object_table_id(ot)
  if (is.null(id$schema)) id$table
  else DBI::Id(schema = id$schema, table = id$table)
}

.pk_col <- function(ot) {
  pk <- ontologySpecR::object_pk(ot)
  if (length(pk) > 1L)
    cli::cli_abort(
      "Object type {.val {ot$id}} has a composite primary key; single-column PKs are required.")
  pk
}

# ---------------------------------------------------------------------------
# Shared helpers (originally from auditR)
# ---------------------------------------------------------------------------

new_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    paste0(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  )
}

assert_that <- function(cond, msg) {
  if (!isTRUE(cond)) cli::cli_abort(msg)
}

parse_roles <- function(roles_json) {
  if (is.null(roles_json) || is.na(roles_json) || nchar(trimws(roles_json)) == 0)
    return(character(0))
  tryCatch(jsonlite::fromJSON(roles_json), error = function(e) character(0))
}

serialise_roles <- function(roles) jsonlite::toJSON(unique(roles), auto_unbox = FALSE)

serialise_key <- function(...) {
  cols <- list(...)
  if (length(cols) == 1L) return(as.character(cols[[1L]]))
  do.call(paste, c(lapply(cols, as.character), sep = "::"))
}

quietly_execute <- function(con, sql) {
  DBI::dbExecute(con, sql)
  invisible(NULL)
}
