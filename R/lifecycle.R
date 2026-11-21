#' Define a new concept
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Unique concept identifier.
#' @param object_type_id Object type this concept applies to.
#' @param description Optional description.
#' @return `ctx` invisibly.
#' @export
cpt_define <- function(ctx, concept_id, object_type_id, description = NULL) {
  .assert_ctx(ctx)
  existing <- .get_def_row(ctx, concept_id)
  if (nrow(existing) > 0)
    cli::cli_abort("Concept {.val {concept_id}} already exists.")

  DBI::dbExecute(ctx$connection,
    "INSERT INTO concept_definitions (concept_id, object_type_id, description, created_at)
     VALUES (?, ?, ?, ?)",
    params = list(concept_id, object_type_id, description %||% NA_character_, .now()))

  invisible(ctx)
}

#' Add a version to a concept
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string (e.g. `"clinical"`, `"operations"`).
#' @param sql_expr SQL boolean expression evaluated against the object table.
#' @param status One of `"draft"`, `"active"`, `"deprecated"`.
#' @param rationale Optional rationale.
#' @param source_standard Optional source standard reference.
#' @param template_id Optional template this version was derived from.
#' @param parameter_values Optional named list of parameter values.
#' @return The new version number invisibly.
#' @export
cpt_add_version <- function(ctx, concept_id, scope, sql_expr,
                             status = "draft", rationale = NULL,
                             source_standard = NULL,
                             template_id = NULL, parameter_values = NULL) {
  .assert_ctx(ctx)
  def <- .get_def_row(ctx, concept_id)
  if (nrow(def) == 0)
    cli::cli_abort("Concept {.val {concept_id}} not found. Call {.fn cpt_define} first.")
  if (!status %in% c("draft", "active", "deprecated"))
    cli::cli_abort("{.arg status} must be one of 'draft', 'active', 'deprecated'.")
  if (is.null(sql_expr) || nchar(trimws(sql_expr)) == 0)
    cli::cli_abort("{.arg sql_expr} must be a non-empty string.")

  version <- .next_version(ctx, concept_id, scope)
  param_json <- if (!is.null(parameter_values)) .json_encode(parameter_values) else NA_character_
  activated_at <- if (status == "active") .now() else NA_character_
  deprecated_at <- if (status == "deprecated") .now() else NA_character_

  DBI::dbExecute(ctx$connection,
    "INSERT INTO concept_versions
       (concept_id, scope, version, sql_expr, status, stage, rationale, source_standard,
        template_id, parameter_values_json, created_at, activated_at, deprecated_at)
     VALUES (?, ?, ?, ?, ?, 'development', ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      concept_id, scope, version, sql_expr, status,
      rationale %||% NA_character_,
      source_standard %||% NA_character_,
      template_id %||% NA_character_,
      param_json,
      .now(), activated_at, deprecated_at
    ))

  invisible(version)
}

#' Activate a concept version
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param version Version number.
#' @return `ctx` invisibly.
#' @export
cpt_activate <- function(ctx, concept_id, scope, version) {
  .assert_ctx(ctx)
  row <- .get_version_row(ctx, concept_id, scope, version)
  if (nrow(row) == 0)
    cli::cli_abort("Version {version} of {.val {concept_id}} / {.val {scope}} not found.")
  if (row$status == "deprecated")
    cli::cli_abort("Cannot activate a deprecated version.")

  active <- .get_active_version(ctx, concept_id, scope)
  if (nrow(active) > 0 && active$version != version)
    cli::cli_warn("Version {active$version} of {.val {concept_id}} / {.val {scope}} is already active. Coexistence allowed.")

  DBI::dbExecute(ctx$connection,
    "UPDATE concept_versions SET status = 'active', activated_at = ?
     WHERE concept_id = ? AND scope = ? AND version = ?",
    params = list(.now(), concept_id, scope, version))

  invisible(ctx)
}

#' Deprecate a concept version
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param version Version number.
#' @return `ctx` invisibly.
#' @export
cpt_deprecate <- function(ctx, concept_id, scope, version) {
  .assert_ctx(ctx)
  row <- .get_version_row(ctx, concept_id, scope, version)
  if (nrow(row) == 0)
    cli::cli_abort("Version {version} of {.val {concept_id}} / {.val {scope}} not found.")
  if (row$status == "deprecated")
    cli::cli_abort("Version {version} of {.val {concept_id}} / {.val {scope}} is already deprecated.")

  DBI::dbExecute(ctx$connection,
    "UPDATE concept_versions SET status = 'deprecated', deprecated_at = ?
     WHERE concept_id = ? AND scope = ? AND version = ?",
    params = list(.now(), concept_id, scope, version))

  invisible(ctx)
}

#' Get a concept version
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Concept identifier.
#' @param scope Scope string.
#' @param version Version number. If `NULL`, returns the active version.
#' @return A list with version details.
#' @export
cpt_get_version <- function(ctx, concept_id, scope, version = NULL) {
  .assert_ctx(ctx)
  row <- .resolve_version(ctx, concept_id, scope, version)
  as.list(row[1, , drop = FALSE])
}

#' List concepts
#'
#' @param ctx A `ConceptContext`.
#' @param object_type_id Optional filter by object type.
#' @param status Optional filter by version status.
#' @return A data frame.
#' @export
cpt_list <- function(ctx, object_type_id = NULL, status = NULL) {
  .assert_ctx(ctx)

  sql <- "
    SELECT d.concept_id, d.object_type_id, d.description,
           COUNT(v.version) AS version_count,
           d.created_at
    FROM concept_definitions d
    LEFT JOIN concept_versions v ON d.concept_id = v.concept_id
  "
  where <- character(0)
  params <- list()
  if (!is.null(object_type_id)) {
    where <- c(where, "d.object_type_id = ?")
    params <- c(params, list(object_type_id))
  }
  if (!is.null(status)) {
    where <- c(where, "v.status = ?")
    params <- c(params, list(status))
  }
  if (length(where) > 0)
    sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  sql <- paste(sql, "GROUP BY d.concept_id, d.object_type_id, d.description, d.created_at")

  DBI::dbGetQuery(ctx$connection, sql, params = params)
}

#' Promote a concept version to a higher stage
#'
#' Valid transitions: `development` -> `staging`, `staging` -> `production`.
#' Promoting to `production` runs the promotion gate (RBAC + drift check) and
#' soft-demotes any existing production version to `staging`.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer.
#' @param to_stage One of `"staging"` or `"production"`.
#' @param notes Optional character note recorded in the governance log.
#' @param force Logical. If `TRUE`, bypass the promotion gate with a warning
#'   and log entry (for emergency overrides). Default `FALSE`.
#' @return `ctx` invisibly.
#' @export
cpt_promote <- function(ctx, concept_id, scope, version,
                         to_stage = c("staging", "production"),
                         notes = NULL, force = FALSE) {
  .assert_ctx(ctx)
  to_stage <- match.arg(to_stage)

  row <- .get_version_row(ctx, concept_id, scope, version)
  if (nrow(row) == 0)
    cli::cli_abort("Version {version} of {.val {concept_id}} / {.val {scope}} not found.")
  if (row$status == "deprecated")
    cli::cli_abort("Cannot promote a deprecated version.")

  current_stage <- row$stage
  valid_from <- switch(to_stage,
    staging    = "development",
    production = "staging"
  )
  if (current_stage != valid_from)
    cli::cli_abort(
      "Cannot promote from stage {.val {current_stage}} to {.val {to_stage}}. \\
       Expected current stage: {.val {valid_from}}.")

  if (to_stage == "production") {
    .check_promotion(ctx, concept_id, scope, version, force = force)
    # Soft-demote any existing production version for this (concept, scope)
    DBI::dbExecute(ctx$connection,
      "UPDATE concept_versions SET stage = 'staging'
       WHERE concept_id = ? AND scope = ? AND stage = 'production' AND version != ?",
      params = list(concept_id, scope, version))
  }

  DBI::dbExecute(ctx$connection,
    "UPDATE concept_versions SET stage = ?
     WHERE concept_id = ? AND scope = ? AND version = ?",
    params = list(to_stage, concept_id, scope, version))

  adt_log(ctx, action = paste0("promote_to_", to_stage),
          concept_id = concept_id, scope = scope, version = as.integer(version),
          notes = notes)

  invisible(ctx)
}

#' Roll back a concept version to a lower stage
#'
#' Valid transitions: `staging` -> `development`, `production` -> `staging`.
#' A `reason` is required.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer.
#' @param to_stage One of `"development"` or `"staging"`.
#' @param reason Character. Required; must explain why rolling back.
#' @return `ctx` invisibly.
#' @export
cpt_rollback <- function(ctx, concept_id, scope, version,
                          to_stage = c("development", "staging"),
                          reason) {
  .assert_ctx(ctx)
  to_stage <- match.arg(to_stage)

  if (missing(reason) || is.null(reason) || nchar(trimws(reason)) == 0)
    cli::cli_abort("{.arg reason} is required for rollback.")

  row <- .get_version_row(ctx, concept_id, scope, version)
  if (nrow(row) == 0)
    cli::cli_abort("Version {version} of {.val {concept_id}} / {.val {scope}} not found.")

  current_stage <- row$stage
  valid_from <- switch(to_stage,
    development = "staging",
    staging     = "production"
  )
  if (current_stage != valid_from)
    cli::cli_abort(
      "Cannot roll back from stage {.val {current_stage}} to {.val {to_stage}}. \\
       Expected current stage: {.val {valid_from}}.")

  DBI::dbExecute(ctx$connection,
    "UPDATE concept_versions SET stage = ?
     WHERE concept_id = ? AND scope = ? AND version = ?",
    params = list(to_stage, concept_id, scope, version))

  invisible(ctx)
}

#' Get the production version for a concept/scope
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @return A list with the version row, or `NULL` if no production version exists.
#' @export
cpt_production_version <- function(ctx, concept_id, scope) {
  .assert_ctx(ctx)
  row <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM concept_versions
     WHERE concept_id = ? AND scope = ? AND stage = 'production'
     ORDER BY version DESC LIMIT 1",
    params = list(concept_id, scope))
  if (nrow(row) == 0) return(NULL)
  as.list(row[1, , drop = FALSE])
}

#' List versions filtered by stage
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Optional character filter.
#' @param scope Optional character filter.
#' @param stage One of `"development"`, `"staging"`, `"production"`, or `NULL`
#'   for all stages.
#' @return A data frame.
#' @export
cpt_versions_by_stage <- function(ctx, concept_id = NULL, scope = NULL,
                                   stage = NULL) {
  .assert_ctx(ctx)
  valid_stages <- c("development", "staging", "production")
  if (!is.null(stage) && !stage %in% valid_stages)
    cli::cli_abort("{.arg stage} must be one of {.val {valid_stages}} or NULL.")

  sql    <- "SELECT * FROM concept_versions WHERE 1=1"
  params <- list()
  if (!is.null(concept_id)) { sql <- paste(sql, "AND concept_id = ?"); params <- c(params, list(concept_id)) }
  if (!is.null(scope))      { sql <- paste(sql, "AND scope = ?");      params <- c(params, list(scope)) }
  if (!is.null(stage))      { sql <- paste(sql, "AND stage = ?");      params <- c(params, list(stage)) }
  sql <- paste(sql, "ORDER BY concept_id, scope, version")

  DBI::dbGetQuery(ctx$connection, sql, params = params)
}
