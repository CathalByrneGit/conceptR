#' Create a concept context
#'
#' Initialises the concept and audit schema tables and returns a `ConceptContext`
#' ready for lifecycle, evaluation, and audit operations.
#'
#' @param bundle An `ontology_bundle` from `ontologySpecR`.
#' @param connection A `DBIConnection`.
#' @param actor Character scalar. Identity of the current actor (default: OS user).
#' @return A `ConceptContext` object.
#' @export
concept_context <- function(bundle, connection,
                              actor = Sys.info()[["user"]]) {
  if (!inherits(bundle, "ontology_bundle"))
    cli::cli_abort("{.arg bundle} must be an {.cls ontology_bundle}.")
  if (!inherits(connection, "DBIConnection"))
    cli::cli_abort("{.arg connection} must be a {.cls DBIConnection}.")
  assert_that(is.character(actor) && length(actor) == 1L && nzchar(actor),
              "actor must be a non-empty character scalar")

  ctx <- structure(
    list(
      bundle     = bundle,
      connection = connection,
      actor      = actor,
      policies   = .default_policies()
    ),
    class = "ConceptContext"
  )
  .init_schema(ctx)
  ctx
}

#' Create an audit context (deprecated wrapper)
#'
#' `audit_context()` is a compatibility shim. The preferred approach is to pass
#' `actor` directly to [concept_context()]. Calling `audit_context()` on an
#' existing `ConceptContext` returns the same context with `actor` updated.
#'
#' @param concept_ctx A `ConceptContext` returned by [concept_context()].
#' @param actor Character scalar. The identity of the current actor.
#' @return The `ConceptContext` with `actor` set.
#' @export
audit_context <- function(concept_ctx, actor = Sys.info()[["user"]]) {
  assert_that(inherits(concept_ctx, "ConceptContext"),
              "concept_ctx must be a ConceptContext object from concept_context()")
  assert_that(is.character(actor) && length(actor) == 1L && nzchar(actor),
              "actor must be a non-empty character scalar")
  concept_ctx$actor <- actor
  concept_ctx
}

.init_schema <- function(ctx) {
  con <- ctx$connection

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS concept_definitions (
      concept_id      TEXT    NOT NULL,
      object_type_id  TEXT    NOT NULL,
      description     TEXT,
      created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (concept_id)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS concept_versions (
      concept_id            TEXT    NOT NULL,
      scope                 TEXT    NOT NULL,
      version               INTEGER NOT NULL,
      sql_expr              TEXT    NOT NULL,
      status                TEXT    NOT NULL DEFAULT 'draft',
      stage                 TEXT    NOT NULL DEFAULT 'development',
      rationale             TEXT,
      source_standard       TEXT,
      template_id           TEXT,
      parameter_values_json TEXT,
      created_at            TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      activated_at          TIMESTAMP,
      deprecated_at         TIMESTAMP,
      PRIMARY KEY (concept_id, scope, version)
    )
  ")

  # Migration: add stage column to databases created before this change.
  existing_cols <- DBI::dbListFields(con, "concept_versions")
  if (!"stage" %in% existing_cols) {
    DBI::dbExecute(con,
      "ALTER TABLE concept_versions ADD COLUMN stage TEXT NOT NULL DEFAULT 'development'")
  }

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS concept_templates (
      template_id     TEXT    NOT NULL,
      object_type_id  TEXT    NOT NULL,
      base_sql_expr   TEXT    NOT NULL,
      parameters_json TEXT    NOT NULL,
      source_standard TEXT,
      description     TEXT,
      created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (template_id)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS composite_scores (
      score_id        TEXT    NOT NULL,
      object_type_id  TEXT    NOT NULL,
      description     TEXT,
      aggregation     TEXT    NOT NULL DEFAULT 'weighted_sum',
      thresholds_json TEXT,
      created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (score_id)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS composite_score_components (
      score_id    TEXT    NOT NULL,
      concept_id  TEXT    NOT NULL,
      scope       TEXT    NOT NULL,
      version     INTEGER NOT NULL,
      weight      REAL    NOT NULL DEFAULT 1.0,
      PRIMARY KEY (score_id, concept_id, scope, version)
    )
  ")

  .init_audit_schema(con)

  invisible(ctx)
}

.default_policies <- function() {
  list(
    activate       = c("ontology_admin", "concept_owner"),
    deprecate      = c("ontology_admin", "concept_owner"),
    approve_action = c("ontology_admin", "approver"),
    view_audit     = c("ontology_admin", "auditor", "concept_owner")
  )
}

.init_audit_schema <- function(con) {
  stmts <- c(
    "CREATE TABLE IF NOT EXISTS audit_samples (
      sample_id    TEXT      NOT NULL,
      concept_id   TEXT      NOT NULL,
      scope        TEXT      NOT NULL,
      version      INTEGER   NOT NULL,
      object_key   TEXT      NOT NULL,
      system_value BOOLEAN   NOT NULL,
      sampled_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      reviewed     BOOLEAN   NOT NULL DEFAULT FALSE,
      PRIMARY KEY (sample_id)
    )",
    "CREATE TABLE IF NOT EXISTS audit_judgments (
      judgment_id    TEXT      NOT NULL,
      sample_id      TEXT      NOT NULL,
      concept_id     TEXT      NOT NULL,
      scope          TEXT      NOT NULL,
      version        INTEGER   NOT NULL,
      object_key     TEXT      NOT NULL,
      system_value   BOOLEAN   NOT NULL,
      reviewer_value BOOLEAN   NOT NULL,
      reviewer_id    TEXT      NOT NULL,
      reviewed_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      notes          TEXT,
      PRIMARY KEY (judgment_id),
      FOREIGN KEY (sample_id) REFERENCES audit_samples(sample_id)
    )",
    "CREATE TABLE IF NOT EXISTS drift_events (
      drift_id           TEXT      NOT NULL,
      concept_id         TEXT      NOT NULL,
      scope              TEXT      NOT NULL,
      version            INTEGER   NOT NULL,
      detected_at        TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      disagreement_rate  REAL      NOT NULL,
      threshold          REAL      NOT NULL,
      status             TEXT      NOT NULL DEFAULT 'open',
      resolved_at        TIMESTAMP,
      resolved_by        TEXT,
      resolution_notes   TEXT,
      PRIMARY KEY (drift_id)
    )",
    "CREATE TABLE IF NOT EXISTS governance_log (
      log_id        TEXT      NOT NULL,
      action        TEXT      NOT NULL,
      concept_id    TEXT,
      scope         TEXT,
      version       INTEGER,
      actor         TEXT      NOT NULL,
      performed_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      evidence_json TEXT,
      notes         TEXT,
      PRIMARY KEY (log_id)
    )",
    "CREATE TABLE IF NOT EXISTS rbac_principals (
      principal_id TEXT      NOT NULL,
      roles_json   TEXT      NOT NULL DEFAULT '[]',
      created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (principal_id)
    )"
  )
  for (stmt in stmts) DBI::dbExecute(con, stmt)
  invisible(NULL)
}
