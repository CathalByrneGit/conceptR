#' Create a concept context
#'
#' @param bundle An `ontology_bundle` from `ontologySpecR`.
#' @param connection A `DBIConnection`.
#' @return A `ConceptContext` object.
#' @export
concept_context <- function(bundle, connection) {
  if (!inherits(bundle, "ontology_bundle"))
    cli::cli_abort("{.arg bundle} must be an {.cls ontology_bundle}.")
  if (!inherits(connection, "DBIConnection"))
    cli::cli_abort("{.arg connection} must be a {.cls DBIConnection}.")

  ctx <- structure(
    list(bundle = bundle, connection = connection),
    class = "ConceptContext"
  )
  .init_schema(ctx)
  ctx
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

  invisible(ctx)
}
