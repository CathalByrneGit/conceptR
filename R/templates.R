#' Define a concept template
#'
#' @param ctx A `ConceptContext`.
#' @param template_id Unique template identifier.
#' @param object_type_id Object type this template applies to.
#' @param base_sql_expr SQL expression with `{{param_name}}` placeholders.
#' @param parameters Named list: `name -> list(type, default, description)`.
#' @param source_standard Optional source standard.
#' @param description Optional description.
#' @return `ctx` invisibly.
#' @export
cpt_define_template <- function(ctx, template_id, object_type_id,
                                  base_sql_expr, parameters,
                                  source_standard = NULL, description = NULL) {
  .assert_ctx(ctx)
  existing <- DBI::dbGetQuery(ctx$connection,
    "SELECT template_id FROM concept_templates WHERE template_id = ?",
    params = list(template_id))
  if (nrow(existing) > 0)
    cli::cli_abort("Template {.val {template_id}} already exists.")

  placeholders <- .extract_placeholders(base_sql_expr)
  missing_params <- setdiff(placeholders, names(parameters))
  if (length(missing_params) > 0)
    cli::cli_abort(
      "Placeholder(s) {.val {missing_params}} in {.arg base_sql_expr} have no corresponding parameter definition.")

  DBI::dbExecute(ctx$connection,
    "INSERT INTO concept_templates
       (template_id, object_type_id, base_sql_expr, parameters_json,
        source_standard, description, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      template_id, object_type_id, base_sql_expr,
      .json_encode(parameters),
      source_standard %||% NA_character_,
      description %||% NA_character_,
      .now()
    ))

  invisible(ctx)
}

#' Create a concept from a template
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id New concept identifier.
#' @param template_id Template to use.
#' @param scope Scope string.
#' @param parameter_values Named list of parameter values.
#' @param rationale Optional rationale.
#' @param status Initial status (`"draft"` by default).
#' @return The new version number invisibly.
#' @export
cpt_inherit <- function(ctx, concept_id, template_id, scope,
                          parameter_values, rationale = NULL,
                          status = "draft") {
  .assert_ctx(ctx)
  tmpl <- .get_template_row(ctx, template_id)

  rendered_sql <- .render_template_sql(tmpl$base_sql_expr, parameter_values)

  def <- .get_def_row(ctx, concept_id)
  if (nrow(def) == 0)
    cpt_define(ctx, concept_id, tmpl$object_type_id)

  cpt_add_version(ctx, concept_id, scope, rendered_sql,
                   status = status, rationale = rationale,
                   template_id = template_id,
                   parameter_values = parameter_values)
}

#' Render a template without creating database rows
#'
#' @param ctx A `ConceptContext`.
#' @param template_id Template identifier.
#' @param parameter_values Named list of parameter values.
#' @return Rendered SQL string.
#' @export
cpt_render_template <- function(ctx, template_id, parameter_values) {
  .assert_ctx(ctx)
  tmpl <- .get_template_row(ctx, template_id)
  .render_template_sql(tmpl$base_sql_expr, parameter_values)
}

#' Compare all variants derived from a template
#'
#' @param ctx A `ConceptContext`.
#' @param template_id Template identifier.
#' @return A data frame with concept_id, scope, version, and parameter columns.
#' @export
cpt_compare_template_variants <- function(ctx, template_id) {
  .assert_ctx(ctx)
  tmpl <- .get_template_row(ctx, template_id)
  params_def <- .json_decode(tmpl$parameters_json)
  param_names <- names(params_def)

  rows <- DBI::dbGetQuery(ctx$connection,
    "SELECT concept_id, scope, version, parameter_values_json
     FROM concept_versions WHERE template_id = ?
     ORDER BY concept_id, scope, version",
    params = list(template_id))

  if (nrow(rows) == 0) return(rows)

  param_cols <- lapply(param_names, function(p) {
    vapply(rows$parameter_values_json, function(json) {
      vals <- .json_decode(json)
      v <- vals[[p]]
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1))
  })
  names(param_cols) <- param_names

  result <- rows[, c("concept_id", "scope", "version")]
  for (nm in param_names) result[[nm]] <- param_cols[[nm]]
  result
}

#' List all templates
#'
#' @param ctx A `ConceptContext`.
#' @return A data frame with template metadata and variant count.
#' @export
cpt_list_templates <- function(ctx) {
  .assert_ctx(ctx)
  DBI::dbGetQuery(ctx$connection,
    "SELECT t.template_id, t.object_type_id, t.source_standard,
            COUNT(v.version) AS n_variants
     FROM concept_templates t
     LEFT JOIN concept_versions v ON v.template_id = t.template_id
     GROUP BY t.template_id, t.object_type_id, t.source_standard")
}

# --- internal helpers ---

.get_template_row <- function(ctx, template_id) {
  row <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM concept_templates WHERE template_id = ?",
    params = list(template_id))
  if (nrow(row) == 0)
    cli::cli_abort("Template {.val {template_id}} not found.")
  row[1, , drop = FALSE]
}

.extract_placeholders <- function(sql) {
  m <- gregexpr("\\{\\{([A-Za-z_][A-Za-z0-9_]*)\\}\\}", sql, perl = TRUE)
  raw <- regmatches(sql, m)[[1]]
  gsub("^\\{\\{|\\}\\}$", "", raw)
}

.render_template_sql <- function(base_sql, parameter_values) {
  placeholders <- .extract_placeholders(base_sql)
  missing_vals <- setdiff(placeholders, names(parameter_values))
  if (length(missing_vals) > 0)
    cli::cli_abort("Missing parameter value(s): {.val {missing_vals}}.")

  sql <- base_sql
  for (nm in names(parameter_values)) {
    sql <- gsub(paste0("\\{\\{", nm, "\\}\\}"), as.character(parameter_values[[nm]]), sql)
  }
  sql
}
