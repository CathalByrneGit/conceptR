test_that("cpt_define_template stores a template", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define_template(f$ctx, "los_threshold", "Encounter",
    base_sql_expr = "length_of_stay > {{threshold}}",
    parameters    = list(threshold = list(type = "numeric", default = 3, description = "LOS cutoff")))

  rows <- DBI::dbGetQuery(f$con, "SELECT * FROM concept_templates WHERE template_id = 'los_threshold'")
  expect_equal(nrow(rows), 1L)
})

test_that("cpt_define_template aborts on missing parameter definition", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  expect_error(
    cpt_define_template(f$ctx, "bad_tmpl", "Encounter",
      base_sql_expr = "length_of_stay > {{threshold}}",
      parameters    = list()),
    "no corresponding parameter"
  )
})

test_that("cpt_render_template substitutes parameters correctly", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define_template(f$ctx, "los_threshold", "Encounter",
    base_sql_expr = "length_of_stay > {{threshold}}",
    parameters    = list(threshold = list(type = "numeric", default = 3, description = "LOS cutoff")))

  rendered <- cpt_render_template(f$ctx, "los_threshold", list(threshold = 7))
  expect_equal(rendered, "length_of_stay > 7")
})

test_that("cpt_render_template aborts on missing parameter value", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define_template(f$ctx, "los_threshold", "Encounter",
    base_sql_expr = "length_of_stay > {{threshold}}",
    parameters    = list(threshold = list(type = "numeric", default = 3, description = "")))

  expect_error(cpt_render_template(f$ctx, "los_threshold", list()), "Missing parameter")
})

test_that("cpt_inherit creates concept and stores parameter_values", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define_template(f$ctx, "los_threshold", "Encounter",
    base_sql_expr = "length_of_stay > {{threshold}}",
    parameters    = list(threshold = list(type = "numeric", default = 3, description = "")))

  cpt_inherit(f$ctx, "long_stay_v1", "los_threshold", "clinical",
              parameter_values = list(threshold = 4))

  row <- DBI::dbGetQuery(f$con,
    "SELECT * FROM concept_versions WHERE concept_id = 'long_stay_v1'")
  expect_equal(nrow(row), 1L)
  expect_equal(row$template_id, "los_threshold")
  expect_equal(row$sql_expr, "length_of_stay > 4")
  param_vals <- jsonlite::fromJSON(row$parameter_values_json)
  expect_equal(param_vals$threshold, 4)
})

test_that("cpt_compare_template_variants returns one row per variant", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define_template(f$ctx, "los_threshold", "Encounter",
    base_sql_expr = "length_of_stay > {{threshold}}",
    parameters    = list(threshold = list(type = "numeric", default = 3, description = "")))

  cpt_inherit(f$ctx, "variant_a", "los_threshold", "clinical", list(threshold = 3))
  cpt_inherit(f$ctx, "variant_b", "los_threshold", "clinical", list(threshold = 7))

  variants <- cpt_compare_template_variants(f$ctx, "los_threshold")
  expect_equal(nrow(variants), 2L)
  expect_true("threshold" %in% names(variants))
})

test_that("cpt_list_templates returns template with n_variants", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define_template(f$ctx, "los_threshold", "Encounter",
    base_sql_expr = "length_of_stay > {{threshold}}",
    parameters    = list(threshold = list(type = "numeric", default = 3, description = "")))

  cpt_inherit(f$ctx, "variant_a", "los_threshold", "clinical", list(threshold = 3))

  tmpl_list <- cpt_list_templates(f$ctx)
  expect_equal(nrow(tmpl_list), 1L)
  expect_equal(tmpl_list$n_variants, 1L)
})
