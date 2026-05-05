test_that("cpt_define creates a concept", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter", description = "LOS > 3")
  rows <- DBI::dbGetQuery(f$con, "SELECT * FROM concept_definitions WHERE concept_id = 'long_stay'")
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$object_type_id, "Encounter")
})

test_that("cpt_define aborts on duplicate concept_id", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  expect_error(cpt_define(f$ctx, "long_stay", "Encounter"), "already exists")
})

test_that("cpt_add_version auto-assigns version numbers", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  v1 <- cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  v2 <- cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 5")
  expect_equal(v1, 1L)
  expect_equal(v2, 2L)
})

test_that("cpt_add_version aborts on empty sql_expr", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  expect_error(cpt_add_version(f$ctx, "long_stay", "clinical", ""), "non-empty")
})

test_that("cpt_activate sets status to active", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  row <- DBI::dbGetQuery(f$con,
    "SELECT status FROM concept_versions WHERE concept_id='long_stay' AND version=1")
  expect_equal(row$status, "active")
})

test_that("cpt_activate aborts on deprecated version", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_deprecate(f$ctx, "long_stay", "clinical", 1L)
  expect_error(cpt_activate(f$ctx, "long_stay", "clinical", 1L), "deprecated")
})

test_that("cpt_deprecate sets status to deprecated", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_deprecate(f$ctx, "long_stay", "clinical", 1L)

  row <- DBI::dbGetQuery(f$con,
    "SELECT status FROM concept_versions WHERE concept_id='long_stay' AND version=1")
  expect_equal(row$status, "deprecated")
})

test_that("cpt_deprecate aborts when already deprecated", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_deprecate(f$ctx, "long_stay", "clinical", 1L)
  expect_error(cpt_deprecate(f$ctx, "long_stay", "clinical", 1L), "already deprecated")
})

test_that("cpt_get_version returns active version when version=NULL", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  ver <- cpt_get_version(f$ctx, "long_stay", "clinical")
  expect_equal(ver$version, 1L)
  expect_equal(ver$status, "active")
})

test_that("cpt_list returns all concepts", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_define(f$ctx, "icu_admit", "Encounter")
  lst <- cpt_list(f$ctx)
  expect_equal(nrow(lst), 2L)
})
