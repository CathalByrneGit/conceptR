test_that("new database has stage column in concept_versions", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cols <- DBI::dbListFields(f$con, "concept_versions")
  expect_true("stage" %in% cols)
})

test_that("migration adds stage column to existing database without it", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Simulate a pre-staging database: create concept_versions without stage column
  DBI::dbExecute(con, "
    CREATE TABLE concept_versions (
      concept_id  TEXT    NOT NULL,
      scope       TEXT    NOT NULL,
      version     INTEGER NOT NULL,
      sql_expr    TEXT    NOT NULL,
      status      TEXT    NOT NULL DEFAULT 'draft',
      PRIMARY KEY (concept_id, scope, version)
    )
  ")

  bundle <- structure(
    list(object_types = list(
      list(id = "Encounter", table_name = "encounters", primary_key = "encounter_id")
    )),
    class = "ontology_bundle"
  )

  # concept_context() should run the migration
  concept_context(bundle, con)

  cols <- DBI::dbListFields(con, "concept_versions")
  expect_true("stage" %in% cols)

  # Existing rows (none here, but column default should be 'development')
  DBI::dbExecute(con,
    "INSERT INTO concept_versions (concept_id, scope, version, sql_expr, status)
     VALUES ('c1', 's1', 1, 'x > 1', 'draft')")
  row <- DBI::dbGetQuery(con, "SELECT stage FROM concept_versions WHERE concept_id = 'c1'")
  expect_equal(row$stage, "development")
})

test_that("new versions start at development stage", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")

  row <- DBI::dbGetQuery(f$con,
    "SELECT stage FROM concept_versions WHERE concept_id = 'long_stay'")
  expect_equal(row$stage, "development")
})

test_that("cpt_promote development -> staging succeeds", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")

  row <- DBI::dbGetQuery(f$con,
    "SELECT stage FROM concept_versions WHERE concept_id = 'long_stay' AND version = 1")
  expect_equal(row$stage, "staging")
})

test_that("cpt_promote staging -> production succeeds and warns without auditR", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")

  expect_warning(
    cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "production"),
    "auditR"
  )

  row <- DBI::dbGetQuery(f$con,
    "SELECT stage FROM concept_versions WHERE concept_id = 'long_stay' AND version = 1")
  expect_equal(row$stage, "production")
})

test_that("cpt_promote to production demotes existing production to staging", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 5")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_activate(f$ctx, "long_stay", "clinical", 2L)
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")
  suppressWarnings(
    cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "production"))
  cpt_promote(f$ctx, "long_stay", "clinical", 2L, to_stage = "staging")

  suppressWarnings(
    cpt_promote(f$ctx, "long_stay", "clinical", 2L, to_stage = "production"))

  rows <- DBI::dbGetQuery(f$con,
    "SELECT version, stage FROM concept_versions WHERE concept_id = 'long_stay'
     ORDER BY version")
  expect_equal(rows$stage[rows$version == 1L], "staging")  # demoted
  expect_equal(rows$stage[rows$version == 2L], "production")
})

test_that("cpt_promote aborts on invalid stage transition", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")

  # Cannot jump development -> production directly
  expect_error(
    cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "production"),
    "Expected current stage"
  )
})

test_that("cpt_promote aborts on deprecated version", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_deprecate(f$ctx, "long_stay", "clinical", 1L)

  expect_error(
    cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging"),
    "deprecated"
  )
})

test_that("cpt_rollback staging -> development succeeds", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")

  cpt_rollback(f$ctx, "long_stay", "clinical", 1L,
               to_stage = "development", reason = "Expression needs revision")

  row <- DBI::dbGetQuery(f$con,
    "SELECT stage FROM concept_versions WHERE concept_id = 'long_stay' AND version = 1")
  expect_equal(row$stage, "development")
})

test_that("cpt_rollback production -> staging leaves no production version", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")
  suppressWarnings(
    cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "production"))

  cpt_rollback(f$ctx, "long_stay", "clinical", 1L,
               to_stage = "staging", reason = "Incident in prod")

  expect_null(cpt_production_version(f$ctx, "long_stay", "clinical"))
})

test_that("cpt_rollback requires a reason", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")

  expect_error(
    cpt_rollback(f$ctx, "long_stay", "clinical", 1L, to_stage = "development"),
    "reason"
  )
})

test_that("cpt_rollback aborts on invalid stage transition", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  # Stage is development; cannot roll back to staging from development
  expect_error(
    cpt_rollback(f$ctx, "long_stay", "clinical", 1L,
                 to_stage = "staging", reason = "test"),
    "Expected current stage"
  )
})

test_that("cpt_production_version returns the production row", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")
  suppressWarnings(
    cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "production"))

  prod <- cpt_production_version(f$ctx, "long_stay", "clinical")
  expect_equal(prod$version, 1L)
  expect_equal(prod$stage, "production")
})

test_that("cpt_production_version returns NULL when none exists", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")

  expect_null(cpt_production_version(f$ctx, "long_stay", "clinical"))
})

test_that("cpt_versions_by_stage filters correctly", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 5")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_activate(f$ctx, "long_stay", "clinical", 2L)
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")

  staging_rows <- cpt_versions_by_stage(f$ctx, stage = "staging")
  expect_equal(nrow(staging_rows), 1L)
  expect_equal(staging_rows$version, 1L)

  dev_rows <- cpt_versions_by_stage(f$ctx, stage = "development")
  expect_equal(nrow(dev_rows), 1L)
  expect_equal(dev_rows$version, 2L)

  all_rows <- cpt_versions_by_stage(f$ctx, concept_id = "long_stay")
  expect_equal(nrow(all_rows), 2L)
})

test_that("cpt_evaluate resolves production preferentially", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  # v1: loose definition in production
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 2")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")
  suppressWarnings(
    cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "production"))

  # v2: stricter definition in staging only
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 7")
  cpt_activate(f$ctx, "long_stay", "clinical", 2L)
  cpt_promote(f$ctx, "long_stay", "clinical", 2L, to_stage = "staging")

  # Should use v1 (production) silently
  res <- expect_no_warning(cpt_evaluate(f$ctx, "long_stay", "clinical"))
  v1_count <- sum(DBI::dbGetQuery(f$con,
    "SELECT length_of_stay FROM encounters")$length_of_stay > 2)
  expect_equal(sum(res$long_stay), v1_count)
})

test_that("cpt_evaluate falls back to staging with a warning", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_promote(f$ctx, "long_stay", "clinical", 1L, to_stage = "staging")

  expect_warning(
    cpt_evaluate(f$ctx, "long_stay", "clinical"),
    "staging"
  )
})

test_that("cpt_evaluate falls back to development with a stronger warning", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  # Stage remains 'development'

  expect_warning(
    cpt_evaluate(f$ctx, "long_stay", "clinical"),
    "development"
  )
})
