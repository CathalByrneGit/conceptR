# Helper: build a minimal ctx with a concept at staging stage.
make_staging_ctx <- function() {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  bundle <- ontologySpecR::bundle(
    bundle_id = "test", bundle_version = "0.1.0",
    objects = list(
      ontologySpecR::object_type(
        id           = "Encounter",
        properties   = list(list(id = "encounter_id"), list(id = "value")),
        primary_key  = "encounter_id",
        source_kind  = "table",
        source_table = "encounters"
      )
    )
  )

  DBI::dbExecute(con, "CREATE TABLE encounters (encounter_id TEXT PRIMARY KEY, value BOOLEAN)")
  DBI::dbWriteTable(con, "encounters",
    rbind(
      data.frame(encounter_id = paste0("enc_", 1:10), value = TRUE,  stringsAsFactors = FALSE),
      data.frame(encounter_id = paste0("enc_", 11:15), value = FALSE, stringsAsFactors = FALSE)
    ),
    append = TRUE
  )

  ctx <- concept_context(bundle, con, actor = "alice")
  adt_grant_role(ctx, "alice", "ontology_admin")

  cpt_define(ctx, "rfd", "Encounter")
  cpt_add_version(ctx, "rfd", "flow", "value = TRUE", status = "active")
  cpt_promote(ctx, "rfd", "flow", 1L, to_stage = "staging")

  list(ctx = ctx, con = con)
}

teardown_staging <- function(f) DBI::dbDisconnect(f$con, shutdown = TRUE)

# --- Production promotion blocked by open drift ---

test_that("production promotion is blocked when drift is open", {
  skip_if_not_installed("duckdb")
  f <- make_staging_ctx()
  on.exit(teardown_staging(f))

  insert_judgments(f$ctx, "rfd", "flow", n_agree = 1L, n_disagree = 9L)
  adt_detect_drift(f$ctx, "rfd", "flow", version = 1L, threshold = 0.15)

  expect_error(
    cpt_promote(f$ctx, "rfd", "flow", 1L, to_stage = "production"),
    regexp = "drift"
  )
  # Version should still be in staging
  expect_equal(cpt_production_version(f$ctx, "rfd", "flow"), NULL)
})

# --- Production promotion succeeds when no drift ---

test_that("production promotion succeeds for admin with no drift", {
  skip_if_not_installed("duckdb")
  f <- make_staging_ctx()
  on.exit(teardown_staging(f))

  expect_no_error(cpt_promote(f$ctx, "rfd", "flow", 1L, to_stage = "production"))

  pv <- cpt_production_version(f$ctx, "rfd", "flow")
  expect_equal(pv$version, 1L)
})

# --- Production promotion with force = TRUE logs and proceeds despite drift ---

test_that("production promotion with force = TRUE warns and proceeds despite drift", {
  skip_if_not_installed("duckdb")
  f <- make_staging_ctx()
  on.exit(teardown_staging(f))

  insert_judgments(f$ctx, "rfd", "flow", n_agree = 1L, n_disagree = 9L)
  adt_detect_drift(f$ctx, "rfd", "flow", version = 1L, threshold = 0.15)

  expect_warning(
    cpt_promote(f$ctx, "rfd", "flow", 1L, to_stage = "production", force = TRUE)
  )

  pv <- cpt_production_version(f$ctx, "rfd", "flow")
  expect_equal(pv$version, 1L)

  # forced_promote event should appear in governance log
  log <- adt_get_log(f$ctx, concept_id = "rfd")
  expect_true("forced_promote" %in% log$action)
})

# --- Actor without required role is blocked ---

test_that("production promotion is blocked for actor without ontology_admin role", {
  skip_if_not_installed("duckdb")
  f <- make_staging_ctx()
  on.exit(teardown_staging(f))

  ctx_bob <- audit_context(f$ctx, actor = "bob")
  # bob has no roles granted in this context

  expect_error(
    cpt_promote(ctx_bob, "rfd", "flow", 1L, to_stage = "production"),
    regexp = "not permitted"
  )
})
