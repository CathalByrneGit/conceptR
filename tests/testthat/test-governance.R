test_that("governance log is appended correctly", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  adt_log(ctx, "activate",  concept_id = "rfd", scope = "flow", version = 1L)
  adt_log(ctx, "deprecate", concept_id = "rfd", scope = "flow", version = 1L,
          notes = "superseded by v2")

  entries <- adt_get_log(ctx, concept_id = "rfd")
  expect_equal(nrow(entries), 2L)
  expect_true("activate"  %in% entries$action)
  expect_true("deprecate" %in% entries$action)
})

test_that("adt_get_log filters by action type", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  adt_log(ctx, "activate",  concept_id = "rfd", scope = "flow")
  adt_log(ctx, "deprecate", concept_id = "rfd", scope = "flow")
  adt_log(ctx, "activate",  concept_id = "rfd", scope = "flow")

  acts <- adt_get_log(ctx, concept_id = "rfd", action = "activate")
  expect_equal(nrow(acts), 2L)
})

test_that("adt_get_log filters by actor", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  adt_log(ctx, "activate", concept_id = "rfd", scope = "flow")

  ctx_bob <- audit_context(ctx, actor = "bob")
  adt_log(ctx_bob, "view_audit", concept_id = "rfd", scope = "flow")

  alice_log <- adt_get_log(ctx, actor = "alice")
  expect_true(all(alice_log$actor == "alice"))
})

test_that("adt_governance_report returns one row per concept-scope", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd",       "flow",  n_agree = 8L, n_disagree = 2L)
  insert_judgments(ctx, "other_cpt", "scope2", n_agree = 3L, n_disagree = 1L)
  adt_log(ctx, "activate", concept_id = "rfd",       scope = "flow")
  adt_log(ctx, "activate", concept_id = "other_cpt", scope = "scope2")

  report <- adt_governance_report(ctx)
  expect_equal(nrow(report), 2L)
  expect_true(all(c("concept_id", "scope", "n_judged_total",
                     "current_disagreement_rate", "drift_status",
                     "last_governance_action") %in% names(report)))
})

test_that("adt_governance_report returns empty frame when no judgments exist", {
  skip_if_not_installed("duckdb")
  ctx    <- make_test_ctx()
  report <- adt_governance_report(ctx)
  expect_equal(nrow(report), 0L)
})
