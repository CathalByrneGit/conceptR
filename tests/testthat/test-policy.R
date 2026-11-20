test_that("alice (admin) can activate when no drift", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  expect_true(adt_check_policy(ctx, "activate", "rfd", "flow", version = 1L))
})

test_that("bob (auditor) cannot activate", {
  skip_if_not_installed("duckdb")
  ctx_alice <- make_test_ctx()
  ctx_bob   <- audit_context(ctx_alice, actor = "bob")

  expect_error(
    adt_check_policy(ctx_bob, "activate", "rfd", "flow", version = 1L),
    regexp = "not permitted"
  )
})

test_that("drift gate blocks activate for admin when drift is open", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 1L, n_disagree = 9L)
  adt_detect_drift(ctx, "rfd", "flow", version = 1L, threshold = 0.15)

  expect_error(
    adt_check_policy(ctx, "activate", "rfd", "flow", version = 1L),
    regexp = "drift"
  )
})

test_that("view_audit is allowed for auditor role", {
  skip_if_not_installed("duckdb")
  ctx_alice <- make_test_ctx()
  ctx_bob   <- audit_context(ctx_alice, actor = "bob")

  expect_true(adt_check_policy(ctx_bob, "view_audit", "rfd", "flow"))
})

test_that("adt_block_on_drift passes when no open drift", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  expect_invisible(adt_block_on_drift(ctx, "rfd", "flow"))
})

test_that("adt_block_on_drift aborts when drift is open", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 1L, n_disagree = 9L)
  adt_detect_drift(ctx, "rfd", "flow", version = 1L)

  expect_error(adt_block_on_drift(ctx, "rfd", "flow"), regexp = "drift")
})
