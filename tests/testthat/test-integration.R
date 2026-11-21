test_that("adt_approval_gate returns 'approved' for admin with no drift", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  gate   <- adt_approval_gate(ctx)
  result <- gate(list(triggers = NULL))
  expect_equal(result, "approved")
})

test_that("adt_approval_gate returns 'denied' for actor without required role", {
  skip_if_not_installed("duckdb")
  ctx_alice <- make_test_ctx()
  ctx_guest <- audit_context(ctx_alice, actor = "guest_user")

  gate   <- adt_approval_gate(ctx_guest)
  result <- gate(list(triggers = NULL))
  expect_equal(result, "denied")
})

test_that("adt_approval_gate returns 'pending' when triggered concept has open drift", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 1L, n_disagree = 9L)
  adt_detect_drift(ctx, "rfd", "flow", version = 1L)

  gate   <- adt_approval_gate(ctx)
  result <- gate(list(
    triggers = list(list(concept_id = "rfd", scope = "flow", version = 1L))
  ))
  expect_equal(result, "pending")
})

test_that("adt_approval_gate returns 'approved' for approver role", {
  skip_if_not_installed("duckdb")
  ctx_alice    <- make_test_ctx()
  ctx_approver <- audit_context(ctx_alice, actor = "approver_user")
  adt_grant_role(ctx_approver, "approver_user", "approver")

  gate   <- adt_approval_gate(ctx_approver)
  result <- gate(list(triggers = NULL))
  expect_equal(result, "approved")
})

test_that("approval gate with multiple triggers blocks on any open drift", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "concept_a", "scope1", n_agree = 0L, n_disagree = 10L)
  adt_detect_drift(ctx, "concept_a", "scope1", version = 1L)

  gate   <- adt_approval_gate(ctx)
  result <- gate(list(
    triggers = list(
      list(concept_id = "clean_concept", scope = "flow",   version = 1L),
      list(concept_id = "concept_a",     scope = "scope1", version = 1L)
    )
  ))
  expect_equal(result, "pending")
})
