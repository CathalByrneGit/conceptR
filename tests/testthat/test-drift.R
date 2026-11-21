test_that("below threshold: no drift event created", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  # Only 3 judgments - below minimum count of 5
  insert_judgments(ctx, "rfd", "flow", n_agree = 2L, n_disagree = 1L)

  result <- adt_detect_drift(ctx, "rfd", "flow", version = 1L,
                             threshold = 0.15, window_days = 30L)
  expect_null(result)
})

test_that("above threshold: drift event is created", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  # 10 judgments: 8 disagree = 0.8 rate
  insert_judgments(ctx, "rfd", "flow", n_agree = 2L, n_disagree = 8L)

  result <- adt_detect_drift(ctx, "rfd", "flow", version = 1L,
                             threshold = 0.15, window_days = 30L)

  expect_false(is.null(result))
  expect_equal(nrow(result), 1L)
  expect_equal(result$status[1L], "open")
  expect_gt(result$disagreement_rate[1L], 0.15)
})

test_that("second call with existing open event returns existing event", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 2L, n_disagree = 8L)

  r1 <- adt_detect_drift(ctx, "rfd", "flow", version = 1L, threshold = 0.15)
  r2 <- adt_detect_drift(ctx, "rfd", "flow", version = 1L, threshold = 0.15)

  expect_equal(r1$drift_id[1L], r2$drift_id[1L])
})

test_that("resolve_drift closes the event", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 1L, n_disagree = 9L)

  event <- adt_detect_drift(ctx, "rfd", "flow", version = 1L, threshold = 0.15)
  drift_id <- event$drift_id[1L]

  adt_resolve_drift(ctx, drift_id, resolution_notes = "definition updated")

  updated <- DBI::dbGetQuery(ctx$connection,
    "SELECT status FROM drift_events WHERE drift_id = ?",
    params = list(drift_id))
  expect_equal(updated$status[1L], "resolved")
})

test_that("adt_drift_status shows only open events", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 1L, n_disagree = 9L)
  event    <- adt_detect_drift(ctx, "rfd", "flow", version = 1L, threshold = 0.15)
  drift_id <- event$drift_id[1L]

  open <- adt_drift_status(ctx)
  expect_equal(nrow(open), 1L)

  adt_resolve_drift(ctx, drift_id)

  open_after <- adt_drift_status(ctx)
  expect_equal(nrow(open_after), 0L)
})

test_that("adt_check_drift returns FALSE when n_judged < 5", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 0L, n_disagree = 4L)
  expect_false(adt_check_drift(ctx, "rfd", "flow"))
})
