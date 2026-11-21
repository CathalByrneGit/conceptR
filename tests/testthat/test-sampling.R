test_that("adt_sample returns exactly n rows when enough instances exist", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  set.seed(42)
  for (i in 1:10) {
    DBI::dbExecute(ctx$connection,
      "INSERT INTO audit_samples
         (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
       VALUES (?, ?, ?, ?, ?, ?, ?, FALSE)",
      params = list(paste0("s", i), "ready_for_discharge", "flow", 1L,
                    paste0("enc_", sprintf("%03d", i)), i <= 7L, now)
    )
  }

  pending <- adt_pending_reviews(ctx, "ready_for_discharge", "flow")
  expect_equal(nrow(pending), 10L)
})

test_that("stratified sample returns both TRUE and FALSE system values", {
  skip_if_not_installed("duckdb")

  eval_df <- data.frame(
    object_key   = paste0("enc_", 1:50),
    system_value = c(rep(TRUE, 30), rep(FALSE, 20)),
    stringsAsFactors = FALSE
  )

  set.seed(1)
  result <- conceptR:::.stratified_sample(eval_df, 10L)

  expect_true(any(result$system_value == TRUE))
  expect_true(any(result$system_value == FALSE))
  expect_lte(nrow(result), 10L)
})

test_that("adt_pending_reviews filters by concept_id and scope", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(ctx$connection,
    "INSERT INTO audit_samples (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
     VALUES ('s1', 'concept_a', 'scope1', 1, 'k1', TRUE, ?, FALSE)",
    params = list(now))
  DBI::dbExecute(ctx$connection,
    "INSERT INTO audit_samples (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
     VALUES ('s2', 'concept_b', 'scope2', 1, 'k2', FALSE, ?, FALSE)",
    params = list(now))

  result <- adt_pending_reviews(ctx, concept_id = "concept_a")
  expect_equal(nrow(result), 1L)
  expect_equal(result$sample_id, "s1")
})

test_that("adt_pending_reviews returns empty frame when all reviewed", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(ctx$connection,
    "INSERT INTO audit_samples (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
     VALUES ('s1', 'rfd', 'flow', 1, 'k1', TRUE, ?, TRUE)",
    params = list(now))

  result <- adt_pending_reviews(ctx, "rfd", "flow")
  expect_equal(nrow(result), 0L)
})
