test_that("5 agree + 5 disagree gives rate = 0.5", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  insert_judgments(ctx, "rfd", "flow", n_agree = 5L, n_disagree = 5L)

  smry <- adt_summary(ctx, "rfd", "flow")
  expect_equal(smry$n_judged,          10L)
  expect_equal(smry$n_agree,           5L)
  expect_equal(smry$n_disagree,        5L)
  expect_equal(smry$disagreement_rate, 0.5)
})

test_that("window filter excludes old judgments", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  # Insert 4 old judgments (1 month ago)
  old_time <- format(Sys.time() - as.difftime(40, units = "days"), "%Y-%m-%d %H:%M:%S")
  for (i in 1:4) {
    sid <- paste0("old_s", i)
    DBI::dbExecute(ctx$connection,
      "INSERT INTO audit_samples
         (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
       VALUES (?, 'rfd', 'flow', 1, ?, TRUE, ?, TRUE)",
      params = list(sid, paste0("enc_old_", i), old_time)
    )
    DBI::dbExecute(ctx$connection,
      "INSERT INTO audit_judgments
         (judgment_id, sample_id, concept_id, scope, version, object_key,
          system_value, reviewer_value, reviewer_id, reviewed_at)
       VALUES (?, ?, 'rfd', 'flow', 1, ?, TRUE, FALSE, 'bob', ?)",
      params = list(paste0("old_j", i), sid, paste0("enc_old_", i), old_time)
    )
  }

  # Insert 3 recent agree judgments
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  for (i in 1:3) {
    sid <- paste0("new_s", i)
    DBI::dbExecute(ctx$connection,
      "INSERT INTO audit_samples
         (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
       VALUES (?, 'rfd', 'flow', 1, ?, TRUE, ?, TRUE)",
      params = list(sid, paste0("enc_new_", i), now)
    )
    DBI::dbExecute(ctx$connection,
      "INSERT INTO audit_judgments
         (judgment_id, sample_id, concept_id, scope, version, object_key,
          system_value, reviewer_value, reviewer_id, reviewed_at)
       VALUES (?, ?, 'rfd', 'flow', 1, ?, TRUE, TRUE, 'alice', ?)",
      params = list(paste0("new_j", i), sid, paste0("enc_new_", i), now)
    )
  }

  smry_windowed <- adt_summary(ctx, "rfd", "flow", window_days = 30L)
  expect_equal(smry_windowed$n_judged,   3L)
  expect_equal(smry_windowed$n_disagree, 0L)

  smry_all <- adt_summary(ctx, "rfd", "flow")
  expect_equal(smry_all$n_judged, 7L)
})

test_that("empty judgment set returns NA rate", {
  skip_if_not_installed("duckdb")
  ctx  <- make_test_ctx()
  smry <- adt_summary(ctx, "nobody", "nowhere")
  expect_equal(smry$n_judged, 0L)
  expect_true(is.na(smry$disagreement_rate))
})

test_that("adt_rolling_disagreement returns a data frame", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()
  insert_judgments(ctx, "rfd", "flow", n_agree = 4L, n_disagree = 4L)

  result <- adt_rolling_disagreement(ctx, "rfd", "flow")
  expect_s3_class(result, "data.frame")
  expect_true("window_start" %in% names(result))
  expect_true("disagreement_rate" %in% names(result))
})
