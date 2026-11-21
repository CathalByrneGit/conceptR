test_that("recording a judgment marks sample as reviewed", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(ctx$connection,
    "INSERT INTO audit_samples
       (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
     VALUES ('s1', 'rfd', 'flow', 1, 'enc_001', TRUE, ?, FALSE)",
    params = list(now)
  )

  jid <- adt_record_judgment(ctx, "s1", reviewer_value = FALSE, notes = "disagree")
  expect_type(jid, "character")
  expect_true(nzchar(jid))

  sample_row <- DBI::dbGetQuery(ctx$connection,
    "SELECT reviewed FROM audit_samples WHERE sample_id = 's1'")
  expect_true(as.logical(sample_row$reviewed[1L]))

  judgment_row <- DBI::dbGetQuery(ctx$connection,
    "SELECT * FROM audit_judgments WHERE sample_id = 's1'")
  expect_equal(nrow(judgment_row), 1L)
  expect_false(as.logical(judgment_row$reviewer_value[1L]))
  expect_equal(judgment_row$reviewer_id[1L], "alice")
})

test_that("recording a judgment twice on the same sample_id aborts", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(ctx$connection,
    "INSERT INTO audit_samples
       (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
     VALUES ('s2', 'rfd', 'flow', 1, 'enc_002', TRUE, ?, FALSE)",
    params = list(now)
  )

  adt_record_judgment(ctx, "s2", reviewer_value = TRUE)
  expect_error(adt_record_judgment(ctx, "s2", reviewer_value = FALSE),
               regexp = "already been reviewed")
})

test_that("batch recording returns correct statuses", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  for (i in 1:3) {
    DBI::dbExecute(ctx$connection,
      "INSERT INTO audit_samples
         (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
       VALUES (?, 'rfd', 'flow', 1, ?, TRUE, ?, FALSE)",
      params = list(paste0("bs", i), paste0("enc_", i), now)
    )
  }

  judgments <- data.frame(
    sample_id      = c("bs1", "bs2", "bs3", "bs_missing"),
    reviewer_value = c(TRUE,  FALSE, TRUE,  TRUE),
    stringsAsFactors = FALSE
  )

  result <- adt_record_judgments_batch(ctx, judgments)
  expect_equal(nrow(result), 4L)
  expect_equal(sum(result$status == "ok"), 3L)
  expect_true(result$status[result$sample_id == "bs_missing"] != "ok")
})

test_that("recording judgment for unknown sample_id aborts", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  expect_error(adt_record_judgment(ctx, "nonexistent_id", TRUE),
               regexp = "not found")
})
