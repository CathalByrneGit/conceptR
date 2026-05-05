test_that("cpt_evaluate_score computes weighted_sum correctly", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay",  "Encounter")
  cpt_add_version(f$ctx, "long_stay",  "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay",  "clinical", 1L)

  cpt_define(f$ctx, "short_stay", "Encounter")
  cpt_add_version(f$ctx, "short_stay", "clinical", "length_of_stay <= 2")
  cpt_activate(f$ctx, "short_stay", "clinical", 1L)

  cpt_define_score(f$ctx, "complexity", "Encounter",
    components = list(
      list(concept_id = "long_stay",  scope = "clinical", version = 1L, weight = 2.0),
      list(concept_id = "short_stay", scope = "clinical", version = 1L, weight = 1.0)
    ),
    aggregation = "weighted_sum"
  )

  res <- cpt_evaluate_score(f$ctx, "complexity")
  expect_true("score" %in% names(res))
  expect_equal(nrow(res), 10L)

  # For rows where long_stay=TRUE, short_stay=FALSE: score = 2*1 + 1*0 = 2
  # For rows where long_stay=FALSE, short_stay=TRUE: score = 2*0 + 1*1 = 1
  for (i in seq_len(nrow(res))) {
    expected <- 2.0 * res$long_stay[i] + 1.0 * res$short_stay[i]
    expect_equal(res$score[i], expected)
  }
})

test_that("tier assignment works correctly", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  cpt_define_score(f$ctx, "risk_score", "Encounter",
    components = list(
      list(concept_id = "long_stay", scope = "clinical", version = 1L, weight = 1.0)
    ),
    aggregation = "weighted_sum",
    thresholds  = list(low = 0.5, high = 1.5)
  )

  res <- cpt_evaluate_score(f$ctx, "risk_score")
  expect_true("tier" %in% names(res))
  expect_true(all(res$tier %in% c("below_low", "low", "high")))
})

test_that("cpt_add_score_component and cpt_remove_score_component work", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay",  "Encounter")
  cpt_add_version(f$ctx, "long_stay",  "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay",  "clinical", 1L)

  cpt_define(f$ctx, "short_stay", "Encounter")
  cpt_add_version(f$ctx, "short_stay", "clinical", "length_of_stay <= 2")
  cpt_activate(f$ctx, "short_stay", "clinical", 1L)

  cpt_define_score(f$ctx, "s1", "Encounter",
    components  = list(list(concept_id = "long_stay", scope = "clinical", version = 1L, weight = 1.0)),
    aggregation = "weighted_sum")

  res1 <- cpt_evaluate_score(f$ctx, "s1")
  expect_false("short_stay" %in% names(res1))

  cpt_add_score_component(f$ctx, "s1", "short_stay", "clinical", version = 1L, weight = 1.0)
  res2 <- cpt_evaluate_score(f$ctx, "s1")
  expect_true("short_stay" %in% names(res2))

  cpt_remove_score_component(f$ctx, "s1", "short_stay", "clinical")
  res3 <- cpt_evaluate_score(f$ctx, "s1")
  expect_false("short_stay" %in% names(res3))
})
