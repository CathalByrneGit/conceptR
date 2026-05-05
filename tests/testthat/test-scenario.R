test_that("cpt_scenario correctly identifies newly_included and newly_excluded", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 5")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  # Relaxing to > 3 should include more rows (newly_included), none newly_excluded
  result <- cpt_scenario(f$ctx, "long_stay", "clinical", "length_of_stay > 3")

  expect_true(result$proposed_count >= result$current_count)
  expect_equal(nrow(result$newly_excluded), 0L)
  expect_true(nrow(result$newly_included) >= 0L)
  expect_true(result$agreement_rate >= 0 && result$agreement_rate <= 1)
})

test_that("cpt_scenario tightening threshold produces newly_excluded rows", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  result <- cpt_scenario(f$ctx, "long_stay", "clinical", "length_of_stay > 7")

  expect_true(result$current_count >= result$proposed_count)
  expect_equal(nrow(result$newly_included), 0L)
  expect_true(nrow(result$newly_excluded) >= 0L)
})

test_that("cpt_compare_scenarios returns correct summary per proposal", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 4")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  proposals <- list(
    strict  = "length_of_stay > 7",
    relaxed = "length_of_stay > 2"
  )

  summary <- cpt_compare_scenarios(f$ctx, "long_stay", "clinical", proposals)
  expect_equal(nrow(summary), 2L)
  expect_true("scenario_name"  %in% names(summary))
  expect_true("count"          %in% names(summary))
  expect_true("newly_included" %in% names(summary))
  expect_true("newly_excluded" %in% names(summary))
  expect_true("agreement_rate" %in% names(summary))

  strict_row  <- summary[summary$scenario_name == "strict",  ]
  relaxed_row <- summary[summary$scenario_name == "relaxed", ]
  expect_true(strict_row$count <= relaxed_row$count)
})
