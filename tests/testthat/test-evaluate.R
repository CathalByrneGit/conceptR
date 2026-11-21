test_that("cpt_evaluate returns correct TRUE/FALSE per row", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  res <- cpt_evaluate(f$ctx, "long_stay", "clinical")
  expect_true("encounter_id" %in% names(res))
  expect_true("long_stay" %in% names(res))
  expect_equal(nrow(res), 10L)

  los <- DBI::dbGetQuery(f$con, "SELECT encounter_id, length_of_stay FROM encounters")
  for (i in seq_len(nrow(res))) {
    eid <- res$encounter_id[i]
    expected <- los$length_of_stay[los$encounter_id == eid] > 3
    expect_equal(res$long_stay[i], expected)
  }
})

test_that("cpt_evaluate aborts on deprecated version without explicit version", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)
  cpt_deprecate(f$ctx, "long_stay", "clinical", 1L)

  expect_error(cpt_evaluate(f$ctx, "long_stay", "clinical"), class = "rlang_error")
})

test_that("cpt_evaluate_multi returns one entry per concept", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  cpt_define(f$ctx, "short_stay", "Encounter")
  cpt_add_version(f$ctx, "short_stay", "clinical", "length_of_stay <= 2")
  cpt_activate(f$ctx, "short_stay", "clinical", 1L)

  res <- cpt_evaluate_multi(f$ctx, c("long_stay", "short_stay"), scope = "clinical")
  expect_equal(length(res), 2L)
  expect_named(res, c("long_stay", "short_stay"))
  expect_true("long_stay"  %in% names(res$long_stay))
  expect_true("short_stay" %in% names(res$short_stay))
})

test_that("cpt_compare_versions diff is correct", {
  f <- make_concept_ctx()
  on.exit(teardown_ctx(f))

  cpt_define(f$ctx, "long_stay", "Encounter")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 3")
  cpt_add_version(f$ctx, "long_stay", "clinical", "length_of_stay > 5")
  cpt_activate(f$ctx, "long_stay", "clinical", 1L)

  diff <- cpt_compare_versions(f$ctx, "long_stay", "clinical", 1L, 2L)
  expect_true(diff$total_a >= diff$total_b)  # stricter threshold = fewer hits
  expect_true(diff$only_a >= 0L)
  expect_true(diff$only_b >= 0L)
  expect_true(diff$match  >= 0L)
})

test_that("object types with source$schema are queried schema-qualified", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE SCHEMA flights")
  DBI::dbExecute(con, "CREATE TABLE flights.airports (airport_id TEXT PRIMARY KEY, elevation INTEGER)")
  DBI::dbExecute(con, "INSERT INTO flights.airports VALUES ('A', 10), ('B', 2000)")

  ot <- ontologySpecR::object_type(
    id = "Airport",
    properties = list(list(id = "airport_id", type = "text"),
                      list(id = "elevation", type = "integer")),
    primary_key = "airport_id", source_kind = "table", source_table = "airports")
  ot$source$schema <- "flights"
  bundle <- ontologySpecR::bundle(bundle_id = "t", bundle_version = "0.1.0",
                                  objects = list(ot))

  ctx <- concept_context(bundle, con)
  cpt_define(ctx, "high", "Airport")
  cpt_add_version(ctx, "high", "geo", "elevation > 1000", status = "active")
  res <- cpt_evaluate(ctx, "high", "geo")
  expect_equal(res$high[res$airport_id == "B"], TRUE)
  expect_equal(res$high[res$airport_id == "A"], FALSE)
})
