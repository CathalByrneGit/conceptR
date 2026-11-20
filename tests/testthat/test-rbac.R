test_that("grant, revoke, get_roles round-trip", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  adt_grant_role(ctx, "carol", "auditor")
  expect_equal(adt_get_roles(ctx, "carol"), "auditor")

  adt_revoke_role(ctx, "carol", "auditor")
  expect_equal(adt_get_roles(ctx, "carol"), character(0))
})

test_that("granting duplicate role is idempotent", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  adt_grant_role(ctx, "dave", "ontology_admin")
  adt_grant_role(ctx, "dave", "ontology_admin")

  roles <- adt_get_roles(ctx, "dave")
  expect_equal(length(roles), 1L)
  expect_equal(roles, "ontology_admin")
})

test_that("get_roles returns character(0) for unknown principal", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  expect_equal(adt_get_roles(ctx, "nobody"), character(0))
})

test_that("multiple roles can be granted to one principal", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  adt_grant_role(ctx, "eve", "auditor")
  adt_grant_role(ctx, "eve", "concept_owner")

  roles <- adt_get_roles(ctx, "eve")
  expect_setequal(roles, c("auditor", "concept_owner"))
})

test_that("revoking a role the principal does not hold is a no-op", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  adt_grant_role(ctx, "frank", "auditor")
  expect_silent(adt_revoke_role(ctx, "frank", "ontology_admin"))

  expect_equal(adt_get_roles(ctx, "frank"), "auditor")
})

test_that("adt_define_policy overrides default policy", {
  skip_if_not_installed("duckdb")
  ctx <- make_test_ctx()

  ctx2 <- adt_define_policy(ctx, "activate", c("super_admin"))
  expect_equal(ctx2$policies[["activate"]], "super_admin")
  # Original context unchanged
  expect_true("ontology_admin" %in% ctx$policies[["activate"]])
})
