# Shared test fixture: ConceptContext with an Encounter object type.
# encounters table has 10 rows:
#   E1-E5:  length_of_stay 4,5,6,7,8  (all > 3)
#   E6-E8:  length_of_stay 1,2,3       (all <= 3)
#   E9-E10: length_of_stay 4,5         (both > 3)
# 7 rows satisfy length_of_stay > 3; 3 rows satisfy length_of_stay <= 3.

make_concept_ctx <- function() {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  bundle <- ontologySpecR::bundle(
    bundle_id      = "test",
    bundle_version = "0.1.0",
    objects        = list(
      ontologySpecR::object_type(
        id           = "Encounter",
        properties   = list(
          list(id = "encounter_id",   type = "text"),
          list(id = "length_of_stay", type = "integer"),
          list(id = "diagnosis_code", type = "text")
        ),
        primary_key  = "encounter_id",
        source_kind  = "table",
        source_table = "encounters"
      )
    )
  )

  DBI::dbExecute(con, "
    CREATE TABLE encounters (
      encounter_id    TEXT PRIMARY KEY,
      length_of_stay  INTEGER,
      diagnosis_code  TEXT
    )
  ")

  for (i in 1:10) {
    los <- if (i <= 5) i + 3L else i - 5L
    DBI::dbExecute(con,
      "INSERT INTO encounters VALUES (?, ?, ?)",
      params = list(paste0("E", i), los, paste0("D", i)))
  }

  ctx <- concept_context(bundle, con)
  list(ctx = ctx, con = con)
}

teardown_ctx <- function(fixture) {
  DBI::dbDisconnect(fixture$con, shutdown = TRUE)
}
