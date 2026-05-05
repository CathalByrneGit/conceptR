# Shared test fixture: ConceptContext with an Encounter object type.
# encounters table has 10 rows:
#   E1-E5:  length_of_stay 4,5,6,7,8  (all > 3)
#   E6-E8:  length_of_stay 1,2,3       (all <= 3)
#   E9-E10: length_of_stay 4,5         (both > 3)
# 7 rows satisfy length_of_stay > 3; 3 rows satisfy length_of_stay <= 3.

make_concept_ctx <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  bundle <- structure(
    list(
      object_types = list(
        list(
          id          = "Encounter",
          table_name  = "encounters",
          primary_key = "encounter_id"
        )
      )
    ),
    class = "ontology_bundle"
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
