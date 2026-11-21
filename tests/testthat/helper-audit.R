# Shared audit test fixture helpers.
# Creates an in-memory DuckDB ConceptContext pre-populated with:
#   - 50 encounter rows (30 TRUE, 20 FALSE on concept "ready_for_discharge")
#   - Two principals: alice (ontology_admin), bob (auditor)

make_test_ctx <- function() {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbExecute(con, "CREATE TABLE encounters (encounter_id TEXT PRIMARY KEY, value BOOLEAN)")
  DBI::dbWriteTable(con, "encounters",
    rbind(
      data.frame(encounter_id = paste0("enc_", sprintf("%03d", 1:30)),  value = TRUE,  stringsAsFactors = FALSE),
      data.frame(encounter_id = paste0("enc_", sprintf("%03d", 31:50)), value = FALSE, stringsAsFactors = FALSE)
    ),
    append = TRUE
  )

  bundle <- ontologySpecR::bundle(
    bundle_id = "test", bundle_version = "0.1.0",
    objects = list(
      ontologySpecR::object_type(
        id = "Encounter",
        properties = list(list(id = "encounter_id"), list(id = "value")),
        primary_key  = "encounter_id",
        source_kind  = "table",
        source_table = "encounters"
      )
    )
  )

  ctx <- concept_context(bundle, con, actor = "alice")
  adt_grant_role(ctx, "alice", "ontology_admin")
  adt_grant_role(ctx, "bob",   "auditor")
  ctx
}

# Insert pre-fabricated judgments so tests don't need cpt_evaluate().
insert_judgments <- function(ctx, concept_id, scope, version = 1L,
                             n_agree = 5L, n_disagree = 5L) {
  for (i in seq_len(n_agree)) {
    sid <- paste0("sample_agree_", i)
    DBI::dbExecute(ctx$connection,
      "INSERT OR IGNORE INTO audit_samples
         (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
       VALUES (?, ?, ?, ?, ?, TRUE, CURRENT_TIMESTAMP, TRUE)",
      params = list(sid, concept_id, scope, version)
    )
    DBI::dbExecute(ctx$connection,
      "INSERT OR IGNORE INTO audit_judgments
         (judgment_id, sample_id, concept_id, scope, version, object_key,
          system_value, reviewer_value, reviewer_id, reviewed_at)
       VALUES (?, ?, ?, ?, ?, ?, TRUE, TRUE, 'alice', CURRENT_TIMESTAMP)",
      params = list(paste0("j_agree_", i), sid, concept_id, scope, version,
                    paste0("enc_agree_", i))
    )
  }
  for (i in seq_len(n_disagree)) {
    sid <- paste0("sample_disagree_", i)
    DBI::dbExecute(ctx$connection,
      "INSERT OR IGNORE INTO audit_samples
         (sample_id, concept_id, scope, version, object_key, system_value, sampled_at, reviewed)
       VALUES (?, ?, ?, ?, ?, TRUE, CURRENT_TIMESTAMP, TRUE)",
      params = list(sid, concept_id, scope, version)
    )
    DBI::dbExecute(ctx$connection,
      "INSERT OR IGNORE INTO audit_judgments
         (judgment_id, sample_id, concept_id, scope, version, object_key,
          system_value, reviewer_value, reviewer_id, reviewed_at)
       VALUES (?, ?, ?, ?, ?, ?, TRUE, FALSE, 'bob', CURRENT_TIMESTAMP)",
      params = list(paste0("j_disagree_", i), sid, concept_id, scope, version,
                    paste0("enc_disagree_", i))
    )
  }
}
