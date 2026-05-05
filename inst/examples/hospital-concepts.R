## Hospital concepts example
## Demonstrates defining ready_for_discharge under clinical and flow scopes

library(conceptR)
library(duckdb)

con <- DBI::dbConnect(duckdb(), ":memory:")

bundle <- structure(
  list(
    object_types = list(
      list(id = "Encounter", table_name = "encounters", primary_key = "encounter_id")
    )
  ),
  class = "ontology_bundle"
)

DBI::dbExecute(con, "
  CREATE TABLE encounters (
    encounter_id      TEXT PRIMARY KEY,
    length_of_stay    INTEGER,
    vital_signs_stable INTEGER,
    mobility_score    INTEGER,
    bed_available     INTEGER
  )
")

for (i in 1:20) {
  DBI::dbExecute(con,
    "INSERT INTO encounters VALUES (?, ?, ?, ?, ?)",
    params = list(
      paste0("E", i),
      sample(1:14, 1),
      sample(0:1, 1),
      sample(0:5, 1),
      sample(0:1, 1)
    ))
}

ctx <- concept_context(bundle, con)

cpt_define(ctx, "ready_for_discharge", "Encounter",
  description = "Patient is clinically and logistically ready for discharge")

# Clinical scope: based on medical criteria
cpt_add_version(ctx, "ready_for_discharge", "clinical",
  sql_expr        = "vital_signs_stable = 1 AND length_of_stay >= 2 AND mobility_score >= 3",
  status          = "active",
  rationale       = "Standard discharge readiness criteria per clinical protocol",
  source_standard = "NHS-DC-2023")

# Flow scope: operational criteria (bed availability matters)
cpt_add_version(ctx, "ready_for_discharge", "flow",
  sql_expr = "vital_signs_stable = 1 AND bed_available = 1",
  status   = "active",
  rationale = "Operational readiness includes bed management constraints")

clinical_results <- cpt_evaluate(ctx, "ready_for_discharge", "clinical")
flow_results     <- cpt_evaluate(ctx, "ready_for_discharge", "flow")

cat("Clinical scope — ready:\n")
print(clinical_results[clinical_results$ready_for_discharge == TRUE, ])

cat("\nFlow scope — ready:\n")
print(flow_results[flow_results$ready_for_discharge == TRUE, ])

DBI::dbDisconnect(con, shutdown = TRUE)
