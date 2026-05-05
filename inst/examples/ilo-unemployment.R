## ILO unemployment template example
## Demonstrates parameterised templates with country-specific variants

library(conceptR)
library(duckdb)

con <- DBI::dbConnect(duckdb(), ":memory:")

bundle <- structure(
  list(
    object_types = list(
      list(id = "Person", table_name = "persons", primary_key = "person_id")
    )
  ),
  class = "ontology_bundle"
)

DBI::dbExecute(con, "
  CREATE TABLE persons (
    person_id         TEXT PRIMARY KEY,
    employed          INTEGER,
    available_work    INTEGER,
    seeking_work      INTEGER,
    reference_weeks   INTEGER
  )
")

for (i in 1:30) {
  DBI::dbExecute(con,
    "INSERT INTO persons VALUES (?, ?, ?, ?, ?)",
    params = list(
      paste0("P", i),
      sample(0:1, 1),
      sample(0:1, 1),
      sample(0:1, 1),
      sample(1:4, 1)
    ))
}

ctx <- concept_context(bundle, con)

# ILO definition allows a reference period parameter
cpt_define_template(ctx, "ilo_unemployed", "Person",
  base_sql_expr = paste0(
    "employed = 0 AND available_work = 1 AND ",
    "seeking_work = 1 AND reference_weeks >= {{min_reference_weeks}}"
  ),
  parameters = list(
    min_reference_weeks = list(
      type        = "integer",
      default     = 1,
      description = "Minimum reference period in weeks for seeking-work criterion"
    )
  ),
  source_standard = "ILO-2013",
  description     = "ILO standard unemployment definition with configurable reference period"
)

# Country variants
cpt_inherit(ctx, "unemployed_ireland", "ilo_unemployed", "national",
  parameter_values = list(min_reference_weeks = 1),
  rationale        = "Ireland uses 1-week reference period per CSO methodology",
  status           = "active")

cpt_inherit(ctx, "unemployed_us", "ilo_unemployed", "national",
  parameter_values = list(min_reference_weeks = 4),
  rationale        = "US BLS uses 4-week reference period",
  status           = "active")

variants <- cpt_compare_template_variants(ctx, "ilo_unemployed")
cat("Template variants:\n")
print(variants)

# Preview a hypothetical EU variant without committing
eu_sql <- cpt_render_template(ctx, "ilo_unemployed", list(min_reference_weeks = 2))
cat("\nPreview EU variant SQL:\n", eu_sql, "\n")

DBI::dbDisconnect(con, shutdown = TRUE)
