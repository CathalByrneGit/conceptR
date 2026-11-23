# conceptR 0.2.1

* Object types are now read through ontologySpecR (>= 0.2.2): conceptR's own
  `.ot_table()`, `.ot_pk()` and `.resolve_object_type()` are gone. Table
  references come from `ontologySpecR::object_table_id()` via one internal
  `.table_ref()`, so schema-qualified types (`source$schema`) work on any DBI
  backend.
* Query-backed object types (`source$kind = "query"`) are not supported yet and
  abort with a clear error pointing at `object_source_sql()`.

# conceptR 0.2.0

## Breaking changes

* `concept_context()` gains an `actor` parameter (default: `Sys.info()[["user"]]`).
  Existing calls without an actor continue to work.
* `cpt_promote()` gains a `force` parameter. Production promotion now runs the
  full promotion gate (RBAC + drift check) instead of issuing a warning.

## New features

### Real ontologySpecR bundle accessors (Task A)

* `.resolve_object_type()` now reads `bundle$objects` (not `bundle$object_types`).
* Object-type table and primary-key are accessed via the ontologySpecR field paths
  `ot$source$table` and `ot$primaryKey$properties`.
* `ontologySpecR` moved from Suggests to Imports.

### Absorbed auditR (Task B)

All `adt_*` functions from `auditR` are now part of `conceptR` — no separate
`auditR` package is required.

New exports: `audit_context`, `adt_sample`, `adt_pending_reviews`,
`adt_record_judgment`, `adt_record_judgments_batch`, `adt_summary`,
`adt_rolling_disagreement`, `adt_check_drift`, `adt_detect_drift`,
`adt_resolve_drift`, `adt_drift_status`, `adt_log`, `adt_get_log`,
`adt_governance_report`, `adt_check_policy`, `adt_block_on_drift`,
`adt_activate`, `adt_deprecate`, `adt_grant_role`, `adt_revoke_role`,
`adt_get_roles`, `adt_define_policy`, `adt_approval_gate`.

* Audit schema tables (`audit_samples`, `audit_judgments`, `drift_events`,
  `governance_log`, `rbac_principals`) are created automatically by
  `concept_context()` alongside the concept tables.
* `audit_context(concept_ctx, actor)` is retained as a compatibility shim that
  returns the same `ConceptContext` with a different actor.
* The promotion gate (`.check_promotion()`) blocks production promotion when an
  open drift event exists or the actor lacks the `ontology_admin`/`concept_owner`
  role. Use `force = TRUE` to override with a warning and governance log entry.

## Internal changes

* Added `.ot_table()` and `.ot_pk()` helpers to encapsulate ontologySpecR field access.
* Added `.resolve_version_int()` for audit functions that need an integer version.
* Removed `objectSetsR`, `auditR`, `objectExploreR`, and `glue` from DESCRIPTION.
* Added `Remotes` for `ontologySpecR` and `actionTypesR`.
