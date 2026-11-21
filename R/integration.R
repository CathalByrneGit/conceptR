#' Create an approval gate function for use with actionTypesR
#'
#' Returns a `gate_fn` suitable for passing to
#' `actionTypesR::register_approval_gate()`. The gate checks:
#' 1. The actor holds the `"approver"` or `"ontology_admin"` role.
#' 2. If the action specifies `$triggers` concepts, no open drift event exists.
#'
#' Requires the `actionTypesR` package at runtime.
#'
#' @param ctx A `ConceptContext`.
#'
#' @return A function `function(action_request)` returning `"approved"`,
#'   `"denied"`, or `"pending"`.
#'
#' @export
adt_approval_gate <- function(ctx) {
  .assert_ctx(ctx)
  if (!requireNamespace("actionTypesR", quietly = TRUE))
    cli::cli_warn(c(
      "{.pkg actionTypesR} is not installed.",
      "i" = "The returned gate function will work, but cannot be registered with actionTypesR."
    ))

  force(ctx)

  function(action_request) {
    actor_roles   <- adt_get_roles(ctx, ctx$actor)
    allowed_roles <- c("approver", "ontology_admin")

    if (!any(allowed_roles %in% actor_roles)) return("denied")

    triggers <- action_request[["triggers"]]
    if (!is.null(triggers) && length(triggers) > 0L) {
      for (trig in triggers) {
        concept_id <- trig[["concept_id"]]
        scope      <- trig[["scope"]]
        version    <- trig[["version"]]

        if (!is.null(concept_id) && !is.null(scope)) {
          drift_open <- tryCatch({
            sql  <- "SELECT COUNT(*) AS n FROM drift_events
                      WHERE concept_id = ? AND scope = ? AND status = 'open'"
            args <- list(concept_id, scope)
            if (!is.null(version)) {
              sql  <- paste0(sql, " AND version = ?")
              args <- c(args, list(as.integer(version)))
            }
            DBI::dbGetQuery(ctx$connection, sql, params = args)$n[1L]
          }, error = function(e) 0L)

          if (drift_open > 0L) return("pending")
        }
      }
    }

    "approved"
  }
}
