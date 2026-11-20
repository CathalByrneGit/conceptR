#' Check whether the current actor is permitted to perform an action
#'
#' Enforces two gates:
#' 1. **RBAC**: the actor must hold at least one required role.
#' 2. **Drift gate**: if the action is `"activate"` or `"deprecate"` and an open
#'    drift event exists for the concept version, the action is blocked.
#'
#' @param ctx A `ConceptContext`.
#' @param action Character. One of `"activate"`, `"deprecate"`, `"approve_action"`,
#'   `"view_audit"`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer or `NULL`.
#'
#' @return `TRUE` if permitted. Throws via [cli::cli_abort()] otherwise.
#'
#' @export
adt_check_policy <- function(ctx, action, concept_id, scope, version = NULL) {
  .assert_ctx(ctx)

  required_roles <- ctx$policies[[action]]
  if (is.null(required_roles))
    cli::cli_abort("No policy defined for action '{action}'.")

  actor_roles <- adt_get_roles(ctx, ctx$actor)
  has_role    <- any(required_roles %in% actor_roles)
  if (!has_role) {
    cli::cli_abort(c(
      "Actor '{ctx$actor}' is not permitted to perform '{action}'.",
      "i" = "Required roles: {paste(required_roles, collapse = ', ')}",
      "i" = "Actor's roles: {if (length(actor_roles) == 0) '(none)' else paste(actor_roles, collapse = ', ')}"
    ))
  }

  if (action %in% c("activate", "deprecate")) {
    adt_block_on_drift(ctx, concept_id, scope, version)
  }

  TRUE
}

#' Abort if there is an open drift event for a concept version
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer or `NULL`.
#'
#' @return `NULL` invisibly if no open drift. Throws if drift is open.
#'
#' @export
adt_block_on_drift <- function(ctx, concept_id, scope, version = NULL) {
  .assert_ctx(ctx)

  version_int <- if (!is.null(version)) as.integer(version) else NA_integer_

  sql  <- "SELECT drift_id, disagreement_rate FROM drift_events
            WHERE concept_id = ? AND scope = ? AND status = 'open'"
  args <- list(concept_id, scope)

  if (!is.na(version_int)) {
    sql  <- paste0(sql, " AND version = ?")
    args <- c(args, list(version_int))
  }

  open <- DBI::dbGetQuery(ctx$connection, sql, params = args)

  if (nrow(open) > 0L) {
    cli::cli_abort(c(
      "Lifecycle action blocked: open drift event exists for {concept_id}/{scope}.",
      "i" = "Drift ID: {open$drift_id[1L]}",
      "i" = "Disagreement rate: {round(open$disagreement_rate[1L], 3)}",
      "i" = "Resolve the drift event with adt_resolve_drift() before proceeding."
    ))
  }

  invisible(NULL)
}

#' Governed activation of a concept version
#'
#' Checks policy (RBAC + drift gate) then calls [cpt_activate()] and records
#' the action in the governance log.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer.
#'
#' @return Return value of [cpt_activate()] (invisibly).
#'
#' @export
adt_activate <- function(ctx, concept_id, scope, version) {
  .assert_ctx(ctx)
  adt_check_policy(ctx, "activate", concept_id, scope, version)
  result <- cpt_activate(ctx, concept_id, scope, version)
  adt_log(ctx, "activate", concept_id = concept_id, scope = scope, version = version)
  invisible(result)
}

#' Governed deprecation of a concept version
#'
#' Checks policy (RBAC + drift gate) then calls [cpt_deprecate()] and records
#' the action in the governance log.
#'
#' @param ctx A `ConceptContext`.
#' @param concept_id Character.
#' @param scope Character.
#' @param version Integer.
#'
#' @return Return value of [cpt_deprecate()] (invisibly).
#'
#' @export
adt_deprecate <- function(ctx, concept_id, scope, version) {
  .assert_ctx(ctx)
  adt_check_policy(ctx, "deprecate", concept_id, scope, version)
  result <- cpt_deprecate(ctx, concept_id, scope, version)
  adt_log(ctx, "deprecate", concept_id = concept_id, scope = scope, version = version)
  invisible(result)
}

# ---------------------------------------------------------------------------
# Internal promotion gate (called by cpt_promote for production stage)
# ---------------------------------------------------------------------------

# Checks policy (RBAC + drift gate) before promoting to production.
# With force = TRUE, logs a warning and proceeds; without it, aborts on failure.
.check_promotion <- function(ctx, concept_id, scope, version, force = FALSE) {
  err <- tryCatch({
    adt_check_policy(ctx, "activate", concept_id, scope, as.integer(version))
    NULL
  }, error = function(e) e)

  if (!is.null(err)) {
    if (isTRUE(force)) {
      cli::cli_warn(
        "Production promotion check failed (force = TRUE): {conditionMessage(err)}")
      adt_log(ctx, "forced_promote",
              concept_id = concept_id, scope = scope, version = as.integer(version),
              notes = paste("force=TRUE;", conditionMessage(err)))
    } else {
      stop(err)
    }
  }

  invisible(NULL)
}
