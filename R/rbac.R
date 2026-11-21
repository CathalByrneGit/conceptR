#' Grant a role to a principal
#'
#' Upserts into `rbac_principals`. Adding a role the principal already holds is
#' idempotent.
#'
#' @param ctx A `ConceptContext`.
#' @param principal_id Character.
#' @param role Character.
#'
#' @return `NULL` invisibly.
#'
#' @export
adt_grant_role <- function(ctx, principal_id, role) {
  .assert_ctx(ctx)
  assert_that(is.character(principal_id) && length(principal_id) == 1L && nzchar(principal_id),
              "principal_id must be a non-empty character scalar")
  assert_that(is.character(role) && length(role) == 1L && nzchar(role),
              "role must be a non-empty character scalar")

  existing <- DBI::dbGetQuery(ctx$connection,
    "SELECT roles_json FROM rbac_principals WHERE principal_id = ?",
    params = list(principal_id))

  if (nrow(existing) == 0L) {
    DBI::dbExecute(ctx$connection,
      "INSERT INTO rbac_principals (principal_id, roles_json) VALUES (?, ?)",
      params = list(principal_id, serialise_roles(role)))
  } else {
    current <- parse_roles(existing$roles_json[1L])
    DBI::dbExecute(ctx$connection,
      "UPDATE rbac_principals SET roles_json = ? WHERE principal_id = ?",
      params = list(serialise_roles(c(current, role)), principal_id))
  }

  invisible(NULL)
}

#' Revoke a role from a principal
#'
#' @param ctx A `ConceptContext`.
#' @param principal_id Character.
#' @param role Character.
#'
#' @return `NULL` invisibly.
#'
#' @export
adt_revoke_role <- function(ctx, principal_id, role) {
  .assert_ctx(ctx)

  existing <- DBI::dbGetQuery(ctx$connection,
    "SELECT roles_json FROM rbac_principals WHERE principal_id = ?",
    params = list(principal_id))

  if (nrow(existing) == 0L) return(invisible(NULL))

  current <- parse_roles(existing$roles_json[1L])
  DBI::dbExecute(ctx$connection,
    "UPDATE rbac_principals SET roles_json = ? WHERE principal_id = ?",
    params = list(serialise_roles(setdiff(current, role)), principal_id))

  invisible(NULL)
}

#' Get the roles assigned to a principal
#'
#' @param ctx A `ConceptContext`.
#' @param principal_id Character.
#'
#' @return A character vector of role names, or `character(0)`.
#'
#' @export
adt_get_roles <- function(ctx, principal_id) {
  .assert_ctx(ctx)

  result <- DBI::dbGetQuery(ctx$connection,
    "SELECT roles_json FROM rbac_principals WHERE principal_id = ?",
    params = list(principal_id))

  if (nrow(result) == 0L) return(character(0))
  parse_roles(result$roles_json[1L])
}

#' Define or override a policy for an action
#'
#' Policies are stored in-memory on the context and are not persisted to the
#' database.
#'
#' @param ctx A `ConceptContext`.
#' @param action Character.
#' @param required_roles Character vector. Any of these roles permits the actor.
#'
#' @return The updated `ConceptContext` with the new policy applied.
#'
#' @export
adt_define_policy <- function(ctx, action, required_roles) {
  .assert_ctx(ctx)
  assert_that(is.character(action) && length(action) == 1L && nzchar(action),
              "action must be a non-empty character scalar")
  assert_that(is.character(required_roles) && length(required_roles) >= 1L,
              "required_roles must be a non-empty character vector")

  ctx$policies[[action]] <- required_roles
  ctx
}
