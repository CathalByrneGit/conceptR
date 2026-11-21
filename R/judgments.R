#' Record a reviewer judgment for an audit sample
#'
#' Looks up the sample in `audit_samples`, inserts a row into `audit_judgments`
#' with `ctx$actor` as the reviewer, and marks the sample as reviewed.
#'
#' @param ctx A `ConceptContext`.
#' @param sample_id Character. The sample to judge.
#' @param reviewer_value Logical. The reviewer's verdict for the sampled instance.
#' @param notes Character or `NULL`. Optional free-text notes.
#'
#' @return The `judgment_id` (invisibly).
#'
#' @export
adt_record_judgment <- function(ctx, sample_id, reviewer_value, notes = NULL) {
  .assert_ctx(ctx)
  assert_that(is.character(sample_id) && length(sample_id) == 1L,
              "sample_id must be a character scalar")
  reviewer_value <- as.logical(reviewer_value)
  assert_that(!is.na(reviewer_value), "reviewer_value must be TRUE or FALSE")

  sample <- DBI::dbGetQuery(
    ctx$connection,
    "SELECT * FROM audit_samples WHERE sample_id = ?",
    params = list(sample_id)
  )

  if (nrow(sample) == 0L)
    cli::cli_abort("Sample {sample_id} not found in audit_samples.")

  if (isTRUE(sample$reviewed[1L]))
    cli::cli_abort(
      "Sample {sample_id} has already been reviewed. Each sample may only be judged once.")

  judgment_id <- new_id()
  now         <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(
    ctx$connection,
    "INSERT INTO audit_judgments
       (judgment_id, sample_id, concept_id, scope, version, object_key,
        system_value, reviewer_value, reviewer_id, reviewed_at, notes)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      judgment_id,
      sample_id,
      sample$concept_id[1L],
      sample$scope[1L],
      sample$version[1L],
      sample$object_key[1L],
      as.integer(sample$system_value[1L]),
      as.integer(reviewer_value),
      ctx$actor,
      now,
      notes
    )
  )

  DBI::dbExecute(
    ctx$connection,
    "UPDATE audit_samples SET reviewed = TRUE WHERE sample_id = ?",
    params = list(sample_id)
  )

  invisible(judgment_id)
}

#' Record multiple reviewer judgments in a batch
#'
#' @param ctx A `ConceptContext`.
#' @param judgments A data frame with columns `sample_id` (character),
#'   `reviewer_value` (logical), and optionally `notes` (character).
#'
#' @return A data frame with columns: `sample_id`, `judgment_id`, `status`
#'   (`"ok"` or an error message string).
#'
#' @export
adt_record_judgments_batch <- function(ctx, judgments) {
  .assert_ctx(ctx)
  assert_that(
    is.data.frame(judgments) &&
      "sample_id"      %in% names(judgments) &&
      "reviewer_value" %in% names(judgments),
    "judgments must be a data frame with columns sample_id and reviewer_value"
  )

  if (!"notes" %in% names(judgments)) judgments$notes <- NA_character_

  results <- vector("list", nrow(judgments))

  for (i in seq_len(nrow(judgments))) {
    notes_i <- if (is.na(judgments$notes[i])) NULL else judgments$notes[i]
    result  <- tryCatch({
      jid <- adt_record_judgment(ctx, judgments$sample_id[i],
                                 judgments$reviewer_value[i], notes = notes_i)
      list(sample_id = judgments$sample_id[i], judgment_id = jid, status = "ok")
    }, error = function(e) {
      list(sample_id   = judgments$sample_id[i],
           judgment_id = NA_character_,
           status      = conditionMessage(e))
    })
    results[[i]] <- result
  }

  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
}
