#' Explain how each row (student) was classified according to the rules
#'
#' @description
#' Shows whether each rule was triggered or not, and how the final classification
#' was reached.
#'
#' @param data A data frame or tibble that has already been classified (i.e.,
#'   it has a column with final outcomes).
#' @param ruleset An object of class "academic_ruleset".
#' @param outcome_col_name Character. The name of the classification column in `data`.
#'
#' @return A list (or data frame) detailing which rules were triggered for each row.
#' @export
#'
#' @examples
#' # Using the previous example:
#' df <- data.frame(
#'   student_id = 1:5,
#'   total_score = c(26, 23, 30, 10, 29),
#'   fail_count = c(0, 1, 2, 3, 1),
#'   essay_grade = c(3, 2, 1, 0, 4)
#' )
#' my_rules <- define_rule_set(
#'   list(
#'     "fail_too_many" = list(type = "threshold", value = 2, dimension = "fail_count"),
#'     "min_total" = list(type = "threshold", value = 24, dimension = "total_score"),
#'     "pass_essay" = list(type = "logical", expr = ~ essay_grade >= 2)
#'   ),
#'   default_outcome = "PASS"
#' )
#'
#' df_classified <- apply_rules(df, my_rules, outcome_col_name = "OUTCOME")
#' explain_classification(df_classified, my_rules, "OUTCOME")
explain_classification <- function(data, ruleset, outcome_col_name = "OUTCOME") {
  if (!inherits(ruleset, "academic_ruleset")) {
    stop("ruleset must be an object of class 'academic_ruleset'.")
  }
  if (!outcome_col_name %in% names(data)) {
    stop(paste("Column", outcome_col_name, "not found in data."))
  }

  explanations <- vector("list", nrow(data))

  # For each row (student), evaluate each rule and see if it triggered "FAIL"
  for (i in seq_len(nrow(data))) {
    row_info <- data[i, , drop = FALSE]
    row_rules <- list()

    for (rule_name in ruleset$rule_priority) {
      current_rule <- ruleset$rules[[rule_name]]
      triggered <- FALSE

      if (current_rule$type == "threshold") {
        # same threshold logic
        dim_col <- current_rule$dimension
        threshold <- current_rule$value

        triggered <- switch(
          EXPR = dim_col,
          "total_score" = row_info[[dim_col]] < threshold,
          "fail_count"  = row_info[[dim_col]] > threshold,
          # fallback
          row_info[[dim_col]] < threshold
        )

        row_rules[[rule_name]] <- if (triggered) {
          paste0("Rule '", rule_name, "' triggered: ",
                 "[", dim_col, "=", row_info[[dim_col]],
                 "] vs threshold=", threshold)
        } else {
          paste0("Rule '", rule_name, "' not triggered.")
        }

      } else if (current_rule$type == "logical") {
        # Same fix as in `apply_rules()`
        expr_fn  <- current_rule$expr
        expr_rhs <- expr_fn[[2]]   # The RHS of ~

        # Evaluate for this single row
        value <- eval(expr_rhs, envir = row_info)

        # `value` should be a single logical if everything is correct
        # e.g., essay_grade >= 2 => TRUE/FALSE for that row
        if (!is.logical(value) || length(value) != 1) {
          stop(
            sprintf(
              "Logical rule '%s' did not return a single TRUE/FALSE in row %d.",
              rule_name, i
            )
          )
        }

        triggered <- !value  # If the expression is not satisfied, consider rule triggered

        row_rules[[rule_name]] <- if (triggered) {
          paste0("Rule '", rule_name, "' triggered: expression not satisfied.")
        } else {
          paste0("Rule '", rule_name, "' not triggered.")
        }

      } else {
        stop(paste("Unknown rule type:", current_rule$type))
      }
    }

    final_status <- row_info[[outcome_col_name]]
    explanations[[i]] <- list(
      index = i,
      final_classification = final_status,
      rules_evaluation = row_rules
    )
  }

  return(explanations)
}
