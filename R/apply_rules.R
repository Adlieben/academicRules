#' Apply defined academic rules to a dataset
#'
#' @description
#' Evaluates each row in `data` against the rule set defined by `ruleset`.
#'
#' @param data A data frame or tibble. Each row represents a single student's (or entity's) data.
#' @param ruleset An object of class "academic_ruleset" (from \code{\link{define_rule_set}}).
#' @param outcome_col_name Character. The name of the new column for classification result.
#'
#' @return The input data frame with an additional column containing classification outcomes.
#' @export
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   student_id = 1:5,
#'   total_score = c(26, 23, 30, 10, 29),
#'   fail_count = c(0, 1, 2, 3, 1),
#'   essay_grade = c(3, 2, 1, 0, 4)
#' )
#'
#' my_rules <- define_rule_set(
#'   list(
#'     "fail_too_many" = list(type = "threshold", value = 2, dimension = "fail_count"),
#'     "min_total"     = list(type = "threshold", value = 24, dimension = "total_score"),
#'     "pass_essay"    = list(type = "logical",   expr = ~ essay_grade >= 2)
#'   ),
#'   default_outcome = "PASS"
#' )
#'
#' apply_rules(df, my_rules)
apply_rules <- function(data, ruleset, outcome_col_name = "OUTCOME") {
  if (!inherits(ruleset, "academic_ruleset")) {
    stop("ruleset must be an object of class 'academic_ruleset'.")
  }

  # Initialize outcome with the default
  data[[outcome_col_name]] <- ruleset$default_outcome

  # Evaluate each rule in the specified priority order
  for (rule_name in ruleset$rule_priority) {
    current_rule <- ruleset$rules[[rule_name]]

    if (is.null(current_rule$type)) {
      stop(paste("Rule", rule_name, "does not have a 'type' element."))
    }

    # Evaluate threshold-based rule
    if (current_rule$type == "threshold") {
      dim_col <- current_rule$dimension
      threshold <- current_rule$value

      if (!dim_col %in% names(data)) {
        stop(paste("Column", dim_col, "not found in data."))
      }

      failing_indices <- switch(
        EXPR = dim_col,
        "total_score" = which(data[[dim_col]] < threshold),
        "fail_count"  = which(data[[dim_col]] > threshold),
        which(data[[dim_col]] < threshold)
      )

      data[[outcome_col_name]][failing_indices] <- "FAIL"
    } else if (current_rule$type == "logical") {
      # Evaluate the formula in the context of 'data'
      expr_fn <- current_rule$expr
      expr_rhs <- expr_fn[[2]]  # This is the actual expression on the RHS of ~

      # Evaluate to get a logical vector
      result_vec <- eval(expr_rhs, envir = data)

      # Mark rows that do NOT satisfy the condition as "FAIL"
      failing_indices <- which(!result_vec)
      data[[outcome_col_name]][failing_indices] <- "FAIL"

    } else {
      stop(paste("Unknown rule type:", current_rule$type))
    }
  }

  return(data)
}
