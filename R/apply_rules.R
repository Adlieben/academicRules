#' Apply academic rules to a data frame
#'
#' @description
#' For each row in \code{data}, evaluates the rules in \code{ruleset} in the specified priority
#' order. If a rule is triggered, that row's outcome is set to "FAIL" (or equivalent).
#' If no rules are triggered, the row remains at the default outcome (e.g., "PASS").
#'
#' @param data A data frame or tibble. Each row represents a student or entity.
#' @param ruleset An object of class "academic_ruleset" from \code{\link{define_rule_set}}.
#' @param outcome_col_name A character string for the new column's name (e.g. "OUTCOME").
#'
#' @return The original \code{data} with an extra column named \code{outcome_col_name}.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = 1:4,
#'   score = c(60, 49, 70, 50),
#'   fails = c(0, 1, 3, 2)
#' )
#'
#' rs <- define_rule_set(list(
#'   "min_score" = list(type = "minimum", value = 50, dimension = "score"),
#'   "max_fails" = list(type = "maximum", value = 2, dimension = "fails")
#' ))
#'
#' out <- apply_rules(df, rs, "RESULT")
#' out
apply_rules <- function(data, ruleset, outcome_col_name = "OUTCOME") {
  if (!inherits(ruleset, "academic_ruleset")) {
    stop("`ruleset` must be an object of class 'academic_ruleset'.")
  }

  # Initialize the outcome with the default
  data[[outcome_col_name]] <- ruleset$default_outcome

  # Apply each rule in order
  for (rule_name in ruleset$rule_priority) {
    rule <- ruleset$rules[[rule_name]]

    # Distinguish the three rule types
    if (rule$type == "minimum") {
      dim_col <- rule$dimension
      val <- rule$value
      # Must exist in data
      if (!dim_col %in% names(data)) {
        stop(sprintf("Column '%s' not found in data for rule '%s'.",
                     dim_col, rule_name))
      }
      # Fail if data[dim_col] < val
      fail_rows <- which(data[[dim_col]] < val)
      data[[outcome_col_name]][fail_rows] <- "FAIL"

    } else if (rule$type == "maximum") {
      dim_col <- rule$dimension
      val <- rule$value
      if (!dim_col %in% names(data)) {
        stop(sprintf("Column '%s' not found in data for rule '%s'.",
                     dim_col, rule_name))
      }
      # Fail if data[dim_col] > val
      fail_rows <- which(data[[dim_col]] > val)
      data[[outcome_col_name]][fail_rows] <- "FAIL"

    } else if (rule$type == "logical") {
      expr_fn <- rule$expr
      # Evaluate the formula's RHS in the context of the entire data
      expr_rhs <- expr_fn[[2]]
      result_vec <- eval(expr_rhs, envir = data)
      # If it's not logical or has any NAs, decide how to handle:
      # For simplicity, let's treat NA as fail => we check !TRUE
      fail_rows <- which(!isTRUEx(result_vec))
      data[[outcome_col_name]][fail_rows] <- "FAIL"

    } else {
      stop(sprintf("Unknown rule type '%s' for rule '%s'.", rule$type, rule_name))
    }
  }

  data
}

# Helper function to handle isTRUE for vectors (treat NA as fail)
isTRUEx <- function(x) {
  # Return TRUE if and only if x is TRUE.
  # Return FALSE if x is FALSE or NA
  if (is.logical(x)) {
    # Replace NA with FALSE
    x[is.na(x)] <- FALSE
    x
  } else {
    rep(FALSE, length(x))
  }
}
