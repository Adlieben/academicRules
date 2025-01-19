#' Explain how each row was classified according to the rules
#'
#' @description
#' For each row, indicates whether each rule triggered a fail or not, and shows the final outcome.
#'
#' @param data A data frame that was already classified by \code{\link{apply_rules}}, i.e.,
#'   it has a final outcome column.
#' @param ruleset An object of class "academic_ruleset".
#' @param outcome_col_name The name of the classification column in \code{data} (e.g. "OUTCOME").
#'
#' @return A list of length \code{nrow(data)}, where each element has:
#'   \itemize{
#'     \item \code{index}: row index
#'     \item \code{final_classification}: the final classification (PASS or FAIL, etc.)
#'     \item \code{rules_evaluation}: a list of texts describing whether each rule was triggered
#'   }
#' @export
#'
#' @examples
#' df <- data.frame(id=1:2, score=c(60,45), fails=c(0,3))
#' rs <- define_rule_set(list(
#'   min_score = list(type="minimum", value=50, dimension="score"),
#'   max_fails = list(type="maximum", value=2, dimension="fails")
#' ))
#'
#' out <- apply_rules(df, rs, "OUTCOME")
#' explain_classification(out, rs, "OUTCOME")
explain_classification <- function(data, ruleset, outcome_col_name = "OUTCOME") {
  if (!inherits(ruleset, "academic_ruleset")) {
    stop("`ruleset` must be an object of class 'academic_ruleset'.")
  }
  if (!outcome_col_name %in% names(data)) {
    stop(sprintf("Column '%s' not found in data.", outcome_col_name))
  }

  explanations <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row_info <- data[i, , drop = FALSE]
    row_rules <- list()

    for (rule_name in ruleset$rule_priority) {
      rdef <- ruleset$rules[[rule_name]]
      triggered <- FALSE

      if (rdef$type == "minimum") {
        dim_col <- rdef$dimension
        val <- rdef$value
        triggered <- row_info[[dim_col]] < val
        row_rules[[rule_name]] <- if (triggered) {
          sprintf("Triggered: %s < %s", row_info[[dim_col]], val)
        } else {
          sprintf("Not triggered: %s >= %s", row_info[[dim_col]], val)
        }

      } else if (rdef$type == "maximum") {
        dim_col <- rdef$dimension
        val <- rdef$value
        triggered <- row_info[[dim_col]] > val
        row_rules[[rule_name]] <- if (triggered) {
          sprintf("Triggered: %s > %s", row_info[[dim_col]], val)
        } else {
          sprintf("Not triggered: %s <= %s", row_info[[dim_col]], val)
        }

      } else if (rdef$type == "logical") {
        expr_rhs <- rdef$expr[[2]]
        val <- eval(expr_rhs, envir=row_info)
        # triggered if the expression is FALSE/NA
        triggered <- !isTRUEx(val)
        row_rules[[rule_name]] <- if (triggered) {
          "Triggered: logical expression not satisfied"
        } else {
          "Not triggered: expression is TRUE"
        }
      }

    }

    final_status <- row_info[[outcome_col_name]]
    explanations[[i]] <- list(
      index = i,
      final_classification = final_status,
      rules_evaluation = row_rules
    )
  }

  explanations
}
