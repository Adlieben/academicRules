#' Define a set of academic rules for classification
#'
#' @description
#' Creates a list (or other structure) of user-specified pass/fail or multi-category
#' rules. Each "rule" can be a named function, expression, or threshold that will be
#' applied to a student's record.
#'
#' @param rules_list A named list or vector describing each rule. E.g.,
#'   list(
#'     "min_total" = list(type = "threshold", value = 24),
#'     "max_fails" = list(type = "threshold", value = 1, dimension = "fail_count")
#'   )
#' @param default_outcome Character. The classification assigned if no rules are
#'   triggered (e.g., "PASS").
#' @param rule_priority A character vector indicating the order in which rules
#'   should be checked (optional).
#'
#' @return A list containing all the relevant rules and parameters.
#' @export
#'
#' @examples
#' my_rules <- define_rule_set(
#'   rules_list = list(
#'     "min_total" = list(type = "threshold", value = 24, dimension = "total_score"),
#'     "max_fails" = list(type = "threshold", value = 2, dimension = "fail_count"),
#'     "must_pass_essay" = list(type = "logical", expr = ~ essay_grade >= 2)
#'   ),
#'   default_outcome = "PASS"
#' )
define_rule_set <- function(rules_list = list(),
                            default_outcome = "PASS",
                            rule_priority = NULL) {

  # Validate input
  if (!is.list(rules_list)) {
    stop("`rules_list` must be a list.")
  }

  # Optionally define a priority order if not provided
  if (is.null(rule_priority)) {
    rule_priority <- names(rules_list)
  }

  # Build the ruleset object
  ruleset <- list(
    rules = rules_list,
    default_outcome = default_outcome,
    rule_priority = rule_priority
  )

  class(ruleset) <- "academic_ruleset"
  return(ruleset)
}
