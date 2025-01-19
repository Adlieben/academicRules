#' Define a set of academic rules (minimum, maximum, or logical)
#'
#' @description
#' Creates and returns an object of class "academic_ruleset", containing one or more rules.
#' Each rule can be of type:
#'  - "minimum": Fail if a column is below a specified value.
#'  - "maximum": Fail if a column is above a specified value.
#'  - "logical": Fail if a row-wise logical expression evaluates to FALSE.
#'
#' @param rules_list A named list of rules. Each element has:
#' \describe{
#'   \item{\code{type}}{Either "minimum", "maximum", or "logical".}
#'   \item{\code{value}}{(For "minimum" or "maximum") A numeric value to compare against.}
#'   \item{\code{dimension}}{(For "minimum" or "maximum") The name of a numeric column in the data.}
#'   \item{\code{expr}}{(For "logical") A formula like \code{~ expression}, evaluated row-wise.}
#' }
#' @param default_outcome Character string giving the outcome to assign if none of the rules
#'   are triggered for a row (e.g., "PASS").
#' @param rule_priority A character vector giving the order in which rules are applied.
#'   If \code{NULL}, the order is simply the names in \code{rules_list}.
#'
#' @return An object of class \code{"academic_ruleset"}, which is essentially a list with
#' \itemize{
#'   \item \code{rules}: the \code{rules_list}
#'   \item \code{default_outcome}: the default outcome
#'   \item \code{rule_priority}: the order of rule application
#' }
#' @export
#'
#' @examples
#' # A simple set of rules
#' my_rules <- define_rule_set(
#'   rules_list = list(
#'     "min_score" = list(type = "minimum", value = 50, dimension = "test_score"),
#'     "too_many_fails" = list(type = "maximum", value = 2, dimension = "fail_count"),
#'     "final_check" = list(type = "logical", expr = ~ subject_grade != "N")
#'   ),
#'   default_outcome = "PASS"
#' )
define_rule_set <- function(rules_list = list(),
                            default_outcome = "PASS",
                            rule_priority = NULL) {

  if (!is.list(rules_list)) {
    stop("`rules_list` must be a list of rules.")
  }

  # Validate rule types
  valid_types <- c("minimum", "maximum", "logical")
  for (rn in names(rules_list)) {
    this_type <- rules_list[[rn]]$type
    if (!this_type %in% valid_types) {
      stop(sprintf("Rule '%s' has invalid type '%s'. Must be one of: %s",
                   rn, this_type, paste(valid_types, collapse = ", ")))
    }
  }

  # If no priority is given, use the names of the rules in their current order
  if (is.null(rule_priority)) {
    rule_priority <- names(rules_list)
  }

  ruleset <- list(
    rules = rules_list,
    default_outcome = default_outcome,
    rule_priority = rule_priority
  )

  class(ruleset) <- "academic_ruleset"
  ruleset
}
