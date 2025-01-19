#' Simulate a policy change by modifying one or more rules and re-evaluating
#'
#' @description
#' Creates a temporary copy of the ruleset with one or more changed values
#' (for example, raising a "minimum" cutoff from 24 to 26), applies it to the dataset,
#' and returns the newly classified data.
#'
#' @param data A data frame or tibble with student/entity data.
#' @param ruleset An object of class "academic_ruleset".
#' @param modifications A named list, where each name is a rule in \code{ruleset},
#'   and the value is another list specifying which fields to change. For example:
#'   \code{list(min_total = list(value = 26))} to change the \code{value} for
#'   rule \code{min_total}.
#' @param outcome_col_name The name of the new outcome column to use.
#'
#' @return A data frame (same as \code{data}) but with an additional
#'   \code{outcome_col_name} column representing the newly evaluated results.
#' @export
#'
#' @examples
#' df <- data.frame(score=c(30,25,20,10), fails=c(0,1,3,2))
#' rs <- define_rule_set(list(
#'   min_score = list(type="minimum", value=24, dimension="score"),
#'   max_fails = list(type="maximum", value=2, dimension="fails")
#' ))
#'
#' # Baseline
#' baseline <- apply_rules(df, rs, "OUTCOME_BASE")
#'
#' # Raise min_score from 24 to 26
#' changed <- simulate_policy_change(df, rs,
#'   modifications = list(min_score = list(value=26)),
#'   outcome_col_name = "OUTCOME_NEW"
#' )
#'
#' cbind(baseline, changed["OUTCOME_NEW"])
simulate_policy_change <- function(data,
                                   ruleset,
                                   modifications = list(),
                                   outcome_col_name = "OUTCOME_CHANGED") {
  if (!inherits(ruleset, "academic_ruleset")) {
    stop("`ruleset` must be an object of class 'academic_ruleset'.")
  }

  # Make a copy so we don't overwrite the original
  new_ruleset <- ruleset

  # Apply each modification
  for (rule_name in names(modifications)) {
    if (!rule_name %in% names(new_ruleset$rules)) {
      warning(sprintf("Rule '%s' not found in ruleset; skipping.", rule_name))
      next
    }
    fields_to_change <- modifications[[rule_name]]
    for (fld in names(fields_to_change)) {
      new_ruleset$rules[[rule_name]][[fld]] <- fields_to_change[[fld]]
    }
  }

  # Now apply the modified rules
  result <- apply_rules(data, new_ruleset, outcome_col_name)
  result
}
