#' Simulate policy changes to academic rules and compare outcomes
#'
#' @description
#' Temporarily adjusts rules or thresholds, applies them to the dataset, and
#' compares outcomes against a baseline.
#'
#' @param data A data frame or tibble containing student data.
#' @param ruleset An object of class "academic_ruleset".
#' @param modifications A named list indicating which rules to modify and how,
#'   e.g. \code{list(min_total = list(value = 26))} to change the min_total threshold to 26.
#' @param outcome_col_name Character. Name of classification column for new outcomes.
#'
#' @return A data frame with both old and new outcomes or a summary of changes in pass/fail rates.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   student_id = 1:5,
#'   total_score = c(26, 23, 30, 10, 29),
#'   fail_count = c(0, 1, 2, 3, 1),
#'   essay_grade = c(3, 2, 1, 0, 4)
#' )
#' my_rules <- define_rule_set(
#'   list(
#'     "fail_too_many" = list(type = "threshold", value = 2, dimension = "fail_count"),
#'     "min_total" = list(type = "threshold", value = 24, dimension = "total_score")
#'   )
#' )
#'
#' baseline <- apply_rules(df, my_rules, outcome_col_name = "OUTCOME_BASE")
#'
#' # Increase min_total from 24 to 26
#' changed <- simulate_policy_change(
#'   data = df,
#'   ruleset = my_rules,
#'   modifications = list(min_total = list(value = 26)),
#'   outcome_col_name = "OUTCOME_NEW"
#' )
#'
#' # Compare pass/fail changes
#' table(baseline$OUTCOME_BASE, changed$OUTCOME_NEW)
simulate_policy_change <- function(data, ruleset, modifications = list(),
                                   outcome_col_name = "OUTCOME_CHANGED") {

  if (!inherits(ruleset, "academic_ruleset")) {
    stop("ruleset must be an object of class 'academic_ruleset'.")
  }

  # Create a copy so we don't alter the original ruleset
  new_ruleset <- ruleset

  # For each rule in `modifications`, override the specified fields
  for (rule_name in names(modifications)) {
    if (!rule_name %in% names(new_ruleset$rules)) {
      warning(paste("Rule", rule_name, "not found in ruleset; skipping."))
      next
    }
    # Modify the existing rule
    for (field in names(modifications[[rule_name]])) {
      new_ruleset$rules[[rule_name]][[field]] <- modifications[[rule_name]][[field]]
    }
  }

  # Now apply the modified ruleset
  results <- apply_rules(data, new_ruleset, outcome_col_name = outcome_col_name)

  return(results)
}
