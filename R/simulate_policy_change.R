#' Simulate a policy change by modifying or removing rules
#'
#' @description
#' Creates a temporary copy of the ruleset, applies your requested modifications
#' (e.g., changing the \code{value} for a rule or removing a rule entirely), and
#' then applies the updated ruleset to the dataset to get a new outcome column.
#'
#' @details
#' - **Modifying a rule**: Provide a named list of fields to change, e.g.,
#'   \code{list(minimum_score = list(value=26))} will set \code{rules$minimum_score$value = 26}.
#'
#' - **Removing a rule**: Provide \code{NULL} instead of a list for the rule, e.g.
#'   \code{list(cas_requirement = NULL)}. This completely removes the rule from
#'   the ruleset (and from \code{rule_priority}).
#'
#' @param data A data frame or tibble containing student/entity data.
#' @param ruleset An object of class \code{academic_ruleset} from
#'   \code{\link{define_rule_set}}.
#' @param modifications A named list of modifications. Each name must match a rule
#'   in \code{ruleset}. The value is either:
#'   \itemize{
#'     \item A list of fields to change (e.g., \code{list(value=26)}),
#'     \item \code{NULL}, indicating that the rule should be removed entirely.
#'   }
#' @param outcome_col_name A string for the new outcome column (e.g. \code{"OUTCOME_CHANGED"}).
#'
#' @return A copy of \code{data} with a new column \code{outcome_col_name}
#'   containing the results of applying the modified ruleset.
#'
#' @seealso \code{\link{apply_rules}} for applying a ruleset directly.
#' @export
#'
#' @examples
#' # Example: raising a minimum threshold and removing a logical rule (based on IBDP passing criteria)
#' df <- data.frame(score = c(24,25,30), CAS_met = c(FALSE, TRUE, TRUE))
#'
#' rs <- define_rule_set(
#'   list(
#'     cas_rule = list(type = "logical", expr = ~ CAS_met),
#'     min_score = list(type = "minimum", value = 24, dimension = "score")
#'   ),
#'   default_outcome = "PASS"
#' )
#'
#' # Baseline classification
#' baseline <- apply_rules(df, rs, outcome_col_name = "BASE_OUTCOME")
#'
#' # Now remove 'cas_rule' and raise 'min_score' to 26
#' modifications <- list(
#'   cas_rule = NULL,             # remove the CAS rule
#'   min_score = list(value = 26) # raise threshold from 24 to 26
#' )
#'
#' changed <- simulate_policy_change(df, rs, modifications, "NEW_OUTCOME")
#'
#' cbind(baseline, changed["NEW_OUTCOME"])
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

    if (is.null(fields_to_change)) {
      # Remove the rule entirely
      message(sprintf("Removing rule '%s' from the ruleset.", rule_name))
      new_ruleset$rules[[rule_name]] <- NULL
      new_ruleset$rule_priority <- setdiff(new_ruleset$rule_priority, rule_name)
    } else {
      # Modify specific fields of the rule
      for (fld in names(fields_to_change)) {
        new_ruleset$rules[[rule_name]][[fld]] <- fields_to_change[[fld]]
      }
    }
  }

  # Now apply the modified ruleset
  result <- apply_rules(data, new_ruleset, outcome_col_name)
  result
}
