test_that("tune_rules() can tune multiple numeric rules to approach a target pass rate", {
  # Create a small dataset with varied values
  df <- data.frame(
    # For a quick test, 4 students with minimal columns
    Total  = c(20, 24, 28, 30),
    count_2= c(0,  2,  3,  1)
  )

  # Define ruleset with two numeric rules
  rs <- define_rule_set(
    list(
      min_total = list(type="minimum", value=24, dimension="Total"),
      max_2s    = list(type="maximum", value=2, dimension="count_2")
    ),
    default_outcome = "PASS"
  )

  # We want a pass rate near 50%. We'll try:
  #   min_total in [20,24,28]
  #   max_2s    in [1,2,3]
  # That yields 3 x 3 = 9 combinations
  out <- tune_rules(
    data        = df,
    ruleset     = rs,
    candidates  = list(
      min_total = c(20, 24, 28),
      max_2s    = c(1, 2, 3)
    ),
    target_rate = 0.5,
    outcome_col_name = "TUNED_OUTCOME"
  )

  # Check structure of returned object
  expect_true("best_params" %in% names(out))
  expect_true("best_rate"   %in% names(out))
  expect_true("results"     %in% names(out))

  # The results data frame should have 9 rows (3*3 combos)
  expect_equal(nrow(out$results), 9)

  # best_params should be a list of length 2 (one entry per rule)
  expect_equal(length(out$best_params), 2)

  # best_rate should be between 0 and 1
  expect_true(out$best_rate >= 0 && out$best_rate <= 1)
})
