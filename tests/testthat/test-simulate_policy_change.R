test_that("simulate_policy_change() modifies rules and changes outcome", {
  df <- data.frame(
    student_id  = 1:4,
    total_score = c(24, 23, 30, 25),
    fail_count  = c(1, 1, 0, 2)
  )

  base_rules <- define_rule_set(
    list(
      "min_total" = list(type = "threshold", value = 24, dimension = "total_score")
    ),
    default_outcome = "PASS"
  )

  # Baseline
  baseline <- apply_rules(df, base_rules, outcome_col_name = "OUTCOME_BASE")

  # Now simulate a stricter threshold => 25
  changed <- simulate_policy_change(
    data = df,
    ruleset = base_rules,
    modifications = list(min_total = list(value = 25)),
    outcome_col_name = "OUTCOME_NEW"
  )

  # Compare old vs. new
  merged <- cbind(baseline, changed["OUTCOME_NEW"])

  # Student 1: total_score=24 => PASS in baseline but now FAIL
  expect_equal(merged$OUTCOME_BASE[1], "PASS")
  expect_equal(merged$OUTCOME_NEW[1],  "FAIL")

  # Student 3 => total_score=30 => remains PASS
  expect_equal(merged$OUTCOME_BASE[3], "PASS")
  expect_equal(merged$OUTCOME_NEW[3],  "PASS")
})

test_that("simulate_policy_change() warns if a rule doesn't exist", {
  df <- data.frame(x = 1:3)
  rs <- define_rule_set(
    list("some_rule" = list(type="threshold", value=2, dimension="x"))
  )

  expect_warning(
    simulate_policy_change(
      data = df,
      ruleset = rs,
      modifications = list(fake_rule = list(value = 100))
    ),
    "not found in ruleset; skipping"
  )
})
