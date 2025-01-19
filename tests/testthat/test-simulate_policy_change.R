test_that("simulate_policy_change() modifies the rule and re-applies", {
  df <- data.frame(score=c(24, 25, 30))
  base_rules <- define_rule_set(list(
    min_score = list(type="minimum", value=24, dimension="score")
  ))
  baseline <- apply_rules(df, base_rules, "OUTCOME_BASE")

  # Now raise min_score from 24 to 26
  changed <- simulate_policy_change(
    data = df,
    ruleset = base_rules,
    modifications = list(min_score = list(value=26)),
    outcome_col_name = "OUTCOME_NEW"
  )

  # Compare
  combo <- cbind(baseline, changed["OUTCOME_NEW"])
  # Row 1 => 24 => pass in baseline, fail in new
  expect_equal(combo$OUTCOME_BASE[1], "PASS")
  expect_equal(combo$OUTCOME_NEW[1], "FAIL")
  # Row 2 => 25 => pass vs fail
  expect_equal(combo$OUTCOME_NEW[2], "FAIL")
  # Row 3 => 30 => pass in both
  expect_equal(combo$OUTCOME_NEW[3], "PASS")
})

test_that("simulate_policy_change() warns if rule doesn't exist", {
  df <- data.frame(x=1:3)
  rs <- define_rule_set(list(my_rule=list(type="minimum", value=10, dimension="x")))
  expect_warning(
    simulate_policy_change(df, rs, modifications=list(fake_rule=list(value=99))),
    "Rule 'fake_rule' not found in ruleset; skipping."
  )
})
