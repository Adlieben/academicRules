test_that("apply_rules() assigns outcomes correctly for threshold rules", {
  # Sample data
  df <- data.frame(
    student_id   = 1:5,
    total_score  = c(26, 23, 30, 10, 29),
    fail_count   = c(0, 1, 2, 3, 1)
  )

  # Define ruleset
  rs <- define_rule_set(
    list(
      "min_total"    = list(type = "threshold", value = 24, dimension = "total_score"),
      "max_fail_cnt" = list(type = "threshold", value = 2, dimension = "fail_count")
    ),
    default_outcome = "PASS"
  )

  # Apply rules
  out <- apply_rules(df, rs, outcome_col_name = "OUTCOME")

  expect_true("OUTCOME" %in% names(out))
  # Check some expected outcomes
  # Student 1: total_score = 26, fail_count=0 => PASS
  expect_equal(out$OUTCOME[1], "PASS")

  # Student 2: total_score = 23 (< 24) => should FAIL on "min_total"
  expect_equal(out$OUTCOME[2], "FAIL")

  # Student 4: total_score=10 => definitely FAIL
  # Student 3: total_score=30, fail_count=2 => PASS
  # Student 5: total_score=29, fail_count=1 => PASS
})

test_that("apply_rules() handles logical rules (formulas)", {
  df <- data.frame(
    student_id   = 1:3,
    total_score  = c(25, 15, 27),
    essay_grade  = c(3, 1, 2)
  )

  # Define ruleset with a logical rule
  rs <- define_rule_set(
    list(
      "min_total" = list(type = "threshold", value = 24, dimension = "total_score"),
      "pass_essay" = list(type = "logical", expr = ~ essay_grade >= 2)
    ),
    default_outcome = "PASS"
  )

  out <- apply_rules(df, rs, outcome_col_name = "OUTCOME")

  # Student 2 fails essay rule
  expect_equal(out$OUTCOME[2], "FAIL")
  # Students 1 and 3 pass everything
  expect_equal(out$OUTCOME[c(1,3)], c("PASS", "PASS"))
})

test_that("apply_rules() errors on non-existent column in threshold rule", {
  df <- data.frame(x = 1:3)
  rs <- define_rule_set(
    list("fake_rule" = list(type="threshold", value=2, dimension="bad_col"))
  )

  expect_error(
    apply_rules(df, rs),
    "Column bad_col not found in data."
  )
})
