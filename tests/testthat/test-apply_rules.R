test_that("apply_rules() works with minimum rules", {
  df <- data.frame(id=1:3, score=c(60, 49, 50))
  rs <- define_rule_set(list(
    min_score = list(type="minimum", value=50, dimension="score")
  ))
  out <- apply_rules(df, rs, "OUTCOME")
  # Fail if score<50
  expect_equal(out$OUTCOME, c("PASS","FAIL","PASS"))
})

test_that("apply_rules() works with maximum rules", {
  df <- data.frame(id=1:3, fails=c(1, 2, 3))
  rs <- define_rule_set(list(
    max_fails = list(type="maximum", value=2, dimension="fails")
  ))
  out <- apply_rules(df, rs, "OUTCOME")
  # Fail if fails>2
  expect_equal(out$OUTCOME, c("PASS","PASS","FAIL"))
})

test_that("apply_rules() works with logical rules", {
  df <- data.frame(id=1:3, x=c(10, -1, 5))
  rs <- define_rule_set(list(
    must_be_positive = list(type="logical", expr=~ x>0)
  ))
  out <- apply_rules(df, rs, "OUTCOME")
  expect_equal(out$OUTCOME, c("PASS","FAIL","PASS"))
})

test_that("apply_rules() errors on missing column for min/max rule", {
  df <- data.frame(foo=1:3)
  rs <- define_rule_set(list(
    min_bar = list(type="minimum", value=10, dimension="bar")
  ))
  expect_error(apply_rules(df, rs),
               "Column 'bar' not found in data for rule 'min_bar'.")
})
