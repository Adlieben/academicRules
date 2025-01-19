test_that("explain_classification() returns triggered info correctly", {
  df <- data.frame(id=1:2, score=c(60,40), fails=c(0,3))
  rs <- define_rule_set(list(
    min_score = list(type="minimum", value=50, dimension="score"),
    max_fails = list(type="maximum", value=2, dimension="fails")
  ))
  classified <- apply_rules(df, rs, "OUTCOME")
  expl <- explain_classification(classified, rs, "OUTCOME")
  expect_length(expl, 2)
  expect_true("rules_evaluation" %in% names(expl[[1]]))

  # Student 1 => score=60 => not triggered for min_score
  #             fails=0 => not triggered for max_fails => PASS
  # Student 2 => score=40 => triggered min_score => FAIL
  #             fails=3 => triggered max_fails => also FAIL
  expect_match(expl[[1]]$rules_evaluation$min_score, "Not triggered")
  expect_match(expl[[2]]$rules_evaluation$min_score, "Triggered")
})
