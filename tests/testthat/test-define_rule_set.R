test_that("define_rule_set() returns an academic_ruleset object", {
  rules <- list(
    "min_score" = list(type="minimum", value=50, dimension="score"),
    "max_fails" = list(type="maximum", value=2, dimension="fails"),
    "check_logic"= list(type="logical", expr=~ x>0)
  )
  rs <- define_rule_set(rules)
  expect_s3_class(rs, "academic_ruleset")
  expect_equal(rs$rule_priority, names(rules))
})

test_that("define_rule_set() complains about invalid types", {
  bad_rules <- list("bad"=list(type="invalid", value=10, dimension="foo"))
  expect_error(define_rule_set(bad_rules),
               "invalid type 'invalid'. Must be one of: minimum, maximum, logical")
})
