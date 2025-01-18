test_that("define_rule_set() returns a valid academic_ruleset object", {
  rules <- list(
    "min_total" = list(type = "threshold", value = 24, dimension = "total_score"),
    "max_fails" = list(type = "threshold", value = 2, dimension = "fail_count")
  )

  rs <- define_rule_set(
    rules_list = rules,
    default_outcome = "PASS"
  )

  # Check class
  expect_s3_class(rs, "academic_ruleset")

  # Check the rules
  expect_named(rs$rules, c("min_total", "max_fails"))
  expect_equal(rs$default_outcome, "PASS")
})

test_that("define_rule_set() errors if rules_list is not a list", {
  expect_error(
    define_rule_set(rules_list = "not_a_list"),
    "must be a list"
  )
})

test_that("define_rule_set() sets rule_priority if not provided", {
  rules <- list(
    "rule1" = list(type = "threshold", value = 10, dimension = "x"),
    "rule2" = list(type = "logical", expr = ~ y > 0)
  )
  rs <- define_rule_set(rules_list = rules)

  # By default, rule_priority should match names(rules)
  expect_equal(rs$rule_priority, names(rules))
})
