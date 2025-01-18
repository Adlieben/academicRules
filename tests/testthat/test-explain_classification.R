test_that("explain_classification() returns correct triggered info", {
  df <- data.frame(
    student_id  = 1:3,
    total_score = c(26, 23, 10),
    fail_count  = c(0, 1, 3),
    essay_grade = c(3, 2, 1)
  )

  rules <- list(
    "fail_too_many" = list(type = "threshold", value = 2, dimension = "fail_count"),
    "min_total"     = list(type = "threshold", value = 24, dimension = "total_score"),
    "pass_essay"    = list(type = "logical",   expr = ~ essay_grade >= 2)
  )

  rs <- define_rule_set(rules, default_outcome = "PASS")
  df_classified <- apply_rules(df, rs, "OUTCOME")

  expl <- explain_classification(df_classified, rs, "OUTCOME")

  # We expect a list of length 3 (one per row)
  expect_length(expl, 3)
  expect_true(all(c("index", "final_classification", "rules_evaluation") %in%
                    names(expl[[1]])))

  # Check a triggered rule example
  # Student 3 => total_score=10, definitely triggers min_total, maybe fail_too_many
  triggered_text <- expl[[3]]$rules_evaluation[["min_total"]]
  expect_match(triggered_text, "triggered")

  # Student 2 => total_score=23 triggers min_total, essay_grade=2 is okay
  # so pass_essay "not triggered"
})

test_that("explain_classification() errors if final outcome column is missing", {
  df <- data.frame(x = 1:3)
  rs <- define_rule_set(list())

  # No 'OUTCOME' column in df
  expect_error(explain_classification(df, rs, "OUTCOME"),
               "not found in data")
})
