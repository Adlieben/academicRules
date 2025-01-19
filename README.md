# academicRules

This R package was created to facilitate the thesis project conducted by [Adam Maghout](https://www.adammaghout-academic.com/) at the International Baccalaureate Organisation (IBO). The functions within the package allow users to obtain passing decisions for students given a set of passing criteria. The package also enables investigating the consequences of a change in passing criteria and finding optimal passing criteria to approach a target passing rate (e.g. 80\%).

## Installation

You can install the latest version of `academicRules` from GitHub with:

```{r}
remotes::install_github("Adlieben/academicRules")
```

## Quickstart Example

The SynthIB dataset can be used to get a basic understanding of the main functionalities of the package:

```r
library(academicRules)

# 1) Load the built-in example data
data("SynthIB")

# 2) Define some simple rules
my_rules <- define_rule_set(
  list(
    min_score = list(type="minimum", value=24, dimension="Total"),
    max_fails = list(type="maximum", value=2, dimension="count_2")
  )
)

# 3) Apply the rules to see who passes/fails
results <- apply_rules(dummy_ib_data, my_rules, outcome_col_name="OUTCOME")
table(results$OUTCOME)
```

## Documentation

A detailed reference for all functions, along with vignettes and examples, is available on the
[academicRules website](https://adlieben.github.io/academicRules/).

## Acknowledgments

- [Utrecht University](https://www.uu.nl/en) for providing resources during the thesis.
- [International Baccalaureate Organisation](https://www.ibo.org/) for supporting the thesis project.
- [Anton Béguin](https://orcid.org/0000-0003-2882-4485) for supervising the thesis.
