# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q3_2",
  cases = list(
    ottr::TestCase$new(
      name = "q3_2a",
      hidden = FALSE,
      points = 1,
      code = {
        .t_check <- function(ok, hint) {
          if (isTRUE(ok)) testthat::succeed() else testthat::fail(hint)
        }
        .t_match <- function(x, key) {
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("boot_dist") && is.data.frame(boot_dist) && "stat" %in% names(boot_dist),
                 "Finish Question 3.1 first: `boot_dist` should be a tibble with a `stat` column.")
        .t_check(exists("ci_percentile") && !is.null(ci_percentile),
                 "`ci_percentile` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(ci_percentile),
                 "`ci_percentile` should be a tibble.")
        .t_check(identical(names(ci_percentile), c("lower_ci", "upper_ci")),
                 "`ci_percentile` should have two columns, `lower_ci` and `upper_ci`, in that order.")
        .t_check(nrow(ci_percentile) == 1,
                 "`ci_percentile` should have one row.")
        .v <- unname(as.numeric(unlist(ci_percentile[1, ])))
        .t_check(!isTRUE(all.equal(.v, unname(quantile(boot_dist$stat, c(0.05, 0.95))))),
                 "That's a 90% interval: it cuts off 5% on each side. A 95% interval cuts off 2.5% on each side.")
        .t_check(isTRUE(all.equal(.v, unname(quantile(boot_dist$stat, c(0.025, 0.975))))),
                 "`ci_percentile` should hold the 2.5th and 97.5th percentiles of the `stat` column of `boot_dist`.")
      }
    ),
    ottr::TestCase$new(
      name = "q3_2b",
      hidden = FALSE,
      points = 1,
      code = {
        .t_check <- function(ok, hint) {
          if (isTRUE(ok)) testthat::succeed() else testthat::fail(hint)
        }
        .t_match <- function(x, key) {
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("our_sample") && is.data.frame(our_sample) && nrow(our_sample) == 400 && "salary" %in% names(our_sample),
                 "`our_sample` should be a tibble of 400 salaries. Rerun the cell at the start of Part 3 that creates it.")
        .t_check(exists("formula_se") && is.numeric(formula_se) && length(formula_se) == 1,
                 "Finish Question 3.1 first: `formula_se` should be a single number.")
        .t_check(exists("ci_formula") && !is.null(ci_formula),
                 "`ci_formula` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(ci_formula),
                 "`ci_formula` should be a tibble.")
        .t_check(identical(names(ci_formula), c("lower_ci", "upper_ci")),
                 "`ci_formula` should have two columns, `lower_ci` and `upper_ci`, in that order.")
        .t_check(nrow(ci_formula) == 1,
                 "`ci_formula` should have one row.")
        .v <- unname(as.numeric(unlist(ci_formula[1, ])))
        .m <- mean(our_sample$salary)
        .se <- sd(our_sample$salary) / sqrt(nrow(our_sample))
        .t_check(isTRUE(.v[1] < .v[2]),
                 "`lower_ci` should be smaller than `upper_ci`.")
        .t_check(!(exists("boot_se") && is.numeric(boot_se) && length(boot_se) == 1 &&
                   isTRUE(all(abs(.v - c(.m - 1.96 * boot_se, .m + 1.96 * boot_se)) < 0.01))),
                 "This interval uses `boot_se`. The formula interval uses `formula_se`.")
        .t_check(!isTRUE(all(abs(.v - c(.m - .se, .m + .se)) < 1)),
                 "This interval goes one SE each way. A 95% interval goes 1.96 SEs each way.")
        .t_check(isTRUE(all(abs(.v - c(.m - 1.96 * .se, .m + 1.96 * .se)) < 1)),
                 "`ci_formula` should be the sample mean minus and plus 1.96 times `formula_se`.")
      }
    )
  )
)
