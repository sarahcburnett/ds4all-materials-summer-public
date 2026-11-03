# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q4_1",
  cases = list(
    ottr::TestCase$new(
      name = "q4_1a",
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
        .t_check(exists("many_intervals") && !is.null(many_intervals),
                 "`many_intervals` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.function(many_intervals),
                 "`many_intervals` should be a function.")
        .t_check(identical(names(formals(many_intervals)), c("n", "reps")),
                 "Keep the two arguments, `n` and `reps`, in that order.")
        .r <- tryCatch(many_intervals(400, 200), error = function(e) e)
        .t_check(!inherits(.r, "error"),
                 paste("many_intervals(400, 200) gave an error:", conditionMessage(.r)))
        .t_check(is.data.frame(.r) && nrow(.r) == 200,
                 "many_intervals(400, 200) should return a tibble with 200 rows, one interval per sample. Use the argument `reps` in rep_slice_sample().")
        .t_check(all(c("lower_ci", "upper_ci") %in% names(.r)),
                 "The tibble from `many_intervals()` needs the columns `lower_ci` and `upper_ci`. Name them inside summarize().")
        .lo <- .r$lower_ci
        .hi <- .r$upper_ci
        .t_check(is.numeric(.lo) && is.numeric(.hi) && all(.lo < .hi),
                 "Every `lower_ci` should be a number below its `upper_ci`.")
        .ctr <- (.lo + .hi) / 2
        .w <- .hi - .lo
        .t_check(sd(.ctr) > 1,
                 "Every interval has the same center. Center each interval on the mean of its own sample, mean(salary).")
        .t_check(sd(.w) > 1,
                 "Every interval has the same width. Use the SD of each sample, sd(salary), not the population SD.")
        .t_check(abs(mean(.ctr) - 113400) < 2500,
                 "The intervals should center near the population mean. Center each one on mean(salary).")
        .t_check(mean(.w) > 14000 && mean(.w) < 16700,
                 "For samples of 400 the intervals are the wrong width. Each one should go 1.96 SEs each way, with SE = sd(salary) / sqrt(n).")
        .r2 <- many_intervals(100, 200)
        .t_check(sd((.r2$lower_ci + .r2$upper_ci) / 2) > 6000,
                 "many_intervals(100, 200) should draw samples of 100. Use the argument `n` in rep_slice_sample().")
        .w2 <- mean(.r2$upper_ci - .r2$lower_ci)
        .t_check(.w2 > 28000 && .w2 < 33300,
                 "For samples of 100 the intervals are the wrong width. Use the argument `n` inside sqrt(n), not a fixed number.")
      }
    ),
    ottr::TestCase$new(
      name = "q4_1b",
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
        .t_check(exists("pop_mean") && is.numeric(pop_mean) && length(pop_mean) == 1 &&
          .t_match(.t_num(pop_mean), "0d7f09b7a0706d632a985fb146c25e2276bc9edbe60a9ad554d0a9f8a9340f7f"),
                 "Fix `pop_mean` in Question 1.1 first.")
        .t_check(exists("intervals") && !is.null(intervals),
                 "`intervals` does not exist yet. Fill in the blank and run the cell.")
        .t_check(is.data.frame(intervals) && nrow(intervals) == 100,
                 "`intervals` should be a tibble with 100 rows, one per interval.")
        .t_check(all(c("lower_ci", "upper_ci", "covers") %in% names(intervals)),
                 "`intervals` needs a `covers` column. Add it inside mutate().")
        .t_check(is.logical(intervals$covers) && !anyNA(intervals$covers),
                 "`covers` should be TRUE or FALSE for every interval.")
        .t_check(all(intervals$covers == (intervals$lower_ci <= pop_mean & intervals$upper_ci >= pop_mean)),
                 "`covers` should be TRUE when `pop_mean` is between `lower_ci` and `upper_ci`, and FALSE otherwise.")
        .w <- mean(intervals$upper_ci - intervals$lower_ci)
        .t_check(.w > 14000 && .w < 16700,
                 "These intervals are the wrong width for samples of 400. Make them with many_intervals(400, 100).")
        .t_check(exists("num_covered") && !is.null(num_covered),
                 "`num_covered` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(num_covered),
                 "`num_covered` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(num_covered) && length(num_covered) == 1,
                 "`num_covered` should be a single number: a count.")
        .t_check(num_covered == sum(intervals$covers),
                 "`num_covered` should count the intervals in `intervals` whose `covers` is TRUE.")
        .t_check(num_covered >= 80,
                 "Fewer than 80 of your 100 intervals contain `pop_mean`. That almost never happens with 95% intervals, so check `many_intervals()`.")
      }
    )
  )
)
