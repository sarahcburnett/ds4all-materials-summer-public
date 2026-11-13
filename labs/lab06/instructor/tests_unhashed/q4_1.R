# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
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
          identical(x, key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "10.7296") &&
                   exists("intercept") && is.numeric(intercept) && length(intercept) == 1 && .t_match(.t_num(intercept, 4), "33.4744"),
                 "Fix `slope` and `intercept` in Question 2.1 first.")
        .t_check(exists("zero_minute_wait") && !is.null(zero_minute_wait),
                 "`zero_minute_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(zero_minute_wait),
                 "`zero_minute_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(zero_minute_wait) && length(zero_minute_wait) == 1,
                 "`zero_minute_wait` should be a single number.")
        .t_check(exists("two_point_five_minute_wait") && !is.null(two_point_five_minute_wait),
                 "`two_point_five_minute_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(two_point_five_minute_wait),
                 "`two_point_five_minute_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(two_point_five_minute_wait) && length(two_point_five_minute_wait) == 1,
                 "`two_point_five_minute_wait` should be a single number.")
        .t_check(exists("hour_wait") && !is.null(hour_wait),
                 "`hour_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(hour_wait),
                 "`hour_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(hour_wait) && length(hour_wait) == 1,
                 "`hour_wait` should be a single number.")
        .t_check(.t_match(.t_num(zero_minute_wait, 2), "33.47"),
                 "Not the right value yet for 0 minutes. Put 0 into the line: `slope` times 0, plus `intercept`.")
        .t_check(.t_match(.t_num(two_point_five_minute_wait, 2), "60.30"),
                 "Not the right value yet for 2.5 minutes. Multiply 2.5 by `slope`, then add `intercept`.")
        .t_check(!.t_match(.t_num(hour_wait, 2), "44.20"),
                 "The durations are in minutes, so an hour is 60, not 1.")
        .t_check(.t_match(.t_num(hour_wait, 2), "677.25"),
                 "Not the right value yet for an hour. Multiply 60 by `slope`, then add `intercept`.")
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
          identical(x, key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("trust_answer") && !is.null(trust_answer),
                 "`trust_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(trust_answer) && length(trust_answer) == 1,
                 "Set `trust_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(trust_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `trust_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "A"),
                 "Not quite. Look at your scatter plot from Question 1.1: which of the three durations are close to eruptions in the data?")
      }
    )
  )
)
