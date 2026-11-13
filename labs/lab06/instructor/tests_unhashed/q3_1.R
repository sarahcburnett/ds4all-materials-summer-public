# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
test = list(
  name = "q3_1",
  cases = list(
    ottr::TestCase$new(
      name = "q3_1a",
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
        .t_check(exists("two_minute_wait") && !is.null(two_minute_wait),
                 "`two_minute_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(two_minute_wait),
                 "`two_minute_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(two_minute_wait) && length(two_minute_wait) == 1,
                 "`two_minute_wait` should be a single number.")
        .t_check(exists("five_minute_wait") && !is.null(five_minute_wait),
                 "`five_minute_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(five_minute_wait),
                 "`five_minute_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(five_minute_wait) && length(five_minute_wait) == 1,
                 "`five_minute_wait` should be a single number.")
        .sl <- unname(as.numeric(slope))
        .ic <- unname(as.numeric(intercept))
        .t_check(!isTRUE(all.equal(unname(as.numeric(two_minute_wait)), .ic * 2 + .sl)),
                 "Multiply the duration by `slope`, then add `intercept`. You have them the other way around.")
        .t_check(!isTRUE(all.equal(unname(as.numeric(two_minute_wait)), .sl * 2)),
                 "Don't forget to add `intercept`.")
        .t_check(.t_match(.t_num(two_minute_wait, 2), "54.93"),
                 "Not the right value yet for 2 minutes. Multiply 2 by `slope`, then add `intercept`.")
        .t_check(.t_match(.t_num(five_minute_wait, 2), "87.12"),
                 "Not the right value yet for 5 minutes. Multiply 5 by `slope`, then add `intercept`.")
      }
    )
  )
)
