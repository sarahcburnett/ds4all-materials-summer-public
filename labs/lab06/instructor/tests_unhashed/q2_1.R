# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      name = "q2_1a",
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
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("r") && is.numeric(r) && length(r) == 1 && .t_match(.t_num(r, 4), "0.9008"),
                 "Fix `r` in Question 1.3 first.")
        .t_check(exists("slope") && !is.null(slope),
                 "`slope` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(slope),
                 "`slope` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(slope) && length(slope) == 1,
                 "`slope` should be a single number.")
        .t_check(!.t_match(.t_num(slope, 4), "0.0756"),
                 "Your ratio of SDs is upside down. Multiply r by the SD of `waiting`, divided by the SD of `eruptions`.")
        .t_check(!.t_match(.t_num(slope, 4), "11.9111"),
                 "That's the ratio of the SDs. Multiply it by r.")
        .t_check(.t_match(.t_num(slope, 4), "10.7296"),
                 "Not the right value yet. The slope is r times the SD of `waiting`, divided by the SD of `eruptions`.")
      }
    ),
    ottr::TestCase$new(
      name = "q2_1b",
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
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "10.7296"),
                 "Fix `slope` first.")
        .t_check(exists("intercept") && !is.null(intercept),
                 "`intercept` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(intercept),
                 "`intercept` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(intercept) && length(intercept) == 1,
                 "`intercept` should be a single number.")
        .t_check(!.t_match(.t_num(intercept, 4), "-33.4744"),
                 "The sign is flipped. The intercept is the average of `waiting` minus `slope` times the average of `eruptions`.")
        .t_check(!.t_match(.t_num(intercept, 4), "60.1674"),
                 "Multiply `slope` by the average of `eruptions` before you subtract it.")
        .t_check(.t_match(.t_num(intercept, 4), "33.4744"),
                 "Not the right value yet. The intercept is the average of `waiting` minus `slope` times the average of `eruptions`.")
      }
    )
  )
)
