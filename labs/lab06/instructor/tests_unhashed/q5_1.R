# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
test = list(
  name = "q5_1",
  cases = list(
    ottr::TestCase$new(
      name = "q5_1a",
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
        .t_check(exists("below_3") && !is.null(below_3) && exists("above_3") && !is.null(above_3),
                 "`below_3` and `above_3` are still NULL. Replace the placeholders with your code.")
        .t_check(is.data.frame(below_3) && is.data.frame(above_3),
                 "`below_3` and `above_3` should be tibbles. Use filter() on `faithful`.")
        .t_check(all(c("eruptions", "waiting") %in% names(below_3)) && all(c("eruptions", "waiting") %in% names(above_3)),
                 "Keep the `eruptions` and `waiting` columns in both tibbles.")
        .t_check(!(nrow(below_3) > 0 && nrow(above_3) > 0 && all(below_3$eruptions > 3) && all(above_3$eruptions < 3)),
                 "`below_3` and `above_3` are swapped.")
        .t_check(nrow(below_3) > 0 && all(below_3$eruptions < 3) && nrow(below_3) == sum(faithful$eruptions < 3),
                 "`below_3` should hold every eruption shorter than 3 minutes, and only those.")
        .t_check(nrow(above_3) > 0 && all(above_3$eruptions > 3) && nrow(above_3) == sum(faithful$eruptions > 3),
                 "`above_3` should hold every eruption longer than 3 minutes, and only those.")
        .t_check(exists("below_3_r") && !is.null(below_3_r),
                 "`below_3_r` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(below_3_r),
                 "`below_3_r` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(below_3_r) && length(below_3_r) == 1,
                 "`below_3_r` should be a single number.")
        .t_check(exists("above_3_r") && !is.null(above_3_r),
                 "`above_3_r` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(above_3_r),
                 "`above_3_r` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(above_3_r) && length(above_3_r) == 1,
                 "`above_3_r` should be a single number.")
        .t_check(!.t_match(.t_num(below_3_r, 4), "0.9008") && !.t_match(.t_num(above_3_r, 4), "0.9008"),
                 "That's the correlation for all the eruptions. Use the columns of `below_3` for `below_3_r`, and of `above_3` for `above_3_r`.")
        .t_check(.t_match(.t_num(below_3_r, 4), "0.2902"),
                 "`below_3_r` should be the correlation between `eruptions` and `waiting` in `below_3`.")
        .t_check(.t_match(.t_num(above_3_r, 4), "0.3728"),
                 "`above_3_r` should be the correlation between `eruptions` and `waiting` in `above_3`.")
      }
    ),
    ottr::TestCase$new(
      name = "q5_1b",
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
        .t_check(exists("below_3_r") && is.numeric(below_3_r) && length(below_3_r) == 1 && .t_match(.t_num(below_3_r, 4), "0.2902") &&
                   exists("above_3_r") && is.numeric(above_3_r) && length(above_3_r) == 1 && .t_match(.t_num(above_3_r, 4), "0.3728"),
                 "Finish the first part of this question first: `below_3`, `above_3`, `below_3_r`, and `above_3_r`.")
        .t_check(exists("below_3_slope") && !is.null(below_3_slope),
                 "`below_3_slope` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(below_3_slope),
                 "`below_3_slope` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(below_3_slope) && length(below_3_slope) == 1,
                 "`below_3_slope` should be a single number.")
        .t_check(exists("below_3_intercept") && !is.null(below_3_intercept),
                 "`below_3_intercept` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(below_3_intercept),
                 "`below_3_intercept` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(below_3_intercept) && length(below_3_intercept) == 1,
                 "`below_3_intercept` should be a single number.")
        .t_check(!.t_match(.t_num(below_3_slope, 4), "19.7134"),
                 "Use the cluster's own correlation, `below_3_r`, not `r` from all the eruptions.")
        .t_check(!.t_match(.t_num(below_3_slope, 4), "3.4565"),
                 "Use the SDs of the columns in `below_3`, not in `faithful`.")
        .t_check(.t_match(.t_num(below_3_slope, 4), "6.3505"),
                 "`below_3_slope` should be `below_3_r` times the SD of `waiting` in `below_3`, divided by the SD of `eruptions` in `below_3`.")
        .t_check(.t_match(.t_num(below_3_intercept, 4), "41.5516"),
                 "`below_3_intercept` should be the average of `waiting` in `below_3`, minus `below_3_slope` times the average of `eruptions` in `below_3`.")
        .t_check(exists("above_3_slope") && !is.null(above_3_slope),
                 "`above_3_slope` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(above_3_slope),
                 "`above_3_slope` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(above_3_slope) && length(above_3_slope) == 1,
                 "`above_3_slope` should be a single number.")
        .t_check(exists("above_3_intercept") && !is.null(above_3_intercept),
                 "`above_3_intercept` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(above_3_intercept),
                 "`above_3_intercept` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(above_3_intercept) && length(above_3_intercept) == 1,
                 "`above_3_intercept` should be a single number.")
        .t_check(!.t_match(.t_num(above_3_slope, 4), "13.1426"),
                 "Use the cluster's own correlation, `above_3_r`, not `r` from all the eruptions.")
        .t_check(!.t_match(.t_num(above_3_slope, 4), "4.4402"),
                 "Use the SDs of the columns in `above_3`, not in `faithful`.")
        .t_check(.t_match(.t_num(above_3_slope, 4), "5.4388"),
                 "`above_3_slope` should be `above_3_r` times the SD of `waiting` in `above_3`, divided by the SD of `eruptions` in `above_3`.")
        .t_check(.t_match(.t_num(above_3_intercept, 4), "56.6490"),
                 "`above_3_intercept` should be the average of `waiting` in `above_3`, minus `above_3_slope` times the average of `eruptions` in `above_3`.")
      }
    )
  )
)
