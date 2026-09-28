# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
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
        .t_check(exists("cars_power") && !is.null(cars_power),
                 "`cars_power` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(cars_power),
                 "`cars_power` should be a tibble: `cars_clean` with one new column.")
        .t_check("power_to_weight" %in% names(cars_power),
                 "`cars_power` needs a new column named `power_to_weight`.")
        .t_check(nrow(cars_power) == 406,
                 "`cars_power` should keep all 406 cars. Add a column and sort; don't filter.")
        .t_check(!("Model" %in% names(cars_power)),
                 "Start from `cars_clean`, not `cars_raw`.")
        .t_check(all(c("Horsepower", "Weight") %in% names(cars_power)),
                 "`cars_power` should keep all the columns of `cars_clean`, plus the new one.")
        .ptw <- cars_power$power_to_weight
        .t_check(!isTRUE(all.equal(.ptw, cars_power$Horsepower / cars_power$Weight)),
                 "These numbers are horsepower per pound. The ratio is per 1,000 pounds, so multiply by 1000.")
        .t_check(!isTRUE(all.equal(.ptw, cars_power$Weight / cars_power$Horsepower * 1000)) &&
                   !isTRUE(all.equal(.ptw, cars_power$Weight / cars_power$Horsepower)),
                 "The ratio is upside down. Horsepower goes on top and weight on the bottom.")
        .t_check(isTRUE(all.equal(.ptw, cars_power$Horsepower / cars_power$Weight * 1000)),
                 "`power_to_weight` should be Horsepower / Weight * 1000.")
        .v <- .ptw[!is.na(.ptw)]
        .t_check(!(!is.unsorted(.v) && is.unsorted(rev(.v))),
                 "`cars_power` is sorted from lowest to highest. Wrap the column in desc() to flip the order.")
        .t_check(!is.unsorted(rev(.v)),
                 "`cars_power` should be sorted from the highest power-to-weight ratio to the lowest.")
      }
    )
  )
)
