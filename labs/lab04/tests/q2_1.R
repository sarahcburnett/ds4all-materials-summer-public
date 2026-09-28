# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
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
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("cars_clean") && !is.null(cars_clean),
                 "`cars_clean` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(cars_clean),
                 "`cars_clean` should be a tibble.")
        .t_check(nrow(cars_clean) == 406,
                 "`cars_clean` should keep all 406 cars. Replace the 0s with NA instead of filtering those cars out.")
        .t_check("Year" %in% names(cars_clean) && !("Model" %in% names(cars_clean)),
                 "`cars_clean` should have a `Year` column and no `Model` column. Rename `Model` to `Year`.")
        .t_check(!any(cars_clean$Year < 1900, na.rm = TRUE),
                 "`Year` still holds two-digit years like 70. Turn them into full years like 1970.")
        .t_check(isTRUE(all(cars_clean$Year >= 1970 & cars_clean$Year <= 1982)),
                 "`Year` should run from 1970 to 1982.")
        .t_check(is.integer(cars_clean$Year),
                 "`Year` holds whole numbers, so store it as an integer with as.integer().")
        .t_check(!any(cars_clean$MPG == 0, na.rm = TRUE),
                 "`MPG` still has 0s in it. Replace them with NA using na_if().")
        .t_check(sum(is.na(cars_clean$MPG)) == 8,
                 "`MPG` should have exactly 8 NAs, one for each car that had a 0. Only the 0s should change.")
        .t_check(!any(cars_clean$Horsepower == 0, na.rm = TRUE),
                 "`Horsepower` still has 0s in it. Replace them with NA, the same way you did for `MPG`.")
        .t_check(sum(is.na(cars_clean$Horsepower)) == 6,
                 "`Horsepower` should have exactly 6 NAs. Only the 0s should change.")
      }
    )
  )
)
