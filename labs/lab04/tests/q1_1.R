# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_1",
  cases = list(
    ottr::TestCase$new(
      name = "q1_1a",
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
        .t_check(exists("cars_raw") && !is.null(cars_raw),
                 "`cars_raw` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(cars_raw),
                 "`cars_raw` should be a tibble. Use read_delim().")
        .t_check(ncol(cars_raw) != 1,
                 "`cars_raw` has only one column, so the file was read with the wrong separator. Look at the raw lines again: what sits between the values?")
        .t_check(ncol(cars_raw) == 9,
                 "`cars_raw` should have 9 columns, one for each name in the first line of the file.")
        .t_check(nrow(cars_raw) != 407,
                 "The first row of `cars_raw` is still the row of types (STRING, DOUBLE, ...). Remove it with slice().")
        .t_check(nrow(cars_raw) == 406,
                 "`cars_raw` should have one row per car, 406 in all. Check what you gave slice().")
        .t_check(is.numeric(cars_raw$MPG),
                 "`MPG` is still text. After dropping the row of types, run type_convert() so R guesses each column's type again.")
      }
    )
  )
)
