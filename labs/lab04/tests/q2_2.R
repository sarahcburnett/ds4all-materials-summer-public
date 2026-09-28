# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      name = "q2_2a",
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
        .t_check(exists("mpg_with_zeros") && !is.null(mpg_with_zeros),
                 "`mpg_with_zeros` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(mpg_with_zeros),
                 "`mpg_with_zeros` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(mpg_with_zeros) && length(mpg_with_zeros) == 1,
                 "`mpg_with_zeros` should be a single number.")
        .t_check(!is.na(mpg_with_zeros),
                 "`mpg_with_zeros` is NA. Use `cars_raw`, where the 0s are still numbers rather than NA.")
        .t_check(!.t_match(.t_num(mpg_with_zeros), "7dead44581531453a4d90a4ecfd804905f756e6a7329c54ffbfb0a669023e9df"),
                 "This matches the average with the 0s left out. Use `cars_raw`, where the 0s are still in the MPG column.")
        .t_check(.t_match(.t_num(mpg_with_zeros), "0899153c9b16aaa787b3edb6d16b53c481f5341b26f22d08f815f4b14e463b24"),
                 "Not the right value yet. Average the MPG column of `cars_raw`.")
      }
    ),
    ottr::TestCase$new(
      name = "q2_2b",
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
        .t_check(exists("mpg_without_zeros") && !is.null(mpg_without_zeros),
                 "`mpg_without_zeros` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(mpg_without_zeros),
                 "`mpg_without_zeros` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(mpg_without_zeros) && length(mpg_without_zeros) == 1,
                 "`mpg_without_zeros` should be a single number.")
        .t_check(!is.na(mpg_without_zeros),
                 "`mpg_without_zeros` is NA because `cars_clean` has missing values. Check what na.rm = TRUE does in mean().")
        .t_check(!.t_match(.t_num(mpg_without_zeros), "0899153c9b16aaa787b3edb6d16b53c481f5341b26f22d08f815f4b14e463b24"),
                 "This matches the average with the fake 0s still counted. Use `cars_clean`.")
        .t_check(.t_match(.t_num(mpg_without_zeros), "7dead44581531453a4d90a4ecfd804905f756e6a7329c54ffbfb0a669023e9df"),
                 "Not the right value yet. Average the MPG column of `cars_clean`, leaving out the NAs.")
      }
    )
  )
)
