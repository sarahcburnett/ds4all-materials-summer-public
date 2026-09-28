# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
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
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("best_mpg") && !is.null(best_mpg),
                 "`best_mpg` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(best_mpg),
                 "`best_mpg` should be a tibble: the top rows of `cars_clean`.")
        .t_check(all(c("Car", "MPG") %in% names(best_mpg)),
                 "`best_mpg` is missing columns. Sort and cut the whole table; you don't need select() here.")
        .t_check(!("Model" %in% names(best_mpg)),
                 "Start from `cars_clean`, not `cars_raw`.")
        .t_check(nrow(best_mpg) == 10,
                 "`best_mpg` should have exactly 10 rows. head() keeps the first rows.")
        .t_check(!(isTRUE(!is.unsorted(best_mpg$MPG)) && isTRUE(is.unsorted(rev(best_mpg$MPG)))),
                 "`best_mpg` is sorted from lowest to highest. Wrap the column in desc() to flip the order.")
        .t_check(isTRUE(!is.unsorted(rev(best_mpg$MPG))),
                 "`best_mpg` should be sorted from the highest MPG to the lowest.")
        .t_check(.t_match(paste(best_mpg$Car, collapse = "|"), "2cbfe1a273007f60037fbf6e192540e88e8c32c234e74534de10d6d6c9d8db7b"),
                 "These are not the 10 cars with the highest MPG. Sort all of `cars_clean` before you keep the top 10.")
      }
    )
  )
)
