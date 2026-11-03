# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      name = "q1_2a",
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
        .t_check(exists("one_mean") && !is.null(one_mean),
                 "`one_mean` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(one_mean),
                 "`one_mean` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(one_mean) && length(one_mean) == 1,
                 "`one_mean` should be a single number.")
        .t_check(!.t_match(.t_num(one_mean), "0d7f09b7a0706d632a985fb146c25e2276bc9edbe60a9ad554d0a9f8a9340f7f"),
                 "That's the mean of the whole population. Draw a sample of 100 with slice_sample() first, then take its mean.")
        .t_check(abs(one_mean - 113400) < 45000,
                 "That's too far from the population mean to be the mean of 100 salaries. Take the mean of the `salary` column of your sample.")
      }
    )
  )
)
