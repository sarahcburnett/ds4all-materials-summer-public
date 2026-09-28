test = list(
  name = "q1_1",
  cases = list(
    ottr::TestCase$new(
      name = "q1_1a",
      hidden = FALSE,
      points = 1,
      code = {
        check_equal <- function(actual, expected, hint) {
          if (isTRUE(all.equal(actual, expected))) {
            testthat::succeed()
          } else {
            testthat::fail(hint)
          }
        }
        testthat::expect_true(
          exists("baby") && !is.null(baby),
          info = "`baby` is still NULL. Replace the placeholder with a read_csv() call."
        )
        testthat::expect_true(
          is.data.frame(baby),
          info = "`baby` should be a tibble. Use read_csv(), not readLines()."
        )
        check_equal(
          nrow(baby), 1174L,
          "`baby` does not have the 1174 rows it should. Check the path you gave read_csv(): the file is data/baby.csv"
        )
        check_equal(
          ncol(baby), 6L,
          "`baby` should have 6 columns. If you have only one, the file was read with the wrong separator."
        )
      }
    )
  )
)
