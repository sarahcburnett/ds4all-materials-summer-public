test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      name = "q1_2a",
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
          exists("top_movies") && !is.null(top_movies),
          info = "`top_movies` is still NULL."
        )
        testthat::expect_true(
          is.data.frame(top_movies),
          info = "`top_movies` should be a tibble read in with read_csv()."
        )
        check_equal(
          nrow(top_movies), 200L,
          "`top_movies` does not have the 200 rows it should. Check the path: data/top_movies_2017.csv"
        )
      }
    ),
    ottr::TestCase$new(
      name = "q1_2b",
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
        wanted <- c("title", "studio", "gross", "gross_adjusted", "year")
        missing <- setdiff(wanted, names(top_movies))
        testthat::expect_true(
          length(missing) == 0,
          info = paste(
            "Still missing after the rename:", paste(missing, collapse = ", "), ".",
            "Remember that rename() is written new_name = old_name, and that an old name",
            "with a space or a bracket in it has to be wrapped in backticks."
          )
        )
      }
    )
  )
)
