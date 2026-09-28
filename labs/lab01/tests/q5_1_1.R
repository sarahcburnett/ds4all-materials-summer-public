test = list(
  name = "q5_1_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        if (!exists("min_length_difference") || is.null(min_length_difference)) {
          testthat::fail("Fill in `min_length_difference`.")
        } else if (!isTRUE(all.equal(round(min_length_difference, 5), 3.9))) {
          testthat::fail("Not quite. Find the smallest of the three pairwise absolute differences.")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
