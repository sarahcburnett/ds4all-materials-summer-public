test = list(
  name = "q3_3_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        if (!exists("predicted_distance_m") || is.null(predicted_distance_m)) {
          testthat::fail("Fill in `predicted_distance_m` using Newton's formula.")
        } else if (!isTRUE(all.equal(round(predicted_distance_m, 5), 1.17022))) {
          testthat::fail("Check your translation of (1/2) * G * M / R^2 * t^2 into R code.")
        } else if (!isTRUE(all.equal(round(difference, 5), 0.04022))) {
          testthat::fail("`difference` should be predicted_distance_m - estimated_distance_m.")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
