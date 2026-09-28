test = list(
  name = "q51",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        if (!exists("genghis_distance_from_average_in") || is.null(genghis_distance_from_average_in)) {
          testthat::fail("Fill in `genghis_distance_from_average_in` using the abs() function.")
        } else if (!isTRUE(all.equal(round(genghis_distance_from_average_in, 3), 1.5))) {
          testthat::fail("Not quite. Compute abs(16.7 - 18.2).")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
