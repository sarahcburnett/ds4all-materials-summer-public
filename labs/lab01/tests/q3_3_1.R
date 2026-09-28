test = list(
  name = "q3_3_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        if (!exists("time") || is.null(time)) {
          testthat::fail("Fill in `time` with the duration of the fall, in seconds.")
        } else if (!isTRUE(all.equal(round(time, 5), 1.2))) {
          testthat::fail("Check `time`: the hammer falls for 1.2 seconds (36 frames).")
        } else if (!exists("estimated_distance_m") || is.null(estimated_distance_m)) {
          testthat::fail("Fill in `estimated_distance_m`, the distance the hammer fell in meters.")
        } else if (isTRUE(all.equal(estimated_distance_m, 113))) {
          testthat::fail("`estimated_distance_m` should be in meters, not centimeters.")
        } else if (!isTRUE(all.equal(round(estimated_distance_m, 5), 1.13))) {
          testthat::fail("Check `estimated_distance_m`: convert 113 cm to meters.")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
