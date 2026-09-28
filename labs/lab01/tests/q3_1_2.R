test = list(
  name = "q3_1_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        if (!exists("seconds_in_a_decade") || is.null(seconds_in_a_decade)) {
          testthat::fail("You haven't changed seconds_in_a_decade from NULL yet.")
        } else if (isTRUE(all.equal(seconds_in_a_decade, 315360000))) {
          testthat::fail("So close! It looks like you forgot to include the two leap years.")
        } else if (!isTRUE(all.equal(seconds_in_a_decade, 315532800))) {
          testthat::fail("That's not the right number of seconds. Remember: 8 regular years (365 days) and 2 leap years (366 days).")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
