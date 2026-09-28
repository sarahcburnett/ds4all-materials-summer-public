test = list(
  name = "q4_1_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        if (!exists("num_avenues_away") || is.null(num_avenues_away)) {
          testthat::fail("Fill in `num_avenues_away` using the abs() function.")
        } else if (isTRUE(all.equal(num_avenues_away, -3))) {
          testthat::fail("Use abs() so the number of avenues away is positive.")
        } else if (!isTRUE(all.equal(num_avenues_away, 3))) {
          testthat::fail("Check `num_avenues_away`: it should be abs(7 - 10).")
        } else if (!isTRUE(all.equal(manhattan_distance, 1462))) {
          testthat::fail("`manhattan_distance` isn't right yet. Recheck num_avenues_away.")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
