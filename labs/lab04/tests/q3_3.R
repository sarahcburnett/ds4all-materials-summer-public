# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q3_3",
  cases = list(
    ottr::TestCase$new(
      name = "q3_3a",
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
        .t_check(exists("far_from_avg") && !is.null(far_from_avg),
                 "`far_from_avg` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(!is.data.frame(far_from_avg),
                 "`far_from_avg` is a table. nrow() gives the number of rows as a single number.")
        .t_check(is.numeric(far_from_avg) && length(far_from_avg) == 1,
                 "`far_from_avg` should be a single number.")
        .t_check(!is.na(far_from_avg),
                 "`far_from_avg` is NA.")
        .ans <- sprintf("%d", as.integer(round(unname(as.numeric(far_from_avg)))))
        .t_check(!.t_match(.ans, "5feceb66ffc86f38d952786c6d696c79c2dbc239dd4e91b46729d73a27fb57e9"),
                 "No cars matched. If the average is NA, every comparison with it is NA too: check na.rm = TRUE.")
        .t_check(!.t_match(.ans, "8241649609f88ccd2a0a5b233a07a538ec313ff6adf695aa44a969dbca39f67d"),
                 "That count includes the cars whose MPG was a fake 0. Use `cars_clean`.")
        .t_check(!.t_match(.ans, "98010bd9270f9b100b6214a21754fd33bdc8d41b2bc9f9dd16ff54d3c34ffd71") && !.t_match(.ans, "c6f3ac57944a531490cd39902d0f777715fd005efac9a30622d5f5205e7f6894"),
                 "That only counts the cars on one side of the average. abs() lets you count both sides at once.")
        .t_check(.t_match(.ans, "5316ca1c5ddca8e6ceccfce58f3b8540e540ee22f6180fb89492904051b3d531"),
                 "Not the right count yet. Keep the cars where abs(MPG - average) is more than 10, then count the rows.")
      }
    )
  )
)
