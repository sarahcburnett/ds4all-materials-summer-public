# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
test = list(
  name = "q1_0",
  cases = list(
    ottr::TestCase$new(
      name = "q1_0a",
      hidden = FALSE,
      points = 1,
      code = {
        .t_check <- function(ok, hint) {
          if (isTRUE(ok)) testthat::succeed() else testthat::fail(hint)
        }
        .t_match <- function(x, key) {
          identical(x, key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("least_squares_order") && !is.null(least_squares_order),
                 "`least_squares_order` is still NULL. Replace the placeholder with your code.")
        .t_check(is.numeric(least_squares_order) && length(least_squares_order) == 4,
                 "`least_squares_order` should be a vector of 4 step numbers, made with c().")
        .t_check(setequal(least_squares_order, 1:4) && !anyDuplicated(least_squares_order),
                 "Use each step number, 1 to 4, exactly once.")
        .t_check(least_squares_order[1] == 4,
                 "Which step tells you whether linear regression makes sense at all? That one comes first.")
        .t_check(.t_match(paste(least_squares_order, collapse = ","), "4,1,3,2"),
                 "Not quite. Which steps need another step's results? Predictions need the line, and the residuals and the RMSE need predictions.")
      }
    )
  )
)
