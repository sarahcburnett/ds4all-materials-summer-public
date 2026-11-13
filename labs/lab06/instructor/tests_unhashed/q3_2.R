# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
test = list(
  name = "q3_2",
  cases = list(
    ottr::TestCase$new(
      name = "q3_2a",
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
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "10.7296") &&
                   exists("intercept") && is.numeric(intercept) && length(intercept) == 1 && .t_match(.t_num(intercept, 4), "33.4744"),
                 "Fix `slope` and `intercept` in Question 2.1 first.")
        .t_check(exists("faithful_predictions") && !is.null(faithful_predictions),
                 "`faithful_predictions` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(faithful_predictions) && nrow(faithful_predictions) == 272,
                 "`faithful_predictions` should have one row per eruption, 272 rows. Start the pipeline with `faithful`.")
        .t_check(all(c("eruptions", "waiting", "predicted_wait", "residual") %in% names(faithful_predictions)),
                 "`faithful_predictions` needs the columns `predicted_wait` and `residual`, added with mutate().")
        .p <- unname(as.numeric(faithful_predictions$predicted_wait))
        .t_check(length(.p) == 272 && !anyNA(.p),
                 "`predicted_wait` should hold a number for every eruption.")
        .t_check(isTRUE(all.equal(.p, unname(as.numeric(slope * faithful_predictions$eruptions + intercept)))),
                 "`predicted_wait` should be `slope` times each value of `eruptions`, plus `intercept`.")
        .res <- unname(as.numeric(faithful_predictions$residual))
        .t_check(!isTRUE(all.equal(.res, .p - faithful_predictions$waiting)),
                 "The residual is the actual wait minus the predicted wait. Yours is the other way around.")
        .t_check(isTRUE(all.equal(.res, faithful_predictions$waiting - .p)),
                 "`residual` should be `waiting` minus `predicted_wait`.")
      }
    ),
    ottr::TestCase$new(
      name = "q3_2b",
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
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "10.7296") &&
                   exists("intercept") && is.numeric(intercept) && length(intercept) == 1 && .t_match(.t_num(intercept, 4), "33.4744"),
                 "Fix `slope` and `intercept` in Question 2.1 first.")
        .t_check(exists("faithful_predictions") && is.data.frame(faithful_predictions) && nrow(faithful_predictions) == 272 &&
                   all(c("eruptions", "waiting", "residual") %in% names(faithful_predictions)) &&
                   isTRUE(all.equal(unname(as.numeric(faithful_predictions$residual)),
                                    unname(as.numeric(faithful_predictions$waiting - (slope * faithful_predictions$eruptions + intercept))))),
                 "Finish `faithful_predictions` first, in the first part of this question.")
        .t_check(exists("faithful_rmse") && !is.null(faithful_rmse),
                 "`faithful_rmse` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(faithful_rmse),
                 "`faithful_rmse` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(faithful_rmse) && length(faithful_rmse) == 1,
                 "`faithful_rmse` should be a single number.")
        .t_check(abs(faithful_rmse) > 0.01,
                 "Square the residuals before you take their mean. The residuals themselves add up to 0.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "34.7183"),
                 "That's the mean squared error. Take its square root.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "97.1771"),
                 "Take the mean of the squared residuals, not their sum.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "4.7787"),
                 "That's the average size of the residuals. The RMSE squares them first, then takes the square root of their mean.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "5.9031"),
                 "Close, but sd() divides by n - 1. The RMSE takes the mean of the squared residuals, which divides by n.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "5.9140"),
                 "Close, but sigma() divides by n - 2. Square the residuals, take their mean, then take the square root.")
        .t_check(.t_match(.t_num(faithful_rmse, 4), "5.8922"),
                 "Not the right value yet. Square the residuals, take their mean, then take the square root.")
      }
    )
  )
)
