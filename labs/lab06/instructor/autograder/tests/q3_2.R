# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q3_2",
  cases = list(
    ottr::TestCase$new(
      name = "q3_2a",
      hidden = FALSE,
      points = 5,
      code = {
        .t_check <- function(ok, hint) {
          if (isTRUE(ok)) testthat::succeed() else testthat::fail(hint)
        }
        .t_match <- function(x, key) {
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "3991997758c1e877c43d4018c5e6563d4397e4e3973d6b9cf2b7100f937cd5db") &&
                   exists("intercept") && is.numeric(intercept) && length(intercept) == 1 && .t_match(.t_num(intercept, 4), "76a29ea98e9e253f3b83bb3f8b48750a73572ebc31604ab4c336d0500724148a"),
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
      points = 5,
      code = {
        .t_check <- function(ok, hint) {
          if (isTRUE(ok)) testthat::succeed() else testthat::fail(hint)
        }
        .t_match <- function(x, key) {
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "3991997758c1e877c43d4018c5e6563d4397e4e3973d6b9cf2b7100f937cd5db") &&
                   exists("intercept") && is.numeric(intercept) && length(intercept) == 1 && .t_match(.t_num(intercept, 4), "76a29ea98e9e253f3b83bb3f8b48750a73572ebc31604ab4c336d0500724148a"),
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
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "1f5459326cbc98d41a8876e93be4b1820179eb397e356d9b0d0c425b2873f2b1"),
                 "That's the mean squared error. Take its square root.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "7caf2b6d2a1021f703a691c352b42776e519d8408eae3d67cf6f653b5d4ad860"),
                 "Take the mean of the squared residuals, not their sum.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "d205192652057f7c1321a671d9719201c1c34698bb214fae8debea187a4055e7"),
                 "That's the average size of the residuals. The RMSE squares them first, then takes the square root of their mean.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "00d8eabd146ee99f685fb48cd58aa3362598b2095b8ba5adbe6c7062a7fbfbd9"),
                 "Close, but sd() divides by n - 1. The RMSE takes the mean of the squared residuals, which divides by n.")
        .t_check(!.t_match(.t_num(faithful_rmse, 4), "409494d933b4b1192ee5e9a5ac3b0094cc6b5e30b34d32914e3bcdd59760940f"),
                 "Close, but sigma() divides by n - 2. Square the residuals, take their mean, then take the square root.")
        .t_check(.t_match(.t_num(faithful_rmse, 4), "c0e6017a27773419ce3561888ec3ebcf708b7a7f6ff24f8aabb269a4741d194e"),
                 "Not the right value yet. Square the residuals, take their mean, then take the square root.")
      }
    )
  )
)
