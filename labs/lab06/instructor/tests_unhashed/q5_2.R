# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
test = list(
  name = "q5_2",
  cases = list(
    ottr::TestCase$new(
      name = "q5_2a",
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
        .t_check(exists("below_3_slope") && exists("below_3_intercept") && exists("above_3_slope") && exists("above_3_intercept") &&
                   is.numeric(below_3_slope) && length(below_3_slope) == 1 && .t_match(.t_num(below_3_slope, 4), "6.3505") &&
                   is.numeric(below_3_intercept) && length(below_3_intercept) == 1 && .t_match(.t_num(below_3_intercept, 4), "41.5516") &&
                   is.numeric(above_3_slope) && length(above_3_slope) == 1 && .t_match(.t_num(above_3_slope, 4), "5.4388") &&
                   is.numeric(above_3_intercept) && length(above_3_intercept) == 1 && .t_match(.t_num(above_3_intercept, 4), "56.6490"),
                 "Finish Question 5.1 first: the slopes and intercepts of both clusters' lines.")
        .t_check(exists("predict_wait") && is.function(predict_wait),
                 "`predict_wait` should be a function. Fill in the blanks and run the cell.")
        .d <- c(1.5, 2.5, 3.5, 4.5)
        .p <- tryCatch(predict_wait(.d), error = function(e) e)
        .t_check(!inherits(.p, "error"),
                 paste0("predict_wait(c(1.5, 2.5, 3.5, 4.5)) gave an error: ", conditionMessage(.p),
                        " Use case_when(), which works on a whole vector at once, instead of if."))
        .t_check(is.numeric(.p) && length(.p) == 4,
                 "predict_wait() should return one prediction for each duration it gets. Use case_when(), which works on a whole vector at once.")
        .p <- unname(as.numeric(.p))
        .t_check(!(exists("slope") && exists("intercept") && is.numeric(slope) && is.numeric(intercept) &&
                   length(slope) == 1 && length(intercept) == 1 &&
                   isTRUE(all.equal(.p, unname(as.numeric(slope * .d + intercept))))),
                 "Those predictions come from the single line in Part 2. Use the `below_3` line for durations below 3, and the `above_3` line otherwise.")
        .sw <- ifelse(.d < 3, above_3_slope * .d + above_3_intercept, below_3_slope * .d + below_3_intercept)
        .t_check(!isTRUE(all.equal(.p, unname(as.numeric(.sw)))),
                 "The two lines are swapped. Durations below 3 should use the `below_3` line.")
        .t_check(.t_match(paste(.t_num(.p, 2), collapse = ","), "51.08,57.43,75.68,81.12"),
                 "Not the right values yet. Below 3 minutes, use `below_3_slope` and `below_3_intercept`. Otherwise, use `above_3_slope` and `above_3_intercept`.")
      }
    ),
    ottr::TestCase$new(
      name = "q5_2b",
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
        .t_check(exists("accuracy_answer") && !is.null(accuracy_answer),
                 "`accuracy_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(accuracy_answer) && length(accuracy_answer) == 1,
                 "Set `accuracy_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(accuracy_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `accuracy_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "A"),
                 "Not quite. Each of the two lines only has to fit its own cluster. In the plot above, how closely do the red predictions follow the actual waits?")
      }
    )
  )
)
