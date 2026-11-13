# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q5_2",
  cases = list(
    ottr::TestCase$new(
      name = "q5_2a",
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
        .t_check(exists("below_3_slope") && exists("below_3_intercept") && exists("above_3_slope") && exists("above_3_intercept") &&
                   is.numeric(below_3_slope) && length(below_3_slope) == 1 && .t_match(.t_num(below_3_slope, 4), "fd9b536db74993c68881d221408c872ab52d469d2e871af2f17a64b52b39fb7a") &&
                   is.numeric(below_3_intercept) && length(below_3_intercept) == 1 && .t_match(.t_num(below_3_intercept, 4), "69fa7566371c883171e36b033ff4f1596cb901b43dfb6f22e6ef4cc73211197a") &&
                   is.numeric(above_3_slope) && length(above_3_slope) == 1 && .t_match(.t_num(above_3_slope, 4), "ac7d153014702dbf28833d230df317aedead411edf354885fbe8c6dd8732001d") &&
                   is.numeric(above_3_intercept) && length(above_3_intercept) == 1 && .t_match(.t_num(above_3_intercept, 4), "35b31c88d18a313bc09d2003ca90134b9c7e83226f45048c9d5eba9abc0b8bf2"),
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
        .t_check(.t_match(paste(.t_num(.p, 2), collapse = ","), "4904728319d5416361f8d3c801948af83a641a82dd99c222350afa4b9d96764d"),
                 "Not the right values yet. Below 3 minutes, use `below_3_slope` and `below_3_intercept`. Otherwise, use `above_3_slope` and `above_3_intercept`.")
      }
    ),
    ottr::TestCase$new(
      name = "q5_2b",
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
        .t_check(exists("accuracy_answer") && !is.null(accuracy_answer),
                 "`accuracy_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(accuracy_answer) && length(accuracy_answer) == 1,
                 "Set `accuracy_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(accuracy_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `accuracy_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "559aead08264d5795d3909718cdd05abd49572e84fe55590eef31a88a08fdffd"),
                 "Not quite. Each of the two lines only has to fit its own cluster. In the plot above, how closely do the red predictions follow the actual waits?")
      }
    )
  )
)
