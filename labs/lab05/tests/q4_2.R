# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q4_2",
  cases = list(
    ottr::TestCase$new(
      name = "q4_2a",
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
        .t_check(exists("error_answer") && !is.null(error_answer),
                 "`error_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(error_answer) && length(error_answer) == 1,
                 "Set `error_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(error_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `error_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "3f39d5c348e5b79d06e842c114e6cc571583bbf44e4b0ebfda1a01ec05745d43"),
                 "Not quite. In your plot from Question 4.1, how often did an interval miss `pop_mean`? Each miss is a time you would reject this true claim.")
      }
    )
  )
)
