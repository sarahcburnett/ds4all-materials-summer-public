# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      name = "q2_2a",
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
        .t_check(exists("spread_answer") && !is.null(spread_answer),
                 "`spread_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(spread_answer) && length(spread_answer) == 1,
                 "Set `spread_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(spread_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `spread_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "df7e70e5021544f4834bbee64a9e3789febc4be81470df629cad6ddb03320a5c"),
                 "Not quite. Compare the two SDs in the table above the question, then look back at the three histograms from Question 2.1. Which change moved the spread?")
      }
    )
  )
)
