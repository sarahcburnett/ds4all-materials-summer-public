# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q4_3",
  cases = list(
    ottr::TestCase$new(
      name = "q4_3a",
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
        .t_check(exists("intervals_100") && is.data.frame(intervals_100) && nrow(intervals_100) == 1000 &&
                   exists("intervals_625") && is.data.frame(intervals_625) && nrow(intervals_625) == 1000,
                 "Run the cell above the question that makes `intervals_100` and `intervals_625`, 1,000 intervals each.")
        .t_check(exists("reject_rate_100") && !is.null(reject_rate_100),
                 "`reject_rate_100` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(reject_rate_100),
                 "`reject_rate_100` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(reject_rate_100) && length(reject_rate_100) == 1,
                 "`reject_rate_100` should be a single number.")
        .t_check(reject_rate_100 >= 0 && reject_rate_100 <= 1,
                 "`reject_rate_100` should be a proportion between 0 and 1, not a count.")
        .e <- mean(intervals_100$lower_ci > 125000 | intervals_100$upper_ci < 125000)
        .t_check(!isTRUE(all.equal(unname(as.numeric(reject_rate_100)), 1 - .e)),
                 "That's the share of intervals that contain $125,000. You want the share that reject it: the claim is below `lower_ci` or above `upper_ci`.")
        .t_check(isTRUE(all.equal(unname(as.numeric(reject_rate_100)), .e)),
                 "`reject_rate_100` should be the share of intervals in `intervals_100` that reject the claim: `claim` is below `lower_ci` or above `upper_ci`.")
        .t_check(exists("reject_rate_625") && !is.null(reject_rate_625),
                 "`reject_rate_625` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(reject_rate_625),
                 "`reject_rate_625` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(reject_rate_625) && length(reject_rate_625) == 1,
                 "`reject_rate_625` should be a single number.")
        .t_check(reject_rate_625 >= 0 && reject_rate_625 <= 1,
                 "`reject_rate_625` should be a proportion between 0 and 1, not a count.")
        .e <- mean(intervals_625$lower_ci > 125000 | intervals_625$upper_ci < 125000)
        .t_check(!isTRUE(all.equal(unname(as.numeric(reject_rate_625)), 1 - .e)),
                 "That's the share of intervals that contain $125,000. You want the share that reject it: the claim is below `lower_ci` or above `upper_ci`.")
        .t_check(isTRUE(all.equal(unname(as.numeric(reject_rate_625)), .e)),
                 "`reject_rate_625` should be the share of intervals in `intervals_625` that reject the claim: `claim` is below `lower_ci` or above `upper_ci`.")
      }
    ),
    ottr::TestCase$new(
      name = "q4_3b",
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
        .t_check(exists("why_answer") && !is.null(why_answer),
                 "`why_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(why_answer) && length(why_answer) == 1,
                 "Set `why_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(why_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `why_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "559aead08264d5795d3909718cdd05abd49572e84fe55590eef31a88a08fdffd"),
                 "Not quite. Compare the widths of the intervals in `intervals_100` and `intervals_625`, and think back to Part 2.")
      }
    )
  )
)
