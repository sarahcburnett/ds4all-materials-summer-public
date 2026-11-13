# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q4_1",
  cases = list(
    ottr::TestCase$new(
      name = "q4_1a",
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
        .t_check(exists("zero_minute_wait") && !is.null(zero_minute_wait),
                 "`zero_minute_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(zero_minute_wait),
                 "`zero_minute_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(zero_minute_wait) && length(zero_minute_wait) == 1,
                 "`zero_minute_wait` should be a single number.")
        .t_check(exists("two_point_five_minute_wait") && !is.null(two_point_five_minute_wait),
                 "`two_point_five_minute_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(two_point_five_minute_wait),
                 "`two_point_five_minute_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(two_point_five_minute_wait) && length(two_point_five_minute_wait) == 1,
                 "`two_point_five_minute_wait` should be a single number.")
        .t_check(exists("hour_wait") && !is.null(hour_wait),
                 "`hour_wait` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(hour_wait),
                 "`hour_wait` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(hour_wait) && length(hour_wait) == 1,
                 "`hour_wait` should be a single number.")
        .t_check(.t_match(.t_num(zero_minute_wait, 2), "7d0a2910af4af206a44dd67278826d0f3382152b194ca6def4c5202e64729962"),
                 "Not the right value yet for 0 minutes. Put 0 into the line: `slope` times 0, plus `intercept`.")
        .t_check(.t_match(.t_num(two_point_five_minute_wait, 2), "2319485e34835197d0d0485bf9c671e70391f194caf7d8cbadce2d8453d1846c"),
                 "Not the right value yet for 2.5 minutes. Multiply 2.5 by `slope`, then add `intercept`.")
        .t_check(!.t_match(.t_num(hour_wait, 2), "a27807619597ef2e84a60049e6616fbb7e9d60d0f2193bff95680c2391b547ad"),
                 "The durations are in minutes, so an hour is 60, not 1.")
        .t_check(.t_match(.t_num(hour_wait, 2), "21e88c9e7fdc70563990b7cf116f338dec000a7f49db22c2f09f48f1fe7fa65a"),
                 "Not the right value yet for an hour. Multiply 60 by `slope`, then add `intercept`.")
      }
    ),
    ottr::TestCase$new(
      name = "q4_1b",
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
        .t_check(exists("trust_answer") && !is.null(trust_answer),
                 "`trust_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(trust_answer) && length(trust_answer) == 1,
                 "Set `trust_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(trust_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `trust_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "559aead08264d5795d3909718cdd05abd49572e84fe55590eef31a88a08fdffd"),
                 "Not quite. Look at your scatter plot from Question 1.1: which of the three durations are close to eruptions in the data?")
      }
    )
  )
)
