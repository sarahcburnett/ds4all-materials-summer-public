# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      name = "q2_1a",
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
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("r") && is.numeric(r) && length(r) == 1 && .t_match(.t_num(r, 4), "cef8cb90d1edb4dc53e9ad6effe900a79e38dcfd15899b608fc54ecc7732da81"),
                 "Fix `r` in Question 1.3 first.")
        .t_check(exists("slope") && !is.null(slope),
                 "`slope` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(slope),
                 "`slope` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(slope) && length(slope) == 1,
                 "`slope` should be a single number.")
        .t_check(!.t_match(.t_num(slope, 4), "250e63bace678903f37c0f0a9f73fc2f9046d5d9540bee45aa4093838f68c151"),
                 "Your ratio of SDs is upside down. Multiply r by the SD of `waiting`, divided by the SD of `eruptions`.")
        .t_check(!.t_match(.t_num(slope, 4), "bd90a707be193cf0e3684c7cd292ad51a425feb6c9b6fb80e567396dd6ac9c5a"),
                 "That's the ratio of the SDs. Multiply it by r.")
        .t_check(.t_match(.t_num(slope, 4), "3991997758c1e877c43d4018c5e6563d4397e4e3973d6b9cf2b7100f937cd5db"),
                 "Not the right value yet. The slope is r times the SD of `waiting`, divided by the SD of `eruptions`.")
      }
    ),
    ottr::TestCase$new(
      name = "q2_1b",
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
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "3991997758c1e877c43d4018c5e6563d4397e4e3973d6b9cf2b7100f937cd5db"),
                 "Fix `slope` first.")
        .t_check(exists("intercept") && !is.null(intercept),
                 "`intercept` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(intercept),
                 "`intercept` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(intercept) && length(intercept) == 1,
                 "`intercept` should be a single number.")
        .t_check(!.t_match(.t_num(intercept, 4), "54624f5e83ad19804178bc47813d913fc6021634388b4d7fe8d570b24808eac3"),
                 "The sign is flipped. The intercept is the average of `waiting` minus `slope` times the average of `eruptions`.")
        .t_check(!.t_match(.t_num(intercept, 4), "579ad982a0e13348c55de7f446d389888c9d783e77c9f2d441693ecaed5f7c49"),
                 "Multiply `slope` by the average of `eruptions` before you subtract it.")
        .t_check(.t_match(.t_num(intercept, 4), "76a29ea98e9e253f3b83bb3f8b48750a73572ebc31604ab4c336d0500724148a"),
                 "Not the right value yet. The intercept is the average of `waiting` minus `slope` times the average of `eruptions`.")
      }
    )
  )
)
