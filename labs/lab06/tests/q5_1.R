# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q5_1",
  cases = list(
    ottr::TestCase$new(
      name = "q5_1a",
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
        .t_check(exists("below_3") && !is.null(below_3) && exists("above_3") && !is.null(above_3),
                 "`below_3` and `above_3` are still NULL. Replace the placeholders with your code.")
        .t_check(is.data.frame(below_3) && is.data.frame(above_3),
                 "`below_3` and `above_3` should be tibbles. Use filter() on `faithful`.")
        .t_check(all(c("eruptions", "waiting") %in% names(below_3)) && all(c("eruptions", "waiting") %in% names(above_3)),
                 "Keep the `eruptions` and `waiting` columns in both tibbles.")
        .t_check(!(nrow(below_3) > 0 && nrow(above_3) > 0 && all(below_3$eruptions > 3) && all(above_3$eruptions < 3)),
                 "`below_3` and `above_3` are swapped.")
        .t_check(nrow(below_3) > 0 && all(below_3$eruptions < 3) && nrow(below_3) == sum(faithful$eruptions < 3),
                 "`below_3` should hold every eruption shorter than 3 minutes, and only those.")
        .t_check(nrow(above_3) > 0 && all(above_3$eruptions > 3) && nrow(above_3) == sum(faithful$eruptions > 3),
                 "`above_3` should hold every eruption longer than 3 minutes, and only those.")
        .t_check(exists("below_3_r") && !is.null(below_3_r),
                 "`below_3_r` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(below_3_r),
                 "`below_3_r` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(below_3_r) && length(below_3_r) == 1,
                 "`below_3_r` should be a single number.")
        .t_check(exists("above_3_r") && !is.null(above_3_r),
                 "`above_3_r` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(above_3_r),
                 "`above_3_r` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(above_3_r) && length(above_3_r) == 1,
                 "`above_3_r` should be a single number.")
        .t_check(!.t_match(.t_num(below_3_r, 4), "cef8cb90d1edb4dc53e9ad6effe900a79e38dcfd15899b608fc54ecc7732da81") && !.t_match(.t_num(above_3_r, 4), "cef8cb90d1edb4dc53e9ad6effe900a79e38dcfd15899b608fc54ecc7732da81"),
                 "That's the correlation for all the eruptions. Use the columns of `below_3` for `below_3_r`, and of `above_3` for `above_3_r`.")
        .t_check(.t_match(.t_num(below_3_r, 4), "97c8c0726b9c89df539a3af8308d29a7638f453e344761c8ea5e62f5ff23010f"),
                 "`below_3_r` should be the correlation between `eruptions` and `waiting` in `below_3`.")
        .t_check(.t_match(.t_num(above_3_r, 4), "17840c8f072d77a8a0933b859da4fb5a236157574071cd1222571ae19808eba5"),
                 "`above_3_r` should be the correlation between `eruptions` and `waiting` in `above_3`.")
      }
    ),
    ottr::TestCase$new(
      name = "q5_1b",
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
        .t_check(exists("below_3_r") && is.numeric(below_3_r) && length(below_3_r) == 1 && .t_match(.t_num(below_3_r, 4), "97c8c0726b9c89df539a3af8308d29a7638f453e344761c8ea5e62f5ff23010f") &&
                   exists("above_3_r") && is.numeric(above_3_r) && length(above_3_r) == 1 && .t_match(.t_num(above_3_r, 4), "17840c8f072d77a8a0933b859da4fb5a236157574071cd1222571ae19808eba5"),
                 "Finish the first part of this question first: `below_3`, `above_3`, `below_3_r`, and `above_3_r`.")
        .t_check(exists("below_3_slope") && !is.null(below_3_slope),
                 "`below_3_slope` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(below_3_slope),
                 "`below_3_slope` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(below_3_slope) && length(below_3_slope) == 1,
                 "`below_3_slope` should be a single number.")
        .t_check(exists("below_3_intercept") && !is.null(below_3_intercept),
                 "`below_3_intercept` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(below_3_intercept),
                 "`below_3_intercept` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(below_3_intercept) && length(below_3_intercept) == 1,
                 "`below_3_intercept` should be a single number.")
        .t_check(!.t_match(.t_num(below_3_slope, 4), "6765f820dacf81d128e7deca4369b20e6c2828f6db1c55d1c157651f542d5456"),
                 "Use the cluster's own correlation, `below_3_r`, not `r` from all the eruptions.")
        .t_check(!.t_match(.t_num(below_3_slope, 4), "caf914776d6a344362f51fef14d8c673729096559aad9c7317cc1b78838b50a8"),
                 "Use the SDs of the columns in `below_3`, not in `faithful`.")
        .t_check(.t_match(.t_num(below_3_slope, 4), "fd9b536db74993c68881d221408c872ab52d469d2e871af2f17a64b52b39fb7a"),
                 "`below_3_slope` should be `below_3_r` times the SD of `waiting` in `below_3`, divided by the SD of `eruptions` in `below_3`.")
        .t_check(.t_match(.t_num(below_3_intercept, 4), "69fa7566371c883171e36b033ff4f1596cb901b43dfb6f22e6ef4cc73211197a"),
                 "`below_3_intercept` should be the average of `waiting` in `below_3`, minus `below_3_slope` times the average of `eruptions` in `below_3`.")
        .t_check(exists("above_3_slope") && !is.null(above_3_slope),
                 "`above_3_slope` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(above_3_slope),
                 "`above_3_slope` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(above_3_slope) && length(above_3_slope) == 1,
                 "`above_3_slope` should be a single number.")
        .t_check(exists("above_3_intercept") && !is.null(above_3_intercept),
                 "`above_3_intercept` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(above_3_intercept),
                 "`above_3_intercept` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(above_3_intercept) && length(above_3_intercept) == 1,
                 "`above_3_intercept` should be a single number.")
        .t_check(!.t_match(.t_num(above_3_slope, 4), "f7342ece9c0dd1ff85dc2ae80b912d8073b758fbd21cb264707b5f8c8847d521"),
                 "Use the cluster's own correlation, `above_3_r`, not `r` from all the eruptions.")
        .t_check(!.t_match(.t_num(above_3_slope, 4), "c3b3e21927fc71e6e63128c9f8ba5e2f939812d09b8995dc650b84961aca749f"),
                 "Use the SDs of the columns in `above_3`, not in `faithful`.")
        .t_check(.t_match(.t_num(above_3_slope, 4), "ac7d153014702dbf28833d230df317aedead411edf354885fbe8c6dd8732001d"),
                 "`above_3_slope` should be `above_3_r` times the SD of `waiting` in `above_3`, divided by the SD of `eruptions` in `above_3`.")
        .t_check(.t_match(.t_num(above_3_intercept, 4), "35b31c88d18a313bc09d2003ca90134b9c7e83226f45048c9d5eba9abc0b8bf2"),
                 "`above_3_intercept` should be the average of `waiting` in `above_3`, minus `above_3_slope` times the average of `eruptions` in `above_3`.")
      }
    )
  )
)
