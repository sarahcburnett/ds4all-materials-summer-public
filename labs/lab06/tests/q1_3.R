# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      name = "q1_3a",
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
        .t_check(exists("r_guess") && !is.null(r_guess),
                 "`r_guess` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(r_guess) && length(r_guess) == 1,
                 "Set `r_guess` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(r_guess))
        .t_check(.a %in% c("A", "B", "C", "D", "E"),
                 "Set `r_guess` to one of the letters A, B, C, D, or E, in quotes.")
        .t_check(.t_match(.a, "3f39d5c348e5b79d06e842c114e6cc571583bbf44e4b0ebfda1a01ec05745d43"),
                 "Not quite. A correlation is always between -1 and 1, and your scatter plot slopes up.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_3b",
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
        .su <- function(v) (v - mean(v)) / sd(v)
        .t_check(exists("faithful_su") && is.data.frame(faithful_su) && nrow(faithful_su) == 272 &&
                   all(c("eruptions_su", "waiting_su") %in% names(faithful_su)) &&
                   isTRUE(all.equal(faithful_su$eruptions_su, .su(faithful$eruptions))) &&
                   isTRUE(all.equal(faithful_su$waiting_su, .su(faithful$waiting))),
                 "Fix `faithful_su` in Question 1.2 first.")
        .t_check(exists("faithful_products") && !is.null(faithful_products),
                 "`faithful_products` does not exist yet. Fill in the blank and run the cell.")
        .t_check(is.data.frame(faithful_products) && nrow(faithful_products) == 272,
                 "`faithful_products` should be `faithful_su` with one new column, all 272 rows. Start the pipeline with `faithful_su`.")
        .t_check("product_su" %in% names(faithful_products),
                 "`faithful_products` needs a column named `product_su`. Name it inside mutate().")
        .p <- faithful_products$product_su
        .t_check(is.numeric(.p) && length(.p) == 272,
                 "`product_su` should hold a number for every eruption.")
        .t_check(!isTRUE(all.equal(.p, faithful$eruptions * faithful$waiting)),
                 "You multiplied the original columns, `eruptions` and `waiting`. Multiply their standard units, `eruptions_su` and `waiting_su`.")
        .t_check(!(isTRUE(all.equal(.p, .su(faithful$eruptions) * faithful$waiting)) ||
                     isTRUE(all.equal(.p, faithful$eruptions * .su(faithful$waiting)))),
                 "Use standard units for both columns: `eruptions_su` and `waiting_su`.")
        .t_check(!isTRUE(all.equal(.p, .su(faithful$eruptions) + .su(faithful$waiting))),
                 "Multiply the standard units. Don't add them.")
        .t_check(isTRUE(all.equal(.p, .su(faithful$eruptions) * .su(faithful$waiting))),
                 "`product_su` should be `eruptions_su` times `waiting_su`, row by row.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_3c",
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
        .su <- function(v) (v - mean(v)) / sd(v)
        .t_check(exists("faithful_products") && is.data.frame(faithful_products) && "product_su" %in% names(faithful_products) &&
                   isTRUE(all.equal(faithful_products$product_su, .su(faithful$eruptions) * .su(faithful$waiting))),
                 "Finish `faithful_products` first, in the first step of this question.")
        .t_check(exists("r") && !is.null(r),
                 "`r` is still NULL. Replace the placeholder with your code. If you divide by n, set it first, for example n <- nrow(faithful).")
        .t_check(!is.matrix(r),
                 "cor() of a whole table gives the correlation of every pair of columns. Compute `r` from `product_su` instead.")
        .t_check(!is.data.frame(r),
                 "`r` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(r) && length(r) == 1,
                 "`r` should be a single number.")
        .t_any <- function(x, keys) any(vapply(keys, function(k) .t_match(x, k), logical(1)))
        .v <- .t_num(r, 4)
        .t_check(!.t_any(.v, c("195f6cdc72a8e609a11913ef1aea71b39166c05e7eba2d4ecf56f89ee8aa59ac", "90e6f659bf39b50d974c637528a657e0b9ed0650c0cd682c6c63cf80ca50f6c2", "67a6bc6f51c58f824be4655eb42aa58791b6db4f9ec74557ce1c88d399b2ff9d")),
                 "You multiplied the original columns, `eruptions` and `waiting`. Add up `product_su`, which uses standard units, and divide by n - 1.")
        .t_check(!.t_any(.v, c("85e92fc4603051c4750d539ea1d4e84ee0ce23ebf906e2d2580cf7274d7bedd0", "069dad5f58a929b01a3707031fe4626f6ba18692c53ecc8dab9c4626f412f02d", "8a582b0af049081e789c5003d458a50d42e3c6be08a9577a6281b581b3b81840", "6f71dfabcccecf4a414111f7531cdb141be562f605535ef321cc6d97a7874f86", "52d3604937c06049a9c9ab94d57f8d30db8774a2d09ce59859b59832e5f12c31", "3cc23499457bfac114f1d4dbdbcbad7e11bb8f4809f5fd68854505e8e462d462")),
                 "Use standard units for both columns. Add up `product_su` and divide by n - 1.")
        .t_check(!.t_match(.v, "553dfbfaddd11c07f52c5dcaf2a99089c317c89e9d24268409dfbf68b3a7bd93"),
                 "That's the sum of the products. Divide it by n - 1.")
        .t_check(!.t_match(.v, "8fb2980ef775212975a178e41b85fc070aafe39dc284b4aed1adb0693244438e"),
                 "Close: that's the average of the products, which divides by n. sd() in standard_units() divides by n - 1, so divide the sum by n - 1 to match cor().")
        .t_check(r >= -1 && r <= 1,
                 "A correlation is always between -1 and 1. Add up `product_su` and divide by n - 1.")
        .t_check(.t_match(.v, "cef8cb90d1edb4dc53e9ad6effe900a79e38dcfd15899b608fc54ecc7732da81"),
                 "Not the right value yet. Add up `product_su` and divide by the number of eruptions minus 1.")
      }
    )
  )
)
