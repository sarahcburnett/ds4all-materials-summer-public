# Lab 06 autograder test, UNHASHED instructor copy. Do not distribute.
test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      name = "q1_2a",
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
        .t_check(exists("standard_units") && !is.null(standard_units),
                 "`standard_units` does not exist yet. Fill in the blank and run the cell.")
        .t_check(is.function(standard_units),
                 "`standard_units` should be a function.")
        .x <- c(1, 2, 2, 10)
        .z <- tryCatch(standard_units(.x), error = function(e) e)
        .t_check(!inherits(.z, "error"),
                 paste("standard_units(c(1, 2, 2, 10)) gave an error:", conditionMessage(.z)))
        .t_check(is.numeric(.z) && length(.z) == 4,
                 "standard_units() should return a vector of numbers, one for each number it gets.")
        .z <- unname(as.numeric(.z))
        .t_check(!isTRUE(all.equal(.z, .x / sd(.x))),
                 "Subtract the average before dividing by the SD.")
        .t_check(!isTRUE(all.equal(.z, .x - mean(.x) / sd(.x))),
                 "Check your parentheses: subtract the average first, then divide the difference by the SD.")
        .t_check(!isTRUE(all.equal(.z, (.x - mean(.x)) / sqrt(mean((.x - mean(.x))^2)))),
                 "Use sd() for the SD, as in the 14.2 reading.")
        .t_check(isTRUE(all.equal(.z, (.x - mean(.x)) / sd(.x))),
                 "standard_units() should subtract the average of the numbers, then divide by their SD.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_2b",
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
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("faithful_su") && !is.null(faithful_su),
                 "`faithful_su` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(faithful_su) && nrow(faithful_su) == 272,
                 "`faithful_su` should be the `faithful` tibble, all 272 rows, with two new columns. Start the pipeline with `faithful`.")
        .t_check(all(c("eruptions_su", "waiting_su") %in% names(faithful_su)),
                 "`faithful_su` needs the columns `eruptions_su` and `waiting_su`. Name them inside mutate().")
        .d <- faithful_su$eruptions_su
        .w <- faithful_su$waiting_su
        .t_check(is.numeric(.d) && is.numeric(.w),
                 "`eruptions_su` and `waiting_su` should be numbers.")
        .su <- function(v) (v - mean(v)) / sd(v)
        .t_check(!(isTRUE(all.equal(.d, .su(faithful$waiting))) && isTRUE(all.equal(.w, .su(faithful$eruptions)))),
                 "`eruptions_su` and `waiting_su` are swapped.")
        .t_check(isTRUE(all.equal(.d, .su(faithful$eruptions))),
                 "`eruptions_su` should be the `eruptions` column in standard units. Use standard_units(eruptions).")
        .t_check(isTRUE(all.equal(.w, .su(faithful$waiting))),
                 "`waiting_su` should be the `waiting` column in standard units. Use standard_units(waiting).")
      }
    )
  )
)
