# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
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
        .t_check(exists("sample_means_100") && !is.null(sample_means_100),
                 "`sample_means_100` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(sample_means_100),
                 "`sample_means_100` should be a tibble with one row per sample. End the pipeline with summarize().")
        .t_check(nrow(sample_means_100) != 10000 * 100,
                 "`sample_means_100` has one row per salary, not one per sample. Add summarize() to compute one mean per sample.")
        .t_check("sample_mean" %in% names(sample_means_100),
                 "Name the column of means `sample_mean` inside summarize().")
        .t_check(nrow(sample_means_100) == 10000,
                 paste0("`sample_means_100` has ", nrow(sample_means_100), " rows. Use reps = 10000, for one row per sample."))
        .m <- sample_means_100$sample_mean
        .t_check(is.numeric(.m) && length(unique(.m)) > 1000,
                 "Most of the values in `sample_mean` are the same number. Each mean should come from its own sample: use rep_slice_sample() before summarize().")
        .t_check(abs(mean(.m) - 113400) < 1200,
                 "The sample means in `sample_means_100` should center on the population mean. Take the mean of the `salary` column.")
        .s <- sd(sample_means_100$sample_mean)
        .t_check(.s > 7290 && .s < 8390,
                 "Your sample means are too spread out or too bunched together for samples of 100. Check n in rep_slice_sample().")
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
        .t_check(exists("bell_answer") && !is.null(bell_answer),
                 "`bell_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(bell_answer) && length(bell_answer) == 1,
                 "Set `bell_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(bell_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `bell_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "6b23c0d5f35d1b11f9b683f0b0a617355deb11277d91ae091d399c655b87940d"),
                 "Not quite. Compare the population histogram from Question 1.1 with this one, and think about what happens to the most extreme salaries when you average 100 of them.")
      }
    )
  )
)
