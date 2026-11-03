# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
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
        .t_check(exists("sample_means_400") && !is.null(sample_means_400),
                 "`sample_means_400` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(sample_means_400),
                 "`sample_means_400` should be a tibble with one row per sample. End the pipeline with summarize().")
        .t_check(nrow(sample_means_400) != 10000 * 400,
                 "`sample_means_400` has one row per salary, not one per sample. Add summarize() to compute one mean per sample.")
        .t_check("sample_mean" %in% names(sample_means_400),
                 "Name the column of means `sample_mean` inside summarize().")
        .t_check(nrow(sample_means_400) == 10000,
                 paste0("`sample_means_400` has ", nrow(sample_means_400), " rows. Use reps = 10000, for one row per sample."))
        .m <- sample_means_400$sample_mean
        .t_check(is.numeric(.m) && length(unique(.m)) > 1000,
                 "Most of the values in `sample_mean` are the same number. Each mean should come from its own sample: use rep_slice_sample() before summarize().")
        .t_check(abs(mean(.m) - 113400) < 1200,
                 "The sample means in `sample_means_400` should center on the population mean. Take the mean of the `salary` column.")
        .t_check(exists("sample_means_625") && !is.null(sample_means_625),
                 "`sample_means_625` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(sample_means_625),
                 "`sample_means_625` should be a tibble with one row per sample. End the pipeline with summarize().")
        .t_check(nrow(sample_means_625) != 10000 * 625,
                 "`sample_means_625` has one row per salary, not one per sample. Add summarize() to compute one mean per sample.")
        .t_check("sample_mean" %in% names(sample_means_625),
                 "Name the column of means `sample_mean` inside summarize().")
        .t_check(nrow(sample_means_625) == 10000,
                 paste0("`sample_means_625` has ", nrow(sample_means_625), " rows. Use reps = 10000, for one row per sample."))
        .m <- sample_means_625$sample_mean
        .t_check(is.numeric(.m) && length(unique(.m)) > 1000,
                 "Most of the values in `sample_mean` are the same number. Each mean should come from its own sample: use rep_slice_sample() before summarize().")
        .t_check(abs(mean(.m) - 113400) < 1200,
                 "The sample means in `sample_means_625` should center on the population mean. Take the mean of the `salary` column.")
        .s4 <- sd(sample_means_400$sample_mean)
        .s6 <- sd(sample_means_625$sample_mean)
        .t_check(!(.s4 > 2900 && .s4 < 3335 && .s6 > 3630 && .s6 < 4180),
                 "`sample_means_400` and `sample_means_625` look swapped. Check the sample size in each.")
        .t_check(.s4 > 3630 && .s4 < 4180,
                 "The spread of `sample_means_400` is off for samples of 400. Check n in rep_slice_sample().")
        .t_check(.s6 > 2900 && .s6 < 3335,
                 "The spread of `sample_means_625` is off for samples of 625. Check n in rep_slice_sample().")
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
        .t_check(exists("pop_sd") && is.numeric(pop_sd) && length(pop_sd) == 1 &&
          (.t_match(.t_num(pop_sd), "908b123e0ef9384167f0eb4aad8c366904c494e772c830616ad9ef6cf187af28") || .t_match(.t_num(pop_sd), "352dc77ab5c80bade74d4bb59438c3413ef63d1cfabb10e45f47bd779152f311")),
                 "Fix `pop_sd` in Question 1.1 first.")
        .t_check(exists("sample_means_100") && is.data.frame(sample_means_100) && "sample_mean" %in% names(sample_means_100) && nrow(sample_means_100) == 10000,
                 "Finish Question 1.3 first: `sample_means_100` should hold 10,000 sample means in a column `sample_mean`.")
        .t_check(exists("sample_means_400") && is.data.frame(sample_means_400) && "sample_mean" %in% names(sample_means_400) &&
                   exists("sample_means_625") && is.data.frame(sample_means_625) && "sample_mean" %in% names(sample_means_625),
                 "Finish the first part of this question first: simulate `sample_means_400` and `sample_means_625`.")
        .t_check(exists("sd_comparison") && !is.null(sd_comparison),
                 "`sd_comparison` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(sd_comparison),
                 "`sd_comparison` should be a tibble.")
        .t_check(all(c("sample_size", "simulated_sd", "predicted_sd") %in% names(sd_comparison)),
                 "`sd_comparison` needs the columns `sample_size`, `simulated_sd`, and `predicted_sd`.")
        .t_check(nrow(sd_comparison) == 3 && identical(as.numeric(sd_comparison$sample_size), c(100, 400, 625)),
                 "Keep `sample_size` as c(100, 400, 625), one row per sample size.")
        .t_check(isTRUE(all.equal(unname(as.numeric(sd_comparison$simulated_sd)),
                                  c(sd(sample_means_100$sample_mean), sd(sample_means_400$sample_mean), sd(sample_means_625$sample_mean)))),
                 "`simulated_sd` should hold the SD of each vector of sample means, in the order 100, 400, 625.")
        .t_check(.t_match(paste(.t_num(sd_comparison$predicted_sd, 0), collapse = ","), "9402959f97142a98fb24933de5bedd2cbd5457143a5431fe5054ca79bb4ae3a1"),
                 "`predicted_sd` should be the population SD divided by the square root of the sample size.")
      }
    )
  )
)
