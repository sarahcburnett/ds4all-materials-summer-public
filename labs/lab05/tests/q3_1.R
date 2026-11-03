# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q3_1",
  cases = list(
    ottr::TestCase$new(
      name = "q3_1a",
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
        .t_check(exists("our_sample") && is.data.frame(our_sample) && nrow(our_sample) == 400 && "salary" %in% names(our_sample),
                 "`our_sample` should be a tibble of 400 salaries. Rerun the cell at the start of Part 3 that creates it.")
        .t_check(exists("boot_dist") && !is.null(boot_dist),
                 "`boot_dist` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(boot_dist) && "stat" %in% names(boot_dist),
                 "`boot_dist` should be a tibble with a `stat` column. End the pipeline with calculate().")
        .t_check(nrow(boot_dist) == 5000,
                 paste0("`boot_dist` has ", nrow(boot_dist), " rows. Use reps = 5000, for one row per bootstrap sample."))
        .m <- mean(our_sample$salary)
        .ref <- sd(our_sample$salary) / sqrt(nrow(our_sample))
        .bs <- sd(boot_dist$stat)
        .t_check(.bs > 0,
                 "Every bootstrap mean is the same. Use type = \"bootstrap\" in generate(), so each resample is drawn with replacement.")
        .t_check(abs(mean(boot_dist$stat) - .m) < 0.02 * .m,
                 "The bootstrap distribution should center on the mean of `our_sample`. Calculate stat = \"mean\".")
        .t_check(abs(.bs / .ref - 1) < 0.06,
                 "The spread of the bootstrap means is off. Bootstrap `our_sample` with type = \"bootstrap\", and calculate stat = \"mean\".")
        .t_check(exists("boot_se") && !is.null(boot_se),
                 "`boot_se` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(boot_se),
                 "`boot_se` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(boot_se) && length(boot_se) == 1,
                 "`boot_se` should be a single number.")
        .t_check(isTRUE(all.equal(unname(as.numeric(boot_se)), .bs)),
                 "`boot_se` should be the SD of the `stat` column of `boot_dist`.")
      }
    ),
    ottr::TestCase$new(
      name = "q3_1b",
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
        .t_check(exists("our_sample") && is.data.frame(our_sample) && nrow(our_sample) == 400 && "salary" %in% names(our_sample),
                 "`our_sample` should be a tibble of 400 salaries. Rerun the cell at the start of Part 3 that creates it.")
        .t_check(exists("formula_se") && !is.null(formula_se),
                 "`formula_se` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(formula_se),
                 "`formula_se` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(formula_se) && length(formula_se) == 1,
                 "`formula_se` should be a single number.")
        .sd <- sd(our_sample$salary)
        .t_check(!isTRUE(all.equal(unname(as.numeric(formula_se)), .sd)),
                 "That's the SD of the salaries in `our_sample`. The SE divides it by the square root of the sample size.")
        .t_check(!(exists("pop_sd") && is.numeric(pop_sd) && length(pop_sd) == 1 && isTRUE(abs(formula_se - pop_sd / 20) < 0.01)),
                 "That uses the population SD. In Part 3 you only have `our_sample`, so use the SD of its salaries.")
        .t_check(abs(formula_se / (.sd / sqrt(400)) - 1) < 0.002,
                 "`formula_se` should be the SD of the salaries in `our_sample`, divided by the square root of 400.")
      }
    )
  )
)
