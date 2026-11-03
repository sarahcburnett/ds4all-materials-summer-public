# Lab 05 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_1",
  cases = list(
    ottr::TestCase$new(
      name = "q1_1a",
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
        .t_check(exists("pop_mean") && !is.null(pop_mean),
                 "`pop_mean` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(pop_mean),
                 "`pop_mean` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(pop_mean) && length(pop_mean) == 1,
                 "`pop_mean` should be a single number.")
        .t_check(!.t_match(.t_num(pop_mean), "a5dad4949f7e649535d16ecaecfc29452029322aa38828ccdbc34b56a21fc34c"),
                 "That's the median salary. The question asks for the average: use mean().")
        .t_check(.t_match(.t_num(pop_mean), "0d7f09b7a0706d632a985fb146c25e2276bc9edbe60a9ad554d0a9f8a9340f7f"),
                 "Not the right value yet. Average the `salary` column of `salaries`, all 41,252 rows.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_1b",
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
        .t_check(exists("pop_sd") && !is.null(pop_sd),
                 "`pop_sd` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(pop_sd),
                 "`pop_sd` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(pop_sd) && length(pop_sd) == 1,
                 "`pop_sd` should be a single number.")
        .t_check(pop_sd < 1e8,
                 "That number is far too big to be the SD of salaries. It looks like the variance: use sd().")
        .t_check(.t_match(.t_num(pop_sd), "908b123e0ef9384167f0eb4aad8c366904c494e772c830616ad9ef6cf187af28") || .t_match(.t_num(pop_sd), "352dc77ab5c80bade74d4bb59438c3413ef63d1cfabb10e45f47bd779152f311"),
                 "Not the right value yet. Use sd() on the `salary` column of `salaries`, all 41,252 rows.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_1c",
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
        .t_is_plot <- function(p) inherits(p, "ggplot") || inherits(p, "ggplot2::ggplot")
        .t_labs <- function(p) {
          if (exists("get_labs", envir = asNamespace("ggplot2"), inherits = FALSE)) ggplot2::get_labs(p) else p$labels
        }
        .t_lab <- function(p, a) {
          lab <- .t_labs(p)[[a]]
          if (is.null(lab)) NA_character_ else as.character(as.vector(lab))[1]
        }
        .t_has_label <- function(p, a) {
          lab <- .t_lab(p, a)
          !is.na(lab) && nzchar(trimws(lab)) && trimws(lab) != "..."
        }
        .t_mapped <- function(p, a) {
          m <- p$mapping[[a]]
          if (is.null(m)) for (l in p$layers) if (!is.null(l$mapping[[a]])) { m <- l$mapping[[a]]; break }
          if (is.null(m)) NA_character_ else rlang::as_label(m)
        }
        .t_stat_layer <- function(p, stats) {
          s <- vapply(p$layers, function(l) class(l$stat)[1], character(1))
          which(s %in% stats)[1]
        }
        .t_check(exists("salary_hist") && !is.null(salary_hist),
                 "`salary_hist` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(.t_is_plot(salary_hist),
                 "`salary_hist` should be a ggplot.")
        .i <- .t_stat_layer(salary_hist, "StatBin")
        .t_check(!is.na(.i),
                 "`salary_hist` should be a histogram. Which geom bins a numeric variable?")
        .t_check(identical(.t_mapped(salary_hist, "x"), "salary"),
                 "Map the `salary` column to x.")
        .ld <- ggplot2::layer_data(salary_hist, .i)
        .t_check(isTRUE(sum(.ld$count) == 41252),
                 "The histogram should count all 41,252 salaries. Plot the `salaries` tibble.")
        .t_check(.t_has_label(salary_hist, "title"),
                 "Replace the ... in the title with a title that describes the plot.")
        .t_check(.t_has_label(salary_hist, "x") && .t_has_label(salary_hist, "y"),
                 "Replace the ... in the axis labels with labels in plain words.")
        .t_check(!identical(.t_lab(salary_hist, "x"), "salary"),
                 "Label the x-axis in plain words with units, not with the column name.")
      }
    )
  )
)
