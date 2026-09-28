# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q4_1",
  cases = list(
    ottr::TestCase$new(
      name = "q4_1a",
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
        .t_check(exists("mpg_by_year") && !is.null(mpg_by_year),
                 "`mpg_by_year` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(is.data.frame(mpg_by_year),
                 "`mpg_by_year` should be a tibble.")
        .t_check(all(c("Year", "avg_mpg") %in% names(mpg_by_year)),
                 "`mpg_by_year` needs the columns `Year` and `avg_mpg`.")
        .t_check(nrow(mpg_by_year) == 13,
                 "`mpg_by_year` should have one row per model year, 13 in all. Group by `Year` before you summarize.")
        .t_check(!anyNA(mpg_by_year$avg_mpg),
                 "Some averages are NA. Those years include a car with a missing MPG: check na.rm = TRUE.")
        .ord <- order(mpg_by_year$Year)
        .t_check(.t_match(paste(.t_num(mpg_by_year$avg_mpg[.ord], 3), collapse = ","), "8c77dabea7e084c3369b101a97cf347560e50b9929a8f857026a092afed1db7d"),
                 "The averages don't match. Average the MPG column of `cars_clean` within each year.")
      }
    ),
    ottr::TestCase$new(
      name = "q4_1b",
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
          !is.na(lab) && nzchar(trimws(lab))
        }
        .t_mapped <- function(p, a) {
          m <- p$mapping[[a]]
          if (is.null(m)) for (l in p$layers) if (!is.null(l$mapping[[a]])) { m <- l$mapping[[a]]; break }
          if (is.null(m)) NA_character_ else rlang::as_label(m)
        }
        .t_layer <- function(p, geoms) {
          g <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
          which(g %in% geoms)[1]
        }
        .t_check(exists("mpg_line") && !is.null(mpg_line),
                 "`mpg_line` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(.t_is_plot(mpg_line),
                 "`mpg_line` should be a ggplot.")
        .i <- .t_layer(mpg_line, "GeomLine")
        .t_check(!is.na(.i),
                 "`mpg_line` should draw a line. Check the geom.")
        .ld <- ggplot2::layer_data(mpg_line, .i)
        .t_check(identical(as.numeric(sort(unique(.ld$x))), as.numeric(1970:1982)),
                 "The x-axis should show the model years, 1970 to 1982. Map `Year` to x.")
        .t_check(.t_match(paste(.t_num(.ld$y[order(.ld$x)], 3), collapse = ","), "8c77dabea7e084c3369b101a97cf347560e50b9929a8f857026a092afed1db7d"),
                 "The line doesn't follow the yearly averages. Plot `mpg_by_year`, with `avg_mpg` on the y-axis.")
        .t_check(.t_has_label(mpg_line, "title"),
                 "Give `mpg_line` a title that names the variables or states the finding.")
        .t_check(.t_has_label(mpg_line, "x") && .t_has_label(mpg_line, "y"),
                 "Label both axes with labs().")
        .t_check(!identical(.t_lab(mpg_line, "y"), .t_mapped(mpg_line, "y")),
                 "Label the y-axis in plain words, not with the column name.")
      }
    ),
    ottr::TestCase$new(
      name = "q4_1c",
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
        .t_check(exists("rose_every_year") && !is.null(rose_every_year),
                 "`rose_every_year` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.character(rose_every_year),
                 "Use TRUE or FALSE without quotes.")
        .t_check(is.logical(rose_every_year) && length(rose_every_year) == 1 && !is.na(rose_every_year),
                 "`rose_every_year` should be TRUE or FALSE.")
        .t_check(.t_match(as.character(rose_every_year), "4e523a5ae5b4636c75901b79fafbd3912e41dc7987414e688b09d4b436ff22b3"),
                 "Not quite. Read the line one year at a time: the overall direction and every single step are different questions.")
      }
    )
  )
)
