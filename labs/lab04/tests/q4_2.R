# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q4_2",
  cases = list(
    ottr::TestCase$new(
      name = "q4_2a",
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
        .t_check(exists("weight_scatter") && !is.null(weight_scatter),
                 "`weight_scatter` is still NULL. Replace the placeholder with your plot.")
        .t_check(.t_is_plot(weight_scatter),
                 "`weight_scatter` should be a ggplot.")
        .i <- .t_layer(weight_scatter, "GeomPoint")
        .t_check(!is.na(.i),
                 "`weight_scatter` should draw a point for each car. Which geom makes a scatter plot?")
        .ld <- ggplot2::layer_data(weight_scatter, .i)
        .ok <- !is.na(.ld$x) & !is.na(.ld$y)
        .t_check(!any(.ld$y[.ok] == 0),
                 "Some points sit at 0 MPG. Plot `cars_clean`, where those values are NA.")
        .t_check(!isTRUE(abs(sum(.ld$x[.ok]) - 9358.8) < 0.01),
                 "The axes are swapped. Weight goes on the x-axis and MPG on the y-axis.")
        .t_check(isTRUE(abs(sum(.ld$x[.ok]) - 1182229.0) < 0.01) &&
                   isTRUE(abs(sum(.ld$y[.ok]) - 9358.8) < 0.01),
                 "Map `Weight` to x and `MPG` to y, using `cars_clean`.")
        .t_check(.t_has_label(weight_scatter, "title"),
                 "Give `weight_scatter` a title that names the variables or states the finding.")
        .t_check(.t_has_label(weight_scatter, "x") && .t_has_label(weight_scatter, "y"),
                 "Label both axes with labs().")
        .t_check(!identical(.t_lab(weight_scatter, "x"), .t_mapped(weight_scatter, "x")),
                 "Label the x-axis in plain words with units, not with the column name.")
        .t_check(!identical(.t_lab(weight_scatter, "y"), .t_mapped(weight_scatter, "y")),
                 "Label the y-axis in plain words with units, not with the column name.")
      }
    ),
    ottr::TestCase$new(
      name = "q4_2b",
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
        .t_check(exists("weight_answer") && !is.null(weight_answer),
                 "`weight_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(weight_answer) && length(weight_answer) == 1,
                 "Set `weight_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(weight_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `weight_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "df7e70e5021544f4834bbee64a9e3789febc4be81470df629cad6ddb03320a5c"),
                 "Not quite. Look at which way the points move as weight increases, and how closely they follow that direction.")
      }
    )
  )
)
