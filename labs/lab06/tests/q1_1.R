# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
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
        .t_geom_layer <- function(p, geoms) {
          g <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
          which(g %in% geoms)[1]
        }
        .t_check(exists("faithful") && is.data.frame(faithful) && all(c("eruptions", "waiting") %in% names(faithful)) &&
                   nrow(faithful) == 272,
                 "`faithful` should be the Old Faithful data: 272 rows, with the columns `eruptions` and `waiting`. If you changed it, run faithful <- as_tibble(datasets::faithful) to get it back.")
        .t_check(exists("faithful_scatter") && !is.null(faithful_scatter),
                 "`faithful_scatter` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(.t_is_plot(faithful_scatter),
                 "`faithful_scatter` should be a ggplot.")
        .i <- .t_geom_layer(faithful_scatter, "GeomPoint")
        .t_check(!is.na(.i),
                 "`faithful_scatter` should be a scatter plot. Which geom draws one point for each row?")
        .x <- .t_mapped(faithful_scatter, "x")
        .y <- .t_mapped(faithful_scatter, "y")
        .t_check(!(identical(.x, "waiting") && identical(.y, "eruptions")),
                 "The axes are swapped. Put the column we want to predict, `waiting`, on the vertical axis.")
        .t_check(identical(.x, "eruptions") && identical(.y, "waiting"),
                 "Map `eruptions` to x and `waiting` to y.")
        .ld <- ggplot2::layer_data(faithful_scatter, .i)
        .t_check(nrow(.ld) == 272,
                 "The plot should show all 272 eruptions. Plot the `faithful` tibble.")
        .t_check(.t_has_label(faithful_scatter, "title"),
                 "Replace the ... in the title with a title that describes the plot.")
        .t_check(.t_has_label(faithful_scatter, "x") && .t_has_label(faithful_scatter, "y"),
                 "Replace the ... in the axis labels with labels in plain words.")
        .t_check(!identical(.t_lab(faithful_scatter, "x"), "eruptions") && !identical(.t_lab(faithful_scatter, "y"), "waiting"),
                 "Label the axes in plain words with units, not with the column names.")
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
        .t_check(exists("shape_answer") && !is.null(shape_answer),
                 "`shape_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(shape_answer) && length(shape_answer) == 1,
                 "Set `shape_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(shape_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `shape_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "559aead08264d5795d3909718cdd05abd49572e84fe55590eef31a88a08fdffd"),
                 "Not quite. Look at your scatter plot: as the duration goes up, what happens to the wait? Within each cluster, do the points still rise along a line?")
      }
    )
  )
)
