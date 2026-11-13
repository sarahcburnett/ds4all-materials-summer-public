# Lab 06 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      name = "q2_2a",
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
        .t_check(exists("slope") && is.numeric(slope) && length(slope) == 1 && .t_match(.t_num(slope, 4), "3991997758c1e877c43d4018c5e6563d4397e4e3973d6b9cf2b7100f937cd5db") &&
                   exists("intercept") && is.numeric(intercept) && length(intercept) == 1 && .t_match(.t_num(intercept, 4), "76a29ea98e9e253f3b83bb3f8b48750a73572ebc31604ab4c336d0500724148a"),
                 "Fix `slope` and `intercept` in Question 2.1 first.")
        .t_check(exists("line_plot") && !is.null(line_plot),
                 "`line_plot` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(.t_is_plot(line_plot),
                 "`line_plot` should be a ggplot.")
        .t_check(!is.na(.t_geom_layer(line_plot, "GeomPoint")) && identical(.t_mapped(line_plot, "x"), "eruptions") &&
                   identical(.t_mapped(line_plot, "y"), "waiting"),
                 "`line_plot` should start from `faithful_scatter`, your scatter plot from Question 1.1. Fix that plot first.")
        .i <- .t_geom_layer(line_plot, "GeomAbline")
        .t_check(!is.na(.i),
                 "Add the line with geom_abline().")
        .ld <- ggplot2::layer_data(line_plot, .i)
        .s <- .ld$slope[1]
        .b <- .ld$intercept[1]
        .sl <- unname(as.numeric(slope))
        .ic <- unname(as.numeric(intercept))
        .t_check(is.numeric(.s) && is.numeric(.b),
                 "Give geom_abline() numbers for both slope and intercept.")
        .t_check(!(isTRUE(all.equal(.s, .ic)) && isTRUE(all.equal(.b, .sl))),
                 "The slope and intercept are swapped.")
        .t_check(!(exists("r") && is.numeric(r) && length(r) == 1 && isTRUE(all.equal(.s, unname(as.numeric(r)))) &&
                   isTRUE(all.equal(.b, 0))),
                 "That's the line in standard units. In original units, use your `slope` and `intercept`.")
        .t_check(isTRUE(all.equal(.s, .sl)) && isTRUE(all.equal(.b, .ic)),
                 "geom_abline() should use your `slope` and `intercept` from Question 2.1.")
      }
    )
  )
)
