test = list(
  name = "q4_1",
  cases = list(
    ottr::TestCase$new(
      name = "q4_1a",
      hidden = FALSE,
      points = 1,
      code = {
        check_equal <- function(actual, expected, hint) {
          if (isTRUE(all.equal(actual, expected))) {
            testthat::succeed()
          } else {
            testthat::fail(hint)
          }
        }
        plotAes <- function(p) {
          maps <- c(list(p$mapping), lapply(p$layers, function(l) l$mapping))
          out <- list()
          for (m in maps) {
            if (is.null(m) || length(m) == 0) next
            for (nm in names(m)) out[[nm]] <- rlang::as_label(m[[nm]])
          }
          out
        }
        plotStats <- function(p) {
          if (length(p$layers) == 0) return(character(0))
          vapply(p$layers, function(l) class(l$stat)[1], character(1))
        }
        plotGeoms <- function(p) {
          if (length(p$layers) == 0) return(character(0))
          vapply(p$layers, function(l) class(l$geom)[1], character(1))
        }
        facetVars <- function(p) {
          pr <- p$facet$params
          if (is.null(pr)) return(character(0))
          unique(c(names(pr$facets), names(pr$rows), names(pr$cols)))
        }
        isPlot <- function(x) !is.null(x) && inherits(x, "ggplot")
        hasTitle <- function(p) {
          tt <- p$labels$title
          !is.null(tt) && nzchar(as.character(tt)[1])
        }
        customLab <- function(p, which, mapped) {
          v <- p$labels[[which]]
          !is.null(v) && nzchar(as.character(v)[1]) &&
            !identical(as.character(v)[1], mapped)
        }
        testthat::expect_true(
          isPlot(height_hist),
          info = "`height_hist` should be a ggplot object."
        )
        a <- plotAes(height_hist)
        check_equal(a$x, "maternal_height", "x should be mapped to maternal_height.")
        testthat::expect_true(
          is.null(a$y),
          info = "A histogram takes only x. ggplot2 counts the rows for the y-axis itself."
        )
        testthat::expect_true(
          "StatBin" %in% plotStats(height_hist),
          info = "Use geom_histogram(). geom_bar() and geom_col() are for categories, not for binning a number."
        )
      }
    ),
    ottr::TestCase$new(
      name = "q4_1b",
      hidden = FALSE,
      points = 1,
      code = {
        check_equal <- function(actual, expected, hint) {
          if (isTRUE(all.equal(actual, expected))) {
            testthat::succeed()
          } else {
            testthat::fail(hint)
          }
        }
        plotAes <- function(p) {
          maps <- c(list(p$mapping), lapply(p$layers, function(l) l$mapping))
          out <- list()
          for (m in maps) {
            if (is.null(m) || length(m) == 0) next
            for (nm in names(m)) out[[nm]] <- rlang::as_label(m[[nm]])
          }
          out
        }
        plotStats <- function(p) {
          if (length(p$layers) == 0) return(character(0))
          vapply(p$layers, function(l) class(l$stat)[1], character(1))
        }
        plotGeoms <- function(p) {
          if (length(p$layers) == 0) return(character(0))
          vapply(p$layers, function(l) class(l$geom)[1], character(1))
        }
        facetVars <- function(p) {
          pr <- p$facet$params
          if (is.null(pr)) return(character(0))
          unique(c(names(pr$facets), names(pr$rows), names(pr$cols)))
        }
        isPlot <- function(x) !is.null(x) && inherits(x, "ggplot")
        hasTitle <- function(p) {
          tt <- p$labels$title
          !is.null(tt) && nzchar(as.character(tt)[1])
        }
        customLab <- function(p, which, mapped) {
          v <- p$labels[[which]]
          !is.null(v) && nzchar(as.character(v)[1]) &&
            !identical(as.character(v)[1], mapped)
        }
        bins <- height_hist$layers[[1]]$stat_params$bins
        testthat::expect_true(
          !is.null(bins) && bins == 25,
          info = "The question asks for 25 bins. bins goes inside geom_histogram(), not inside aes()."
        )
        testthat::expect_true(
          hasTitle(height_hist),
          info = "Give the plot a title inside labs()."
        )
      }
    )
  )
)
