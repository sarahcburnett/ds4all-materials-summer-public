test = list(
  name = "q5_1",
  cases = list(
    ottr::TestCase$new(
      name = "q5_1a",
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
          isPlot(smoker_bar),
          info = "`smoker_bar` should be a ggplot object."
        )
        a <- plotAes(smoker_bar)
        check_equal(a$x, "maternal_smoker", "x should be mapped to maternal_smoker.")
        testthat::expect_true(
          "StatCount" %in% plotStats(smoker_bar),
          info = paste(
            "The question says to use the raw baby data, so you want the geom that does its",
            "own counting. geom_col() expects you to have summarized the data first."
          )
        )
        testthat::expect_true(
          is.null(a$y),
          info = "geom_bar() takes only x. It works out the bar heights itself by counting rows."
        )
      }
    ),
    ottr::TestCase$new(
      name = "q5_1b",
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
          hasTitle(smoker_bar),
          info = "Give the plot a title inside labs()."
        )
      }
    )
  )
)
