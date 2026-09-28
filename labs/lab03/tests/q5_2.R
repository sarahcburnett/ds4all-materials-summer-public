test = list(
  name = "q5_2",
  cases = list(
    ottr::TestCase$new(
      name = "q5_2a",
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
          isPlot(gross_bar),
          info = "`gross_bar` should be a ggplot object."
        )
        a <- plotAes(gross_bar)
        testthat::expect_true(
          !is.null(a$x) && grepl("year", a$x, fixed = TRUE),
          info = "x should be mapped to year."
        )
        testthat::expect_true(
          !is.null(a$y) && grepl("total_gross", a$y, fixed = TRUE),
          info = paste(
            "y should be mapped to total_gross. If you left y out entirely, you are using",
            "the geom that counts rows, and there is exactly one row per year to count."
          )
        )
      }
    ),
    ottr::TestCase$new(
      name = "q5_2b",
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
          "StatIdentity" %in% plotStats(gross_bar),
          info = paste(
            "movies_by_year is already summarized: one row per year, with the height you",
            "want sitting in a column. Nothing needs counting, so use the other bar geom."
          )
        )
        testthat::expect_true(
          hasTitle(gross_bar),
          info = "Give the plot a title inside labs()."
        )
        testthat::expect_true(
          customLab(gross_bar, "y", "total_gross"),
          info = "The y-axis still says total_gross. Write a label a reader would understand, and say what the units are."
        )
      }
    )
  )
)
