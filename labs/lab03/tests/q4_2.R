test = list(
  name = "q4_2",
  cases = list(
    ottr::TestCase$new(
      name = "q4_2a",
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
          isPlot(smoker_hist),
          info = "`smoker_hist` should be a ggplot object."
        )
        a <- plotAes(smoker_hist)
        check_equal(
          a$x, "birth_weight",
          "The numeric variable goes on x, and here that is birth_weight. maternal_smoker is what you split by, not what you bin."
        )
        testthat::expect_true(
          "StatBin" %in% plotStats(smoker_hist),
          info = "Use geom_histogram()."
        )
      }
    ),
    ottr::TestCase$new(
      name = "q4_2b",
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
        a <- plotAes(smoker_hist)
        splitBy <- c(a$fill, a$colour, facetVars(smoker_hist))
        testthat::expect_true(
          any(grepl("maternal_smoker", splitBy, fixed = TRUE)),
          info = paste(
            "Right now this is one histogram of everybody, so it cannot show a difference",
            "between two groups. Split it: facet_wrap(~ maternal_smoker) gives one panel",
            "per group, or fill = maternal_smoker inside aes() overlays them."
          )
        )
        testthat::expect_true(
          hasTitle(smoker_hist),
          info = "Give the plot a title inside labs()."
        )
        testthat::expect_true(
          customLab(smoker_hist, "x", "birth_weight"),
          info = "The x-axis still says birth_weight. Write a label a reader would understand, with the units."
        )
      }
    )
  )
)
