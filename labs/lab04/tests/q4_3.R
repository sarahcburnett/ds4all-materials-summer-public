# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q4_3",
  cases = list(
    ottr::TestCase$new(
      name = "q4_3a",
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
        .t_check(exists("origin_bar") && !is.null(origin_bar),
                 "`origin_bar` is still NULL. Replace the placeholder with your plot.")
        .t_check(.t_is_plot(origin_bar),
                 "`origin_bar` should be a ggplot.")
        .i <- .t_layer(origin_bar, c("GeomBar", "GeomCol"))
        .t_check(!is.na(.i),
                 "`origin_bar` should be a bar chart. Use the geom that counts rows for you.")
        .ld <- ggplot2::layer_data(origin_bar, .i)
        .t_check(length(unique(.ld$x)) == 13,
                 "Put the model year on the x-axis, one bar per year.")
        .t_check(length(unique(.ld$fill)) == 3,
                 "Fill the bars by `Origin`, so each bar is split into US, Europe, and Japan.")
        .t_check(isTRUE(abs(sum(.ld$ymax - .ld$ymin) - 406) < 0.01),
                 "The bars should count every car in `cars_clean`, 406 in all.")
        .t_check(.t_has_label(origin_bar, "title"),
                 "Give `origin_bar` a title that names the variables or states the finding.")
        .t_check(.t_has_label(origin_bar, "x") && .t_has_label(origin_bar, "y"),
                 "Label both axes with labs().")
        .t_check(!identical(.t_lab(origin_bar, "y"), "count"),
                 "Label the y-axis in plain words, not `count`.")
      }
    ),
    ottr::TestCase$new(
      name = "q4_3b",
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
        .t_check(exists("japan_share_1980") && !is.null(japan_share_1980),
                 "`japan_share_1980` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(japan_share_1980),
                 "`japan_share_1980` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(japan_share_1980) && length(japan_share_1980) == 1,
                 "`japan_share_1980` should be a single number.")
        .t_check(!is.na(japan_share_1980),
                 "`japan_share_1980` is NA.")
        .t_check(japan_share_1980 >= 0 && japan_share_1980 <= 1,
                 "Give the share as a proportion between 0 and 1, not a percent or a count.")
        .ans <- .t_num(japan_share_1980, 3)
        .t_check(!.t_match(.ans, "0e51a908f3d7743efc90b57fbd1bad9621f9720c953d1c22b81cf4a4c6030002"),
                 "This is the share across all years. Keep only the 1980 models first.")
        .t_check(!.t_match(.ans, "ee5617d8b6cff9c54559651887cedeeab68e672e86c0641b75d00698cfcf0d89") && !.t_match(.ans, "84c7743249b958d1e836eac93fae820dcb43850257611a4c4ab34a8e9d68f7b2"),
                 "This is the share for a different origin.")
        .t_check(.t_match(.ans, "b954c6ed2e98f74861089a3d0408ef0ef45fe14dd83c03566fef8f8d28124ad1"),
                 "Not the right share yet. Keep the 1980 models, then take the mean of Origin == \"Japan\".")
      }
    )
  )
)
