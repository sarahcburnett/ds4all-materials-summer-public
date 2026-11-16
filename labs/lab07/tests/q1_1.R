# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
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
        .t_sig <- function(x) paste(substr(as.character(x), 1, 1), collapse = "")
        .t_same <- function(a, b) isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = FALSE))
        .t_get <- function(name) get0(name, envir = parent.frame())
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
        .b <- .t_get("banknotes")
        .t_check(is.data.frame(.b) && nrow(.b) == 1372 &&
                   all(c("WaveletVar", "WaveletSkew", "WaveletCurt", "Entropy", "Class") %in% names(.b)) &&
                   .t_match(.t_sig(.b$Class), "435fc787ba5a65e7f2c76672a5f68c03b44aba6b19bea5328a7971b82b67348a"),
                 "`banknotes` isn't the table the lab started with. Run the cell that reads `data/banknote.csv` again.")
        .p <- .t_get("banknote_scatter")
        .t_check(!is.null(.p),
                 "`banknote_scatter` does not exist yet. Fill in the blanks and run the cell.")
        .t_check(.t_is_plot(.p),
                 "`banknote_scatter` should be a ggplot.")
        .i <- .t_geom_layer(.p, "GeomPoint")
        .t_check(!is.na(.i),
                 "`banknote_scatter` should be a scatter plot. Keep the geom_point() layer.")
        .x <- .t_mapped(.p, "x")
        .y <- .t_mapped(.p, "y")
        .c <- .t_mapped(.p, "colour")
        .t_check(!(identical(.x, "WaveletCurt") && identical(.y, "WaveletVar")),
                 "The axes are swapped. Put `WaveletVar` on x and `WaveletCurt` on y.")
        .t_check(identical(.x, "WaveletVar") && identical(.y, "WaveletCurt"),
                 "Map `WaveletVar` to x and `WaveletCurt` to y, without quotes.")
        .t_check(identical(.c, "Class"),
                 "Map `Class` to color, so that each point is colored by its class.")
        .ld <- ggplot2::layer_data(.p, .i)
        .t_check(nrow(.ld) == 1372,
                 "The plot should show all 1,372 banknotes. Plot the `banknotes` tibble.")
        .t_check(.t_has_label(.p, "title"),
                 "Replace the ... in the title with a title that describes the plot.")
        .t_check(.t_has_label(.p, "x") && .t_has_label(.p, "y"),
                 "Replace the ... in the axis labels with labels in plain words.")
        .t_check(!identical(.t_lab(.p, "x"), "WaveletVar") && !identical(.t_lab(.p, "y"), "WaveletCurt"),
                 "Label the axes in plain words, not with the column names.")
      }
    )
  )
)
