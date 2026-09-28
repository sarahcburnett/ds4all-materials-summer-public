test = list(
  name = "q3_3",
  cases = list(
    ottr::TestCase$new(
      name = "q3_3a",
      hidden = FALSE,
      points = 1,
      code = {
        testthat::expect_true(
          exists("scatter_answer") && !is.null(scatter_answer),
          info = "`scatter_answer` is still NULL. Replace the placeholder with your answer."
        )

        given <- toupper(trimws(as.character(scatter_answer)[1]))
        if (is.na(given)) given <- ""

        testthat::expect_true(
          given %in% c("A", "B", "C", "D"),
          info = 'Set `scatter_answer` to one of "A", "B", "C", or "D", as a string in quotes.'
        )

        hints <- c(
          A = paste(
            "Option A says strong positive. Compare this cloud to the pregnancy-length",
            "plot above it, which really is positive. Do the points here climb from the",
            "lower left to the upper right the way those do?"
          ),
          B = paste(
            "Option B says strong negative, so the points would fall from the upper left",
            "to the lower right. Cover the left half of the plot with your hand, then the",
            "right half. Do the two halves sit at different heights?"
          ),
          C = paste(
            "Option C says the plot cannot be read. It can be, and what it is telling you",
            "is specific and useful. Describe the shape of the cloud rather than giving up",
            "on it."
          )
        )
        hint <- if (given %in% names(hints)) {
          hints[[given]]
        } else {
          paste(
            "Pick a mother's age on the x-axis and look straight up.",
            "How wide is the spread of birth weights above it?",
            "Now do the same at a very different age. Did that spread move?"
          )
        }

        if (identical(given, "D")) {
          testthat::succeed()
        } else {
          testthat::fail(hint)
        }
      }
    )
  )
)
