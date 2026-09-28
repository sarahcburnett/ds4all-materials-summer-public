test = list(
  name = "q7_3",
  cases = list(
    ottr::TestCase$new(
      name = "q7_3a",
      hidden = FALSE,
      points = 1,
      code = {
        testthat::expect_true(
          exists("q7_3_answer") && !is.null(q7_3_answer),
          info = "`q7_3_answer` is still NULL. Replace the placeholder with your answer."
        )

        given <- toupper(trimws(as.character(q7_3_answer)[1]))
        if (is.na(given)) given <- ""

        testthat::expect_true(
          given %in% c("A", "B", "C", "D"),
          info = 'Set `q7_3_answer` to one of "A", "B", "C", or "D", as a string in quotes.'
        )

        hints <- c(
          B = paste(
            "A histogram of gross shows the spread of one number across all the films.",
            "It never mentions studios. Where would studio go?"
          ),
          C = paste(
            "A scatter plot needs two numeric variables. Is studio a number?"
          ),
          D = paste(
            "A line chart connects consecutive points, which only means something when",
            "there is a real order to them. Is there a natural order to studios?"
          )
        )
        hint <- if (given %in% names(hints)) {
          hints[[given]]
        } else {
          paste(
            "What type is the x-variable in this question?",
            "It is not a number and it is not a date."
          )
        }

        if (identical(given, "A")) {
          testthat::succeed()
        } else {
          testthat::fail(hint)
        }
      }
    )
  )
)
