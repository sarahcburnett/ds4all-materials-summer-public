test = list(
  name = "q7_1",
  cases = list(
    ottr::TestCase$new(
      name = "q7_1a",
      hidden = FALSE,
      points = 1,
      code = {
        testthat::expect_true(
          exists("q7_1_answer") && !is.null(q7_1_answer),
          info = "`q7_1_answer` is still NULL. Replace the placeholder with your answer."
        )

        given <- toupper(trimws(as.character(q7_1_answer)[1]))
        if (is.na(given)) given <- ""

        testthat::expect_true(
          given %in% c("A", "B", "C", "D"),
          info = 'Set `q7_1_answer` to one of "A", "B", "C", or "D", as a string in quotes.'
        )

        hints <- c(
          A = paste(
            "A histogram shows one variable on its own. This question is about two",
            "variables moving together. Can a single histogram show you that?"
          ),
          C = paste(
            "A bar chart compares categories. Are body mass and flipper length",
            "categories, or are they numbers?"
          ),
          D = paste(
            "A line chart is for a trend over time. Is there any time variable in this",
            "question at all?"
          )
        )
        hint <- if (given %in% names(hints)) {
          hints[[given]]
        } else {
          paste(
            "Count the variables in the question and ask what type each one is.",
            "Then find the matching row in the table above."
          )
        }

        if (identical(given, "B")) {
          testthat::succeed()
        } else {
          testthat::fail(hint)
        }
      }
    )
  )
)
