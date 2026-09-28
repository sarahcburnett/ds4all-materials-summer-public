test = list(
  name = "q7_2",
  cases = list(
    ottr::TestCase$new(
      name = "q7_2a",
      hidden = FALSE,
      points = 1,
      code = {
        testthat::expect_true(
          exists("q7_2_answer") && !is.null(q7_2_answer),
          info = "`q7_2_answer` is still NULL. Replace the placeholder with your answer."
        )

        given <- toupper(trimws(as.character(q7_2_answer)[1]))
        if (is.na(given)) given <- ""

        testthat::expect_true(
          given %in% c("A", "B", "C", "D"),
          info = 'Set `q7_2_answer` to one of "A", "B", "C", or "D", as a string in quotes.'
        )

        hints <- c(
          A = paste(
            "That scatter plot shows how two numbers move together, but it never separates",
            "front-wheel drive from rear-wheel drive. Where would the grouping variable go?"
          ),
          B = paste(
            "A bar chart of drivetrain tells you how many cars are in each group. The",
            "question is not how many there are, it is whether their mileage differs."
          ),
          D = paste(
            "A line chart is for a trend over time. Is this question about time?"
          )
        )
        hint <- if (given %in% names(hints)) {
          hints[[given]]
        } else {
          paste(
            "This question compares two groups on one numeric variable.",
            "Find that row in the table above."
          )
        }

        if (identical(given, "C")) {
          testthat::succeed()
        } else {
          testthat::fail(hint)
        }
      }
    )
  )
)
