test = list(
  name = "q4_3",
  cases = list(
    ottr::TestCase$new(
      name = "q4_3a",
      hidden = FALSE,
      points = 1,
      code = {
        testthat::expect_true(
          exists("group_answer") && !is.null(group_answer),
          info = "`group_answer` is still NULL. Replace the placeholder with your answer."
        )

        given <- toupper(trimws(as.character(group_answer)[1]))
        if (is.na(given)) given <- ""

        testthat::expect_true(
          given %in% c("A", "B", "C", "D"),
          info = 'Set `group_answer` to one of "A", "B", "C", or "D", as a string in quotes.'
        )

        hints <- c(
          A = paste(
            "Option A says the two distributions are identical. Look at where each one",
            "peaks and at the left tail. Does one of them sit further left than the other?"
          ),
          B = paste(
            "Option B claims every single baby of a smoker is lighter, which is a much",
            "stronger claim than this plot supports. Look at the range the two groups share.",
            "Are there smokers' babies heavier than some nonsmokers'?"
          ),
          C = paste(
            "Option C has the direction backwards. Find the centre of each distribution",
            "and check which one sits further to the right."
          )
        )
        hint <- if (given %in% names(hints)) {
          hints[[given]]
        } else {
          paste(
            "A good answer here has to get two things right at once:",
            "which way the groups differ, and how much they overlap.",
            "Check your choice against both."
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
