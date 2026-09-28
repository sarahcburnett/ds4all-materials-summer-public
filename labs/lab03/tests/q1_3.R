test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      name = "q1_3a",
      hidden = FALSE,
      points = 1,
      code = {
        testthat::expect_true(
          exists("path_answer") && !is.null(path_answer),
          info = "`path_answer` is still NULL. Replace the placeholder with your answer."
        )

        given <- toupper(trimws(as.character(path_answer)[1]))
        if (is.na(given)) given <- ""

        testthat::expect_true(
          given %in% c("A", "B", "C", "D"),
          info = 'Set `path_answer` to one of "A", "B", "C", or "D", as a string in quotes.'
        )

        hints <- c(
          A = paste(
            "Option A starts at the root of one particular computer.",
            "When you push your blog to GitHub it gets rebuilt on a machine that has",
            "no /Users/me on it at all. What happens to that path there?"
          ),
          B = paste(
            "Option B names the project folder, myblog. But look at where about.qmd",
            "sits in the tree: it is already inside myblog. Is there a second myblog",
            "folder in there for this path to find?"
          ),
          D = paste(
            "Option D climbs up one level with ../ before it looks. Start at the folder",
            "about.qmd is in, take one step up, and look around. Is is_me.jpeg in that",
            "folder, or did you just walk past it?"
          )
        )
        hint <- if (given %in% names(hints)) {
          hints[[given]]
        } else {
          paste(
            "Find about.qmd in the tree, then find is_me.jpeg.",
            "Count the folders between them."
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
