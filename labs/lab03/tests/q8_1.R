test = list(
  name = "q8_1",
  cases = list(
    ottr::TestCase$new(
      name = "q8_1a",
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
        testthat::expect_true(
          exists("image_path") && !is.null(image_path),
          info = "`image_path` is still NULL. Replace the placeholder with a path in quotes."
        )
        given <- trimws(as.character(image_path)[1])
        testthat::expect_true(
          !is.na(given) && nzchar(given),
          info = "`image_path` is empty. It should be the path to one of the files in the images folder."
        )
        testthat::expect_true(
          !startsWith(given, "/") && !grepl("^[A-Za-z]:", given),
          info = paste(
            "That path starts at the very top of this particular machine, which makes it",
            "absolute. Write it starting from where the notebook is instead."
          )
        )
      }
    ),
    ottr::TestCase$new(
      name = "q8_1b",
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
        given <- sub("^\\./", "", trimws(as.character(image_path)[1]))
        testthat::expect_true(
          file.exists(given),
          info = paste(
            "R cannot find a file there. Run list.files(\"images\") again and compare it to",
            "what you wrote, character by character. Both the name and the extension are",
            "case sensitive."
          )
        )
        testthat::expect_true(
          basename(dirname(given)) == "images",
          info = paste(
            "The file is not sitting next to the notebook, it is one folder in. The path",
            "has to name that folder before it names the file."
          )
        )
      }
    )
  )
)
