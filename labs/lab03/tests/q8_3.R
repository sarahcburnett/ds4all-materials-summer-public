test = list(
  name = "q8_3",
  cases = list(
    ottr::TestCase$new(
      name = "q8_3a",
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
          exists("post_image_path") && !is.null(post_image_path),
          info = "`post_image_path` is still NULL. Replace the placeholder with a path in quotes."
        )
        given <- trimws(as.character(post_image_path)[1])
        testthat::expect_true(
          !is.na(given) && nzchar(given),
          info = "`post_image_path` is empty. It should be the path that goes inside posts/post1/index.qmd."
        )
        testthat::expect_true(
          !startsWith(given, "/") && !grepl("^[A-Za-z]:", given),
          info = paste(
            "That is an absolute path. It would break the moment the site is rebuilt",
            "anywhere but on your own laptop."
          )
        )
      }
    ),
    ottr::TestCase$new(
      name = "q8_3b",
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
        given <- gsub("\\\\", "/", trimws(as.character(post_image_path)[1]))
        given <- sub("^\\./", "", given)
        hint <- if (!grepl("^\\.\\./", given)) {
          paste(
            "This path looks for the image inside post1, and is_me.jpeg is not in post1.",
            "Look at the tree and work out how to get from post1 back to the folder the",
            "image is actually in."
          )
        } else {
          paste(
            "You are climbing, but count the levels again. Put your finger on post1 and",
            "walk up one folder at a time until you reach is_me.jpeg. How many steps was",
            "that?"
          )
        }
        check_equal(given, "../../is_me.jpeg", hint)
      }
    )
  )
)
