# Lab 04 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q3_4",
  cases = list(
    ottr::TestCase$new(
      name = "q3_4a",
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
        .t_check(exists("japan_mpg") && !is.null(japan_mpg),
                 "`japan_mpg` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(japan_mpg),
                 "`japan_mpg` is a table. You need the number inside it: which verb takes a column out as a vector?")
        .t_check(is.numeric(japan_mpg) && length(japan_mpg) == 1,
                 "`japan_mpg` should be a single number.")
        .t_check(!is.nan(japan_mpg),
                 "`japan_mpg` is NaN: no cars matched your filter, so there was nothing to average. Check how Japan is spelled in the `Origin` column.")
        .t_check(!is.na(japan_mpg),
                 "`japan_mpg` is NA. Check what na.rm = TRUE does in mean().")
        .ans <- .t_num(japan_mpg)
        .t_check(!.t_match(.ans, "7dead44581531453a4d90a4ecfd804905f756e6a7329c54ffbfb0a669023e9df"),
                 "This is the average for every car. Keep only the Japanese cars first.")
        .t_check(!.t_match(.ans, "216130923e39c6a43633a7267be1107eacb59a5bf7da06d4ede96b0db8c4a587") && !.t_match(.ans, "3b6cdff1e656732e14026c169f08330269d4d41b6ac015d537f7c56004541ced"),
                 "This is the average for a different origin. Check your filter.")
        .t_check(.t_match(.ans, "cf145fc3cac0eb630f1b7f55b9813309bada494a5402785521fb555180b103a7"),
                 "Not the right value yet. Keep the Japanese cars, then average their MPG.")
      }
    )
  )
)
