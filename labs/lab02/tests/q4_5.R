test = list(
  name = "q4_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        # ---- shared helpers for type-aware feedback ----------------------
        describe_type <- function(x) {
          if (is.function(x)) return("a function (did you forget to call it, e.g. f(...)?)")
          if (is.data.frame(x)) return("a data frame")
          if (is.list(x)) return("a list")
          if (is.factor(x)) return("a factor")
          if (is.character(x)) return("a character string (text in quotes)")
          if (is.logical(x)) return("a logical (TRUE/FALSE)")
          if (is.numeric(x)) return("a number")
          paste0("of type ", typeof(x))
        }
        # Run a student function call; return the value, or an error condition.
        safe_call <- function(expr) tryCatch(expr, error = function(e) e)
        # Returns a message (or NULL if fine) checking that `x` is a numeric of length n.
        check_numeric <- function(x, label, n = 1, call_text = label) {
          if (inherits(x, "error")) {
            return(paste0(call_text, " gave an error: ", conditionMessage(x)))
          }
          if (is.null(x)) {
            return(paste0(call_text, " is NULL. If this is a function, make sure the last line of the body is the value you want to return (not print() or cat(), which don't return anything useful)."))
          }
          if (is.character(x) && length(x) == n && !anyNA(suppressWarnings(as.numeric(x)))) {
            return(paste0("Type problem: ", call_text, " is ", describe_type(x), " but should be a number. Remove the quotes: \"", x[1], "\" is text, ", x[1], " is a number."))
          }
          if (!is.numeric(x)) {
            return(paste0("Type problem: ", call_text, " is ", describe_type(x), " but should be a number."))
          }
          if (length(x) != n) {
            what <- if (n == 1) "a single number" else paste0("a vector of ", n, " numbers")
            return(paste0(call_text, " should be ", what, ", but it has length ", length(x), "."))
          }
          NULL
        }
        # ------------------------------------------------------------------

        expected <- 2 ^ (0:9)
        if (!exists("powers_of_two") || is.null(powers_of_two)) {
          testthat::fail("You haven't changed `powers_of_two` from NULL yet.")
        } else if (!is.numeric(powers_of_two)) {
          testthat::fail(paste0("Type problem: `powers_of_two` is ", describe_type(powers_of_two), ", but it should be a numeric vector."))
        } else if (isTRUE(all.equal(unname(powers_of_two), 2 ^ (1:10)))) {
          testthat::fail("Close -- but the first power should be 2^0 = 1. Check your exponent range.")
        } else if (length(powers_of_two) != 10) {
          testthat::fail(paste0("`powers_of_two` should have exactly 10 elements, 2^0 through 2^9, but it has ", length(powers_of_two), "."))
        } else if (isTRUE(all.equal(unname(powers_of_two), (0:9) ^ 2))) {
          testthat::fail("Those are squares (0^2, 1^2, ...). You want 2 raised to each exponent: 2 ^ (0:9).")
        } else if (!isTRUE(all.equal(unname(powers_of_two), expected))) {
          testthat::fail("Not quite. Try raising 2 to the vector 0:9.")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
