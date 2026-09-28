test = list(
  name = "q2_1",
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

        if (!exists("to_percentage") || is.null(to_percentage)) {
          testthat::fail("You haven't defined `to_percentage` yet.")
        } else if (!is.function(to_percentage)) {
          testthat::fail(paste0("Type problem: `to_percentage` is ", describe_type(to_percentage), ", but it should be a function. Define it with to_percentage <- function(proportion) { ... }"))
        } else {
          r1 <- safe_call(to_percentage(0.2))
          r2 <- safe_call(to_percentage(0.55))
          if (!is.null(msg <- check_numeric(r1, "to_percentage(0.2)"))) {
            testthat::fail(msg)
          } else if (!isTRUE(all.equal(r1, 20))) {
            testthat::fail("to_percentage(0.2) should return 20. What do you multiply a proportion by?")
          } else if (!is.null(msg <- check_numeric(r2, "to_percentage(0.55)"))) {
            testthat::fail(msg)
          } else if (!isTRUE(all.equal(r2, 55))) {
            testthat::fail("Almost! Make sure your function works for any proportion, not just 0.2.")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
