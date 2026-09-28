test = list(
  name = "q4_4",
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

        if (!exists("days") || is.null(days)) {
          testthat::fail("`days` is missing. Complete the while loop so it counts the doublings.")
        } else if (!is.null(msg <- check_numeric(days, "`days`"))) {
          testthat::fail(msg)
        } else if (isTRUE(all.equal(days, 0))) {
          testthat::fail("`days` is still 0 -- complete the while loop so it counts the doublings.")
        } else if (isTRUE(all.equal(days, 6))) {
          testthat::fail("Off by one! After 6 doublings you have $64, which doesn't exceed $100 yet.")
        } else if (isTRUE(all.equal(days, 8))) {
          testthat::fail("Off by one! Check your loop condition: you may have doubled once too many times.")
        } else if (!isTRUE(all.equal(days, 7))) {
          testthat::fail("Not quite. Keep doubling money and adding 1 to days while money is at most 100.")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
