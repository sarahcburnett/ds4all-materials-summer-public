test = list(
  name = "q5_1",
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

        if (!exists("split_bill") || is.null(split_bill)) {
          testthat::fail("You haven't defined `split_bill` yet.")
        } else if (!is.function(split_bill)) {
          testthat::fail(paste0("Type problem: `split_bill` is ", describe_type(split_bill), ", but it should be a function."))
        } else {
          default_try <- safe_call(split_bill(100, 4))
          if (inherits(default_try, "error")) {
            testthat::fail(paste0("split_bill(100, 4) errored (", conditionMessage(default_try), "). Did you give tip_rate a default value of 0.2 in the definition?"))
          } else if (!is.null(msg <- check_numeric(default_try, "split_bill(100, 4)"))) {
            testthat::fail(msg)
          } else if (!isTRUE(all.equal(default_try, 30))) {
            testthat::fail("split_bill(100, 4) should be 30: ($100 * 1.2) / 4 people.")
          } else if (!isTRUE(all.equal(safe_call(split_bill(100, 4, tip_rate = 0.25)), 31.25))) {
            testthat::fail("split_bill(100, 4, tip_rate = 0.25) should be 31.25. Is your function using the tip_rate argument?")
          } else if (!isTRUE(all.equal(safe_call(split_bill(47, 5)), 11.28))) {
            testthat::fail("split_bill(47, 5) should be 11.28. Are you rounding to 2 decimal places with round()?")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
