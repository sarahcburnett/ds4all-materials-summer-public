test = list(
  name = "q3_3",
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

        expected <- c("small", "small", "small", "big", "big")
        if (!exists("bill_sizes") || is.null(bill_sizes)) {
          testthat::fail("You haven't changed `bill_sizes` from NULL yet.")
        } else if (is.logical(bill_sizes)) {
          testthat::fail("Type problem: `bill_sizes` is a logical (TRUE/FALSE) vector. That's the test bills > 20, but you need the labels. Wrap it in ifelse(bills > 20, \"big\", \"small\").")
        } else if (is.numeric(bill_sizes)) {
          testthat::fail("Type problem: `bill_sizes` is a vector of numbers, but it should be a character vector of the labels \"big\" and \"small\".")
        } else if (is.factor(bill_sizes)) {
          testthat::fail("Type problem: `bill_sizes` is a factor, but it should be a plain character vector. ifelse() with \"big\" and \"small\" in quotes gives you that directly.")
        } else if (!is.character(bill_sizes)) {
          testthat::fail(paste0("Type problem: `bill_sizes` is ", describe_type(bill_sizes), ", but it should be a character vector of \"big\"/\"small\" labels."))
        } else if (length(bill_sizes) != 5) {
          testthat::fail("`bill_sizes` should have one label for each of the 5 bills. Did you use ifelse() on the whole vector?")
        } else if (identical(unname(bill_sizes), c("small", "big", "small", "big", "big"))) {
          testthat::fail("Check the $20.00 bill: exactly 20 is not OVER 20. Did you use >= where you wanted >?")
        } else if (identical(unname(bill_sizes), rev(expected))) {
          testthat::fail("Your labels look reversed. Check which branch of ifelse() is the yes and which is the no.")
        } else if (!identical(unname(bill_sizes), expected) && identical(tolower(trimws(unname(bill_sizes))), expected)) {
          testthat::fail("Right idea, but the labels must be exactly \"big\" and \"small\" in lower case with no extra spaces.")
        } else if (!identical(unname(bill_sizes), expected)) {
          testthat::fail("Not quite. A bill is \"big\" when it is strictly over 20, and \"small\" otherwise.")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
