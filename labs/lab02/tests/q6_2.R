test = list(
  name = "q6_2",
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

        if (!exists("receipt") || is.null(receipt)) {
          testthat::fail("You haven't defined `receipt` yet.")
        } else if (!is.function(receipt)) {
          testthat::fail(paste0("Type problem: `receipt` is ", describe_type(receipt), ", but it should be a function."))
        } else {
          r1 <- safe_call(receipt(c("eggs", "milk", "bread")))
          if (inherits(r1, "error")) {
            testthat::fail(paste0("receipt(c(\"eggs\", \"milk\", \"bread\")) gave an error: ", conditionMessage(r1)))
          } else if (is.null(r1)) {
            testthat::fail("Type problem: receipt(...) returned NULL. It probably printed the text with cat() or print() instead of returning it. Make the paste0()/paste() expression the last line of the function so it is returned.")
          } else if (!is.character(r1)) {
            testthat::fail(paste0("Type problem: receipt(...) returned ", describe_type(r1), ", but it should return a single character string."))
          } else if (length(r1) != 1) {
            testthat::fail(paste0("Type problem: receipt(...) returned a character vector of length ", length(r1), ", but it should be ONE string. Use paste(items, collapse = \", \") to glue the items together before adding the \"Receipt: \" prefix."))
          } else if (identical(r1, "Receipt: eggs milk bread")) {
            testthat::fail("So close -- the items need a comma AND a space between them. Try collapse = \", \".")
          } else if (identical(r1, "Receipt:eggs, milk, bread")) {
            testthat::fail("Almost -- you're missing the space after the colon.")
          } else if (!identical(r1, "Receipt: eggs, milk, bread")) {
            testthat::fail(paste0("Not quite. Expected exactly \"Receipt: eggs, milk, bread\" but got \"", r1, "\". Check your spacing character by character."))
          } else if (!identical(safe_call(receipt(c("tea"))), "Receipt: tea")) {
            testthat::fail("receipt(c(\"tea\")) should be \"Receipt: tea\". Does your function work for a single item?")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
