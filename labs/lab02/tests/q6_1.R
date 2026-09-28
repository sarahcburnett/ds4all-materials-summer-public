test = list(
  name = "q6_1",
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

        if (!exists("exact_cents") || is.null(exact_cents)) {
          testthat::fail("You haven't defined `exact_cents` yet.")
        } else if (!is.function(exact_cents)) {
          testthat::fail(paste0("Type problem: `exact_cents` is ", describe_type(exact_cents), ", but it should be a function."))
        } else {
          r1 <- safe_call(exact_cents(19.99))
          if (inherits(r1, "error")) {
            testthat::fail(paste0("exact_cents(19.99) gave an error: ", conditionMessage(r1)))
          } else if (is.null(r1)) {
            testthat::fail("exact_cents(19.99) returned NULL. Make sure the last line of the function is the integer value.")
          } else if (is.character(r1)) {
            testthat::fail("Type problem: exact_cents(19.99) returned text (a character string), but it should return an integer. Don't use paste0() or quotes here; use as.integer().")
          } else if (!is.numeric(r1)) {
            testthat::fail(paste0("Type problem: exact_cents(19.99) returned ", describe_type(r1), ", but it should return an integer."))
          } else if (length(r1) != 1) {
            testthat::fail(paste0("exact_cents(19.99) should return a single integer, but it returned ", length(r1), " values."))
          } else if (isTRUE(all.equal(as.numeric(r1), 1998))) {
            testthat::fail("You hit the floating point trap! round() the cents BEFORE converting with as.integer().")
          } else if (!isTRUE(all.equal(as.numeric(r1), 1999))) {
            testthat::fail("exact_cents(19.99) should be 1999. Multiply by 100, round(), then convert.")
          } else if (typeof(r1) != "integer") {
            testthat::fail(paste0("Right number, wrong type! typeof(exact_cents(19.99)) is \"", typeof(r1), "\" but should be \"integer\". round() still gives a double; wrap the result in as.integer()."))
          } else if (!isTRUE(all.equal(as.numeric(safe_call(exact_cents(2.50))), 250)) || !isTRUE(all.equal(as.numeric(safe_call(exact_cents(4.01))), 401))) {
            testthat::fail("Almost! Make sure exact_cents works for any dollar amount, e.g. 2.50 -> 250 and 4.01 -> 401.")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
