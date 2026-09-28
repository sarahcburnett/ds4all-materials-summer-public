test = list(
  name = "q3_2",
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

        if (!exists("is_even") || is.null(is_even)) {
          testthat::fail("You haven't defined `is_even` yet.")
        } else if (!is.function(is_even)) {
          testthat::fail(paste0("Type problem: `is_even` is ", describe_type(is_even), ", but it should be a function."))
        } else {
          r4 <- safe_call(is_even(4))
          r7 <- safe_call(is_even(7))
          if (inherits(r4, "error")) {
            testthat::fail(paste0("is_even(4) gave an error: ", conditionMessage(r4)))
          } else if (is.null(r4)) {
            testthat::fail("is_even(4) returned NULL. Make sure the function returns the TRUE/FALSE value (don't just print() it).")
          } else if (is.character(r4)) {
            testthat::fail("Type problem: is_even(4) returned text (\"TRUE\" in quotes) instead of the logical value TRUE. Drop the quotes: R's TRUE and FALSE are their own type.")
          } else if (is.numeric(r4)) {
            testthat::fail(paste0("Type problem: is_even(4) returned the number ", r4[1], ", but it should return a logical (TRUE/FALSE). The expression n %% 2 == 0 already is TRUE or FALSE."))
          } else if (!is.logical(r4)) {
            testthat::fail(paste0("Type problem: is_even(4) returned ", describe_type(r4), ", but it should return a logical (TRUE/FALSE)."))
          } else if (length(r4) != 1) {
            testthat::fail(paste0("is_even(4) should return a single TRUE or FALSE, but it returned ", length(r4), " values."))
          } else if (!isTRUE(r4) || !isTRUE(safe_call(is_even(0)))) {
            testthat::fail("is_even(4) and is_even(0) should both be TRUE. Remember: even means n %% 2 == 0.")
          } else if (!identical(r7, FALSE)) {
            testthat::fail("is_even(7) should be FALSE.")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
