test = list(
  name = "q5_2",
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

        if (!exists("dinner_share") || is.null(dinner_share)) {
          testthat::fail("You haven't defined `dinner_share` yet.")
        } else if (!is.function(dinner_share)) {
          testthat::fail(paste0("Type problem: `dinner_share` is ", describe_type(dinner_share), ", but it should be a function."))
        } else {
          r1 <- safe_call(dinner_share(100, 4, "excellent"))
          if (!is.null(msg <- check_numeric(r1, "dinner_share(100, 4, \"excellent\")"))) {
            testthat::fail(msg)
          } else if (!isTRUE(all.equal(r1, 31.25))) {
            testthat::fail("dinner_share(100, 4, \"excellent\") should be 31.25. Use choose_tip() to get the rate.")
          } else if (!isTRUE(all.equal(safe_call(dinner_share(100, 4, "good")), 30))) {
            testthat::fail("dinner_share(100, 4, \"good\") should be 30.")
          } else if (!isTRUE(all.equal(safe_call(dinner_share(100, 4, "meh")), 28.75))) {
            testthat::fail("dinner_share(100, 4, \"meh\") should be 28.75 -- any unrecognized service means a 15% tip.")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
