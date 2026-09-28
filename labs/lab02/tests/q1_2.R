test = list(
  name = "q1_2",
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

        if (!exists("each_pays") || !exists("left_over") || is.null(each_pays) || is.null(left_over)) {
          testthat::fail("Fill in both `each_pays` and `left_over`.")
        } else if (!is.null(msg <- check_numeric(each_pays, "`each_pays`"))) {
          testthat::fail(msg)
        } else if (!is.null(msg <- check_numeric(left_over, "`left_over`"))) {
          testthat::fail(msg)
        } else if (isTRUE(all.equal(each_pays, 2)) && isTRUE(all.equal(left_over, 9))) {
          testthat::fail("Looks like you swapped the operators: %/% gives the whole-dollar share, %% gives the remainder.")
        } else if (!isTRUE(all.equal(each_pays, 9))) {
          testthat::fail("Check `each_pays`: how many whole dollars does 47 %/% 5 give?")
        } else if (!isTRUE(all.equal(left_over, 2))) {
          testthat::fail("Check `left_over`: what remainder does 47 %% 5 leave?")
        } else {
          testthat::succeed()
        }
      }
    )
  )
)
