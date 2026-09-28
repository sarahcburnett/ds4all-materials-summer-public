test = list(
  name = "q4_3",
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

        if (!exists("my_total") || is.null(my_total)) {
          testthat::fail("You haven't defined `my_total` yet.")
        } else if (!is.function(my_total)) {
          testthat::fail(paste0("Type problem: `my_total` is ", describe_type(my_total), ", but it should be a function."))
        } else if (grepl("sum\\(", paste(deparse(body(my_total)), collapse = " "))) {
          testthat::fail("No sum() allowed in this one! Build the total yourself with a for loop.")
        } else {
          r1 <- safe_call(my_total(c(1, 2, 3)))
          if (!is.null(msg <- check_numeric(r1, "my_total(c(1, 2, 3))"))) {
            if (is.null(r1)) {
              testthat::fail(paste0(msg, " A for loop returns NULL by itself, so put `total` on its own as the last line of the function."))
            } else if (is.numeric(r1) && length(r1) > 1) {
              testthat::fail(paste0(msg, " It looks like you returned the vector instead of the running total."))
            } else {
              testthat::fail(msg)
            }
          } else if (!isTRUE(all.equal(r1, 6))) {
            testthat::fail("my_total(c(1, 2, 3)) should be 6. Start total at 0 and add each element inside the loop.")
          } else if (!isTRUE(all.equal(safe_call(my_total(c(20.12, 39.90, 31.01, 25.53, 48.99))), 165.55))) {
            testthat::fail("Almost! Make sure your loop visits every element of x.")
          } else if (!isTRUE(all.equal(safe_call(my_total(numeric(0))), 0))) {
            testthat::fail("my_total(numeric(0)) -- an empty vector -- should be 0. Loop with for (val in x), not 1:length(x).")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
