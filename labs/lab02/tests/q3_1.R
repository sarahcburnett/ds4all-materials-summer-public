test = list(
  name = "q3_1",
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

        if (!exists("choose_tip") || is.null(choose_tip)) {
          testthat::fail("You haven't defined `choose_tip` yet.")
        } else if (!is.function(choose_tip)) {
          testthat::fail(paste0("Type problem: `choose_tip` is ", describe_type(choose_tip), ", but it should be a function."))
        } else {
          r1 <- safe_call(choose_tip("excellent"))
          if (!is.null(msg <- check_numeric(r1, "choose_tip(\"excellent\")"))) {
            if (is.character(r1) && grepl("%", r1[1])) {
              testthat::fail(paste0(msg, " Return the rate as a proportion, e.g. 0.25, not \"25%\"."))
            } else {
              testthat::fail(msg)
            }
          } else if (!isTRUE(all.equal(r1, 0.25))) {
            testthat::fail("choose_tip(\"excellent\") should return 0.25.")
          } else if (!isTRUE(all.equal(safe_call(choose_tip("good")), 0.20))) {
            testthat::fail("choose_tip(\"good\") should return 0.2. Check your else if branch.")
          } else if (!isTRUE(all.equal(safe_call(choose_tip("meh")), 0.15)) || !isTRUE(all.equal(safe_call(choose_tip("terrible")), 0.15))) {
            testthat::fail("Any other service string should return 0.15. Use a plain else for the catch-all.")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
