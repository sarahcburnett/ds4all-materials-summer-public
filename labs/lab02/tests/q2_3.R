test = list(
  name = "q2_3",
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

        if (!exists("num_non_vowels") || is.null(num_non_vowels)) {
          testthat::fail("You haven't defined `num_non_vowels` yet.")
        } else if (!is.function(num_non_vowels)) {
          testthat::fail(paste0("Type problem: `num_non_vowels` is ", describe_type(num_non_vowels), ", but it should be a function."))
        } else {
          r1 <- safe_call(num_non_vowels("data science"))
          if (!is.null(msg <- check_numeric(r1, "num_non_vowels(\"data science\")"))) {
            if (is.logical(r1)) {
              testthat::fail(paste0(msg, " It looks like you returned the TRUE/FALSE vector itself. Use sum() to count the TRUEs."))
            } else {
              testthat::fail(msg)
            }
          } else if (!isTRUE(all.equal(r1, 7))) {
            testthat::fail("num_non_vowels(\"data science\") should be 7 (remember: the space counts as a non-vowel).")
          } else if (!isTRUE(all.equal(safe_call(num_non_vowels("rhythm")), 6))) {
            testthat::fail("num_non_vowels(\"rhythm\") should be 6 -- no vowels there at all!")
          } else if (!isTRUE(all.equal(safe_call(num_non_vowels("aeiou")), 0))) {
            testthat::fail("num_non_vowels(\"aeiou\") should be 0. Are you counting the right characters?")
          } else {
            testthat::succeed()
          }
        }
      }
    )
  )
)
