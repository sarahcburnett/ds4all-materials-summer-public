# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      name = "q1_2a",
      hidden = FALSE,
      points = 1,
      code = {
        .t_check <- function(ok, hint) {
          if (isTRUE(ok)) testthat::succeed() else testthat::fail(hint)
        }
        .t_match <- function(x, key) {
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_sig <- function(x) paste(substr(as.character(x), 1, 1), collapse = "")
        .t_same <- function(a, b) isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = FALSE))
        .t_get <- function(name) get0(name, envir = parent.frame())
        .f <- .t_get("distance")
        .t_check(is.function(.f),
                 "`distance` should be a function. Run the cell that defines it.")
        .d <- tryCatch(.f(c(0, 0), c(3, 4)), error = function(e) e)
        .t_check(!inherits(.d, "error"),
                 paste0("distance(c(0, 0), c(3, 4)) gave an error: ", if (inherits(.d, "error")) conditionMessage(.d) else "", " Replace the ... with your code."))
        .t_check(!is.null(.d),
                 "distance() returns nothing. The last line of the function should be the distance.")
        .t_check(is.numeric(.d) && length(.d) != 2,
                 "distance() should return one number. Add up the squared differences with sum() before you take the square root.")
        .t_check(is.numeric(.d) && length(.d) == 1,
                 "distance() should return one number.")
        .t_check(!isTRUE(all.equal(.d, 25)),
                 "distance(c(0, 0), c(3, 4)) should be 5, and yours gives 25. Take the square root of the sum.")
        .t_check(!isTRUE(all.equal(.d, 7)) && !isTRUE(all.equal(.d, -7)),
                 "distance(c(0, 0), c(3, 4)) should be 5, and yours gives 7. Square the differences before you add them up.")
        .t_check(!isTRUE(all.equal(.d, sqrt(7))) && !is.nan(.d),
                 "Square the differences before you add them up: (point1 - point2)^2.")
        .t_check(!isTRUE(all.equal(.d, sqrt(12.5))),
                 "Add up the squared differences with sum(), not mean().")
        .t_check(isTRUE(all.equal(.d, 5)),
                 "distance(c(0, 0), c(3, 4)) should be 5. Subtract the two vectors, square, add up with sum(), then take the square root.")
        .d3 <- tryCatch(.f(c(1, 2, 3, 4), c(2, 4, 5, 8)), error = function(e) NA)
        .t_check(isTRUE(all.equal(.d3, 5)),
                 "distance() should work for vectors of any length. Use `point1` and `point2` whole, not one element at a time.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_2b",
      hidden = FALSE,
      points = 1,
      code = {
        .t_check <- function(ok, hint) {
          if (isTRUE(ok)) testthat::succeed() else testthat::fail(hint)
        }
        .t_match <- function(x, key) {
          identical(digest::digest(x, algo = "sha256", serialize = FALSE), key)
        }
        .t_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), unname(as.numeric(x)))
        .t_sig <- function(x) paste(substr(as.character(x), 1, 1), collapse = "")
        .t_same <- function(a, b) isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = FALSE))
        .t_get <- function(name) get0(name, envir = parent.frame())
        .b <- .t_get("banknotes")
        .t_check(is.data.frame(.b) && nrow(.b) == 1372 &&
                   all(c("WaveletVar", "WaveletSkew", "WaveletCurt", "Entropy", "Class") %in% names(.b)) &&
                   .t_match(.t_sig(.b$Class), "435fc787ba5a65e7f2c76672a5f68c03b44aba6b19bea5328a7971b82b67348a"),
                 "`banknotes` isn't the table the lab started with. Run the cell that reads `data/banknote.csv` again.")
        .f <- .t_get("distance")
        .d <- tryCatch(.f(c(0, 0), c(3, 4)), error = function(e) NA)
        .d3 <- tryCatch(.f(c(1, 2, 3, 4), c(2, 4, 5, 8)), error = function(e) NA)
        .t_check(isTRUE(all.equal(.d, 5)) && isTRUE(all.equal(.d3, 5)),
                 "Finish distance() first.")
        .v <- .t_get("first_two_distance")
        .t_check(!is.null(.v),
                 "`first_two_distance` is still NULL. Replace the placeholder with your code.")
        .t_check(is.numeric(.v) && length(.v) == 1,
                 "`first_two_distance` should be a single number. Call distance() on `first_banknote` and `second_banknote`.")
        .t_check(!isTRUE(all.equal(.v, 0)),
                 "A distance of 0 means you compared a banknote with itself. Use `first_banknote` and `second_banknote`.")
        .t_check(!.t_match(.t_num(.v, 4), "4de83bcfaa6dadedf35846f71f3206daeb08725eb1179807522e8ba6b227a3a0"),
                 "That's the distance from the first banknote to the last one. Use `first_banknote` and `second_banknote`.")
        .t_check(.t_match(.t_num(.v, 4), "91aeeb1b2862489d3685b5ed66d879011b7c352cdc27f51dfdd5b0c4510677c3"),
                 "Call distance() on `first_banknote` and `second_banknote`.")
      }
    )
  )
)
