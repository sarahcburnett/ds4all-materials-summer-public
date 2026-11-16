# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      name = "q1_3a",
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
        .s <- .t_get("banknotes_shuffled")
        .t_check(is.data.frame(.s) && nrow(.s) == 1372 && all(names(.b) %in% names(.s)) &&
                   .t_match(.t_sig(.s$Class), "4da8c05b034205dacb791f2228ead1b02d6ee76dae0c8c95aac5d825ad59d98f") &&
                   .t_match(.t_num(sum(.s$WaveletVar[1:686]), 4), "8992e9cd4937bf62950981fab65058f8352071cf29cfb19f169f123c8d676800"),
                 "`banknotes_shuffled` isn't the shuffle the autograder expects. Run the cell with set.seed(7) again, and leave the seed as it is.")
        .v <- .t_get("banknote_train")
        .t_check(!is.null(.v),
                 "`banknote_train` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(.v),
                 "`banknote_train` should be a tibble: some of the rows of `banknotes_shuffled`.")
        .t_check(all(names(.s) %in% names(.v)),
                 "`banknote_train` should keep every column of `banknotes_shuffled`. Use slice(), which keeps rows by their position.")
        .t_check(nrow(.v) != 1372,
                 "`banknote_train` has all 1,372 rows. Keep only its 686 rows with slice().")
        .t_check(TRUE, "")
        .t_check(nrow(.v) == 686,
                 "`banknote_train` should have 686 rows.")
        .t_check(!.t_same(.v, .b[1:686, ]),
                 "Those rows come from `banknotes`. Take them from `banknotes_shuffled`.")
        .t_check(!.t_same(.v, .s[687:1372, ]),
                 "The two sets are swapped. The first 686 rows are the training set.")
        .t_check(.t_same(.v, .s[1:686, ]),
                 "`banknote_train` should be rows 1:686 of `banknotes_shuffled`, in the same order. Use slice(), and don't shuffle again.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_3b",
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
        .s <- .t_get("banknotes_shuffled")
        .t_check(is.data.frame(.s) && nrow(.s) == 1372 && all(names(.b) %in% names(.s)) &&
                   .t_match(.t_sig(.s$Class), "4da8c05b034205dacb791f2228ead1b02d6ee76dae0c8c95aac5d825ad59d98f") &&
                   .t_match(.t_num(sum(.s$WaveletVar[1:686]), 4), "8992e9cd4937bf62950981fab65058f8352071cf29cfb19f169f123c8d676800"),
                 "`banknotes_shuffled` isn't the shuffle the autograder expects. Run the cell with set.seed(7) again, and leave the seed as it is.")
        .v <- .t_get("banknote_test")
        .t_check(!is.null(.v),
                 "`banknote_test` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(.v),
                 "`banknote_test` should be a tibble: some of the rows of `banknotes_shuffled`.")
        .t_check(all(names(.s) %in% names(.v)),
                 "`banknote_test` should keep every column of `banknotes_shuffled`. Use slice(), which keeps rows by their position.")
        .t_check(nrow(.v) != 1372,
                 "`banknote_test` has all 1,372 rows. Keep only its 686 rows with slice().")
        .t_check(nrow(.v) != 687,
                 "`banknote_test` has 687 rows. Row 686 is already in the training set, so start at row 687.")
        .t_check(nrow(.v) == 686,
                 "`banknote_test` should have 686 rows.")
        .t_check(!.t_same(.v, .b[687:1372, ]),
                 "Those rows come from `banknotes`. Take them from `banknotes_shuffled`.")
        .t_check(!.t_same(.v, .s[1:686, ]),
                 "The two sets are swapped. The first 686 rows are the training set.")
        .t_check(.t_same(.v, .s[687:1372, ]),
                 "`banknote_test` should be rows 687:1372 of `banknotes_shuffled`, in the same order. Use slice(), and don't shuffle again.")
      }
    )
  )
)
