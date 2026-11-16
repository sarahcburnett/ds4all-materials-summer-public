# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_5",
  cases = list(
    ottr::TestCase$new(
      name = "q1_5a",
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
        .t_check(is.data.frame(.t_get("banknote_train")) && is.data.frame(.t_get("banknote_test")) &&
                   .t_same(banknote_train, .s[1:686, ]) && .t_same(banknote_test, .s[687:1372, ]),
                 "Fix `banknote_train` and `banknote_test` in Question 1.3 first.")
        .xa <- .t_get("train_four")
        .xe <- .t_get("test_four")
        .t_check(!is.null(.xa) && !is.null(.xe),
                 "`train_four` and `test_four` are still NULL. Replace the placeholders with your code.")
        .four <- c("Entropy", "WaveletCurt", "WaveletSkew", "WaveletVar")
        .t_check(is.data.frame(.xa) && is.data.frame(.xe) && !("Class" %in% names(.xa)) && !("Class" %in% names(.xe)),
                 "`train_four` and `test_four` should hold the four attributes only. Leave out `Class` with select(-Class).")
        .t_check(identical(sort(names(.xa)), .four) && identical(sort(names(.xe)), .four),
                 "`train_four` and `test_four` should each have all four attributes: `WaveletVar`, `WaveletSkew`, `WaveletCurt`, and `Entropy`.")
        .t_check(nrow(.xa) == 686 && nrow(.xe) == 686 && .t_same(.xa[.four], banknote_train[.four]) && .t_same(.xe[.four], banknote_test[.four]),
                 "`train_four` should come from `banknote_train`, and `test_four` from `banknote_test`.")
        .p <- .t_get("predicted_four")
        .t_check(!is.null(.p),
                 "`predicted_four` does not exist yet. If knn() gave an error about NA or NaN, `train` or `test` still has a class column in it: knn() needs attributes only.")
        .t_check(!is.data.frame(.p),
                 "`predicted_four` is a table. It should be the vector of predictions that knn() returns.")
        .t_check(is.factor(.p) || is.character(.p),
                 "`predicted_four` should be the predictions that knn() returns: one class for each banknote in the test set.")
        .t_check(length(.p) != 2 * 686,
                 "`predicted_four` has a prediction for every row of the data. Classify only the test set: 686 rows.")
        .t_check(length(.p) == 686,
                 "`predicted_four` should have 686 predictions, one for each banknote in the test set. Check what you gave `test`.")
        .g <- .t_sig(.p)
        .t_check(!.t_match(.g, "a6f4169155d7d9b4e2b1b15ace2ce105d6b2c26d80eb317dc911119dc94105d8"),
                 "Those are the predictions from two attributes. Use `train_four` and `test_four` in knn().")
        .t_check(!.t_match(.g, "5ddeaddbf09a98321e7a9e6e2b23a21af206a07f654cfb1a9d73449800111d1c"),
                 "You classified the training set. Give `test` the attributes of the test set, `test_four`.")
        .t_check(.t_match(.g, "1fa96247ad9dfae13d7fc383b2049c53b60e7cb36ab7532b479882984ba0e967"),
                 "Check each argument of knn(): `train` and `test` are the attributes of the training set and the test set, `cl` is the classes of the training set, and `k` is 5.")
        .v <- .t_get("accuracy_four")
        .t_check(!is.null(.v),
                 "`accuracy_four` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(.v),
                 "`accuracy_four` is a table. It should be a single number.")
        .t_check(is.numeric(.v) && length(.v) == 1 && !is.na(.v),
                 "`accuracy_four` should be a single number: the proportion of predictions that are right.")
        .acc <- mean(as.character(.p) == as.character(banknote_test$Class))
        .t_check(!isTRUE(all.equal(.v, 100 * .acc)),
                 "Store the accuracy as a proportion between 0 and 1, not as a percent.")
        .t_check(!isTRUE(all.equal(.v, 686 * .acc)),
                 "That's the number of correct predictions. Use mean() instead of sum() to get the proportion.")
        .t_check(isTRUE(all.equal(.v, .acc)),
                 "The accuracy is the proportion of predictions that match the actual classes: mean(predicted_four == banknote_test$Class).")
      }
    ),
    ottr::TestCase$new(
      name = "q1_5b",
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
        .t_check(exists("attributes_answer") && !is.null(attributes_answer),
                 "`attributes_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(attributes_answer) && length(attributes_answer) == 1,
                 "Set `attributes_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(attributes_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `attributes_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "6b23c0d5f35d1b11f9b683f0b0a617355deb11277d91ae091d399c655b87940d"),
                 "Not quite. Both classifiers used the same training set, the same test set, and the same k. What does the classifier know about each banknote now that it didn't before?")
      }
    )
  )
)
