# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q1_4",
  cases = list(
    ottr::TestCase$new(
      name = "q1_4a",
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
        .tt <- .t_get("test_two")
        .t_check(!is.null(.tt),
                 "`test_two` is still NULL. Replace the placeholder with your code.")
        .t_check(is.data.frame(.tt) && identical(sort(names(.tt)), c("WaveletCurt", "WaveletVar")),
                 "`test_two` should have two columns, `WaveletVar` and `WaveletCurt`, like `train_two`.")
        .t_check(identical(names(.tt), c("WaveletVar", "WaveletCurt")),
                 "Put the columns of `test_two` in the same order as in `train_two`: `WaveletVar`, then `WaveletCurt`. knn() matches columns by position.")
        .t_check(nrow(.tt) == 686 && .t_same(.tt[c("WaveletVar", "WaveletCurt")], banknote_test[c("WaveletVar", "WaveletCurt")]),
                 "`test_two` should come from `banknote_test`: the same two columns that `train_two` takes from `banknote_train`.")
        .p <- .t_get("predicted_two")
        .t_check(!is.null(.p),
                 "`predicted_two` does not exist yet. If knn() gave an error about NA or NaN, `train` or `test` still has a class column in it: knn() needs attributes only.")
        .t_check(!is.data.frame(.p),
                 "`predicted_two` is a table. It should be the vector of predictions that knn() returns.")
        .t_check(is.factor(.p) || is.character(.p),
                 "`predicted_two` should be the predictions that knn() returns: one class for each banknote in the test set.")
        .t_check(length(.p) != 2 * 686,
                 "`predicted_two` has a prediction for every row of the data. Classify only the test set: 686 rows.")
        .t_check(length(.p) == 686,
                 "`predicted_two` should have 686 predictions, one for each banknote in the test set. Check what you gave `test`.")
        .g <- .t_sig(.p)
        .t_check(!.t_match(.g, "a5c82bb68be6914b4b685137eef44561d829df336d872fb8bcbb6819971b1c35"),
                 "knn() uses one neighbor unless you set `k`. Set k = 5.")
        .t_check(!.t_match(.g, "f765b76035237da9dd30d8b7a48e8e97168fad86ddbb9b084cfd1d9f3edb628c"),
                 "You classified the training set. Give `test` the attributes of the test set, `test_two`.")
        .t_check(!.t_match(.g, "1fa96247ad9dfae13d7fc383b2049c53b60e7cb36ab7532b479882984ba0e967"),
                 "Those predictions aren't from two attributes. Use `train_two` and `test_two`, and give `cl` the classes of the training set.")
        .t_check(.t_match(.g, "a6f4169155d7d9b4e2b1b15ace2ce105d6b2c26d80eb317dc911119dc94105d8"),
                 "Check each argument of knn(): `train` and `test` are the attributes of the training set and the test set, `cl` is the classes of the training set, and `k` is 5.")
      }
    ),
    ottr::TestCase$new(
      name = "q1_4b",
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
        .p <- .t_get("predicted_two")
        .t_check(!is.null(.p) && length(.p) == 686 && .t_match(.t_sig(.p), "a6f4169155d7d9b4e2b1b15ace2ce105d6b2c26d80eb317dc911119dc94105d8"),
                 "Finish `predicted_two` first, in the second step of this question.")
        .v <- .t_get("accuracy_two")
        .t_check(!is.null(.v),
                 "`accuracy_two` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(.v),
                 "`accuracy_two` is a table. It should be a single number.")
        .t_check(is.numeric(.v) && length(.v) == 1 && !is.na(.v),
                 "`accuracy_two` should be a single number: the proportion of predictions that are right.")
        .acc <- mean(as.character(.p) == as.character(banknote_test$Class))
        .t_check(!isTRUE(all.equal(.v, 100 * .acc)),
                 "Store the accuracy as a proportion between 0 and 1, not as a percent.")
        .t_check(!isTRUE(all.equal(.v, 686 * .acc)),
                 "That's the number of correct predictions. Use mean() instead of sum() to get the proportion.")
        .t_check(!isTRUE(all.equal(.v, 1 - .acc)),
                 "That's the proportion of wrong predictions. Count the predictions that match with ==.")
        .t_check(!isTRUE(all.equal(.v, mean(as.character(.p) == as.character(banknote_train$Class)))),
                 "Compare the predictions with the classes of the test set, `banknote_test$Class`.")
        .t_check(.v >= 0 && .v <= 1,
                 "An accuracy is a proportion, so it is between 0 and 1.")
        .t_check(isTRUE(all.equal(.v, .acc)),
                 "The accuracy is the proportion of predictions that match the actual classes: mean(predicted_two == banknote_test$Class).")
      }
    )
  )
)
