# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q2_4",
  cases = list(
    ottr::TestCase$new(
      name = "q2_4a",
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
        .tr <- .t_get("penguin_train")
        .te <- .t_get("penguin_test")
        .xa <- .t_get("penguin_train_attributes")
        .xe <- .t_get("penguin_test_attributes")
        .cols <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")
        .t_check(is.data.frame(.tr) && is.data.frame(.te) && nrow(.tr) == 171 && nrow(.te) == 171 &&
                   all(c("species", .cols) %in% names(.tr)) && all(c("species", .cols) %in% names(.te)) &&
                   .t_match(.t_sig(.tr$species), "c78ab2e78633d98a4f11c833ddec298edc0e43638241757251fe096576d709e1") && .t_match(.t_sig(.te$species), "f148819e5cb8fbd286469fae13e86123d0c886947a33629df346c5bd56529066") &&
                   .t_match(.t_num(sum(.tr$bill_length_mm), 4), "1e7577e8647ff6eef5d51170d5292106be81a05da6426f44e6d4078b2aef171f") &&
                   is.data.frame(.xa) && is.data.frame(.xe) && identical(names(.xa), .cols) && identical(names(.xe), .cols) &&
                   .t_same(.xa, .tr[.cols]) && .t_same(.xe, .te[.cols]),
                 "The training and test sets of penguins aren't the ones the autograder expects. Run the cell with set.seed(7) above Question 2.2 again, and leave the seed as it is.")
        .v <- .t_get("train_accuracy_k1")
        .t_check(!is.null(.v),
                 "`train_accuracy_k1` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(.v),
                 "`train_accuracy_k1` is a table. It should be a single number.")
        .t_check(is.numeric(.v) && length(.v) == 1 && !is.na(.v),
                 "`train_accuracy_k1` should be a single number: the proportion of predictions that are right.")
        .a <- .v
        .v <- .t_get("test_accuracy_k1")
        .t_check(!is.null(.v),
                 "`test_accuracy_k1` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(.v),
                 "`test_accuracy_k1` is a table. It should be a single number.")
        .t_check(is.numeric(.v) && length(.v) == 1 && !is.na(.v),
                 "`test_accuracy_k1` should be a single number: the proportion of predictions that are right.")
        .b <- .v
        .t_check(.a <= 1 && .b <= 1 && .a >= 0 && .b >= 0,
                 "Store each accuracy as a proportion between 0 and 1, not as a percent or a count.")
        .ka <- .t_num(.a, 4)
        .kb <- .t_num(.b, 4)
        .t_check(!(.t_match(.ka, "5e5b3ab30bbe758aed9bfafe28aa076715d233b0231ea533e107a5b4a4f40fa1") && .t_match(.kb, "64e4b892473fb47ab1d8d02f64d984d001ccb5be27756a14481122eac48a8333")),
                 "The two accuracies are swapped. `train_accuracy_k1` is for the penguins in the training set.")
        .t_check(!.t_match(.ka, "4622ae06883e6c425a1e69932928efed03a8fb163cbd8203bc01dfddac708c97"),
                 "For `train_accuracy_k1`, that's the accuracy with five neighbors. Set k = 1.")
        .t_check(!.t_match(.ka, "5e5b3ab30bbe758aed9bfafe28aa076715d233b0231ea533e107a5b4a4f40fa1"),
                 "`train_accuracy_k1` is the accuracy on the test set. Give `test` the attributes of the training set, and compare with `penguin_train$species`.")
        .t_check(.t_match(.ka, "64e4b892473fb47ab1d8d02f64d984d001ccb5be27756a14481122eac48a8333"),
                 "For `train_accuracy_k1`: knn(train = penguin_train_attributes, test = penguin_train_attributes, cl = penguin_train$species, k = 1), compared with `penguin_train$species`.")
        .t_check(!.t_match(.kb, "403db54ad24622de30029c614e1d5aa94d1498ae13037759f5f29fff38dfc6ab"),
                 "For `test_accuracy_k1`, that's the accuracy with five neighbors. Set k = 1.")
        .t_check(!.t_match(.kb, "64e4b892473fb47ab1d8d02f64d984d001ccb5be27756a14481122eac48a8333"),
                 "`test_accuracy_k1` is the accuracy on the training set. Give `test` the attributes of the test set, and compare with `penguin_test$species`.")
        .t_check(.t_match(.kb, "5e5b3ab30bbe758aed9bfafe28aa076715d233b0231ea533e107a5b4a4f40fa1"),
                 "For `test_accuracy_k1`: the same knn() as in Question 2.3 with k = 1, compared with `penguin_test$species`.")
      }
    ),
    ottr::TestCase$new(
      name = "q2_4b",
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
        .t_check(exists("training_answer") && !is.null(training_answer),
                 "`training_answer` is still NULL. Replace the placeholder with your code.")
        .t_check(is.character(training_answer) && length(training_answer) == 1,
                 "Set `training_answer` to one letter in quotes, like \"A\".")
        .a <- toupper(trimws(training_answer))
        .t_check(.a %in% c("A", "B", "C", "D"),
                 "Set `training_answer` to one of the letters A, B, C, or D, in quotes.")
        .t_check(.t_match(.a, "df7e70e5021544f4834bbee64a9e3789febc4be81470df629cad6ddb03320a5c"),
                 "Not quite. Pick one penguin in the training set. Which point in the training set is closest to it?")
      }
    )
  )
)
