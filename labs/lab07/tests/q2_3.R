# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q2_3",
  cases = list(
    ottr::TestCase$new(
      name = "q2_3a",
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
        .p <- .t_get("species_predicted")
        .t_check(!is.null(.p),
                 "`species_predicted` does not exist yet. If knn() gave an error about NA or NaN, `train` or `test` still has a class column in it: knn() needs attributes only.")
        .t_check(!is.data.frame(.p),
                 "`species_predicted` is a table. It should be the vector of predictions that knn() returns.")
        .t_check(is.factor(.p) || is.character(.p),
                 "`species_predicted` should be the predictions that knn() returns: one class for each penguin in the test set.")
        .t_check(length(.p) != 2 * 171,
                 "`species_predicted` has a prediction for every row of the data. Classify only the test set: 171 rows.")
        .t_check(length(.p) == 171,
                 "`species_predicted` should have 171 predictions, one for each penguin in the test set. Check what you gave `test`.")
        .t_check(!("Other" %in% as.character(.p)),
                 "Those predictions are the two groups. For this question, give `cl` the three species: `penguin_train$species`.")
        .g <- .t_sig(.p)
        .t_check(!.t_match(.g, "0b78d4a051a5f1133888816996725b7603785f5bb1ad0da6e3c351cd4127bcde"),
                 "knn() uses one neighbor unless you set `k`. Set k = 5.")
        .t_check(.t_match(.g, "f9632635ed1b165644c223714342e597866638e2cbf6a60d34cc45c4f4f263b1"),
                 "Check each argument of knn(): train = penguin_train_attributes, test = penguin_test_attributes, cl = the training set's `species` column, and k = 5.")
      }
    ),
    ottr::TestCase$new(
      name = "q2_3b",
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
        .p <- .t_get("species_predicted")
        .t_check(!is.null(.p) && length(.p) == 171 && .t_match(.t_sig(.p), "f9632635ed1b165644c223714342e597866638e2cbf6a60d34cc45c4f4f263b1"),
                 "Finish `species_predicted` first.")
        .v <- .t_get("species_accuracy")
        .t_check(!is.null(.v),
                 "`species_accuracy` is still NULL. Replace the placeholder with your code.")
        .t_check(!is.data.frame(.v),
                 "`species_accuracy` is a table. It should be a single number.")
        .t_check(is.numeric(.v) && length(.v) == 1 && !is.na(.v),
                 "`species_accuracy` should be a single number: the proportion of predictions that are right.")
        .acc <- mean(as.character(.p) == as.character(.te$species))
        .t_check(!isTRUE(all.equal(.v, 100 * .acc)),
                 "Store the accuracy as a proportion between 0 and 1, not as a percent.")
        .t_check(!isTRUE(all.equal(.v, 171 * .acc)),
                 "That's the number of correct predictions. Use mean() instead of sum() to get the proportion.")
        .t_check(isTRUE(all.equal(.v, .acc)) || !isTRUE(all.equal(.v, mean(as.character(.p) == as.character(.te$group)))),
                 "Compare the predictions with the species of the test set, `penguin_test$species`, not with the groups.")
        .t_check(isTRUE(all.equal(.v, .acc)) || !isTRUE(all.equal(.v, mean(as.character(.p) == as.character(.tr$species)))),
                 "Compare the predictions with the species of the test set, `penguin_test$species`.")
        .t_check(isTRUE(all.equal(.v, .acc)),
                 "The accuracy is the proportion of predictions that match the actual species: mean(species_predicted == penguin_test$species).")
      }
    )
  )
)
