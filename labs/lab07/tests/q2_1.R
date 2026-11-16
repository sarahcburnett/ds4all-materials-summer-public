# Lab 07 autograder test. Expected answers are stored as SHA-256 hashes.
test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      name = "q2_1a",
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
        .pg <- .t_get("penguins")
        .t_check(is.data.frame(.pg) && nrow(.pg) == 342 &&
                   all(c("species", "bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g") %in% names(.pg)),
                 "`penguins` isn't the table the lab started with. Run the cell that reads `data/penguins.csv` again.")
        .v <- .t_get("penguins_grouped")
        .t_check(!is.null(.v),
                 "`penguins_grouped` does not exist yet. Fill in the blank and run the cell.")
        .t_check(is.data.frame(.v) && nrow(.v) == 342 && all(names(.pg) %in% names(.v)),
                 "`penguins_grouped` should be `penguins` with one more column. Use mutate().")
        .t_check("group" %in% names(.v),
                 "`penguins_grouped` needs a column named `group`.")
        .g <- .v$group
        .e <- ifelse(.v$species == "Adelie", "Adelie", "Other")
        .t_check(!is.logical(.g),
                 "`group` holds TRUE and FALSE. It should hold the words \"Adelie\" and \"Other\": if_else(condition, value if true, value if false).")
        .t_check(is.character(.g) || is.factor(.g),
                 "`group` should hold the words \"Adelie\" and \"Other\".")
        .g <- as.character(.g)
        .t_check(!anyNA(.g),
                 "`group` has missing values. Give if_else() a value for both cases.")
        .t_check(length(unique(.g)) == 2,
                 "`group` should have exactly two values, \"Adelie\" and \"Other\". Chinstrap and Gentoo penguins both get \"Other\".")
        .t_check(!identical(.g, ifelse(.e == "Adelie", "Other", "Adelie")),
                 "The two groups are swapped. Adelie penguins get \"Adelie\".")
        .t_check(!(identical(tolower(trimws(.g)), tolower(.e)) && !identical(.g, .e)),
                 "Check the spelling and capital letters: the values should be exactly \"Adelie\" and \"Other\".")
        .t_check(identical(.g, .e),
                 "`group` should be \"Adelie\" when `species` is \"Adelie\", and \"Other\" otherwise.")
      }
    )
  )
)
