# Lab 06 instructor notes (do not distribute)

Lab 6: Correlation and the Regression Line. Due Thursday 5 PM, Week 13. Built from Data 8 Lab 9
(Regression, Old Faithful), in R with dplyr and ggplot2, packages Coding Assignment 5's autograder
has. The data are R's built-in `faithful` (columns `eruptions` and `waiting`), the same 272 eruptions
as Data 8's `faithful.csv`. 11 questions, 20 test cases, all autograded.

It follows the course modules: standard units as in 14.2 (`standard_units()` with `sd()`), $r$ from
standard units as in 15.1, the regression line as in 15.2 (slope and intercept computed once from the
formula, then `lm()` and `coef()` shown as a check), and the RMSE as in 15.3.

## Folder

| Path | What it is |
|---|---|
| `lab06.ipynb` | student notebook |
| `helpers.R`, `ds4all-logo.png` | same as Lab 5 |
| `snooze.jpg` | moral-support image, from `labs/lab06/` in the course repo |
| `tests/` | hashed hub tests, 1 point per case |
| `instructor/lab06_solutions.ipynb` | solutions, run top to bottom with outputs. It reads `tests/` from the lab folder, so copy it up a level to rerun it. |
| `instructor/tests_unhashed/` | the same tests with plain-text answers |
| `instructor/Lab06_Gradescope.zip` | the Gradescope autograder to upload |
| `instructor/autograder/` | the same files unzipped, for reading or rebuilding |

There is no `data/` folder. Keep `instructor/` out of the student repo.

## Answer key

| Q | Variable | Answer |
|---|---|---|
| 1.0 | `least_squares_order` | `c(4, 1, 3, 2)` |
| 1.1 | `faithful_scatter` | `ggplot(faithful) + aes(x = eruptions, y = waiting) + geom_point(alpha = 0.5)`; needs a title and axis labels (not `...`, not the column names) |
| | `shape_answer` | `"A"` (roughly linear and positive) |
| 1.2 | `standard_units()` | `(numbers - mean(numbers)) / sd(numbers)` |
| | `faithful_su` | `mutate(eruptions_su = standard_units(eruptions), waiting_su = standard_units(waiting))` |
| 1.3 | `r_guess` | `"D"` (1) |
| | `faithful_products` | `faithful_su |> mutate(product_su = eruptions_su * waiting_su)` |
| | `r` | `sum(faithful_products$product_su) / (nrow(faithful_products) - 1)` = 0.9008 |
| 2.1 | `slope` | `r * sd(faithful$waiting) / sd(faithful$eruptions)` = 10.7296 |
| | `intercept` | `mean(faithful$waiting) - slope * mean(faithful$eruptions)` = 33.4744 |
| 2.2 | `line_plot` | `faithful_scatter + geom_abline(slope = slope, intercept = intercept, ...)` |
| 3.1 | `two_minute_wait`, `five_minute_wait` | 54.93, 87.12 |
| 3.2 | `faithful_predictions` | `predicted_wait = slope * eruptions + intercept`, `residual = waiting - predicted_wait`; first row 72.10 and 6.90 |
| | `faithful_rmse` | `sqrt(mean(faithful_predictions$residual^2))` = 5.8922 |
| 4.1 | `zero_minute_wait`, `two_point_five_minute_wait`, `hour_wait` | 33.47, 60.30, 677.25 |
| | `trust_answer` | `"A"` (only 2.5 minutes) |
| 5.1 | `below_3`, `above_3` | `filter(eruptions < 3)` (97 rows), `filter(eruptions > 3)` (175 rows) |
| | `below_3_r`, `above_3_r` | 0.2902, 0.3728 |
| | `below_3_slope`, `below_3_intercept` | 6.3505, 41.5516 |
| | `above_3_slope`, `above_3_intercept` | 5.4388, 56.6490 |
| 5.2 | `predict_wait()` | `case_when(duration < 3 ~ below_3_slope * duration + below_3_intercept, .default = above_3_slope * duration + above_3_intercept)`; on 1.5, 2.5, 3.5, 4.5 it gives 51.08, 57.43, 75.68, 81.12 |
| | `accuracy_answer` | `"A"` (more accurate) |

The given cells after 5.2 compare the RMSEs: 5.89 minutes for one line, 5.55 for two.

## How the tests work

- **No randomness.** Every answer is computed from the data, so every value is hashed (SHA-256 with
  `digest::digest(x, algo = "sha256", serialize = FALSE)`) after rounding: $r$, slopes, intercepts, and
  the RMSE to 4 decimals, predictions to 2.
- **The tests see values, not code.** The formula for `r` gives exactly the same number as `cor()`, so
  no test can tell which one a student typed. That's why 1.3 grades the products column: `cor()` can't
  make `product_su`, so the multiplying step is checked. A student who builds `faithful_products` and
  then types `r <- cor(...)` still passes the last step.
- **Other correct ways pass:** the formula typed on `faithful_su` directly; `lm()` and `coef()` for the slope and intercept (named
  numbers are fine); `if_else()` in `predict_wait()`; skipping the `as_tibble()` cell, which leaves the
  built-in data frame.
- **Earlier answers are checked first.** A wrong `r` gives "Fix `r` in Question 1.3 first" in the later
  questions, instead of a wrong-value hint in each. So an early mistake costs every question that
  builds on it until it's fixed: a student who leaves `r` as the average of the products gets 56/100.
  Part 5 doesn't depend on Part 2.
- **Plots** are checked by their layers and mappings: points with `eruptions` on x and `waiting` on y,
  all 272 rows, real title and axis labels. `line_plot` must have a `geom_abline()` layer that uses the
  student's own `slope` and `intercept`.
- **Common mistakes with their own hints:** steps out of order, or typed as strings; axes swapped;
  labels left as column names; missing parentheses in `standard_units()`; the SD that divides by n;
  for `product_su`, the original columns, standard units for only one column, or adding instead of
  multiplying; for `r`, the original columns (divided by n - 1 or by n), standard units for only one
  column, the plain sum, the average of the products (divides by n), `n` never defined (in R, `n` is
  dplyr's `n()` function, so the line errors), and `cor()` of the whole tibble; the SD ratio upside down; the SD ratio without $r$;
  an intercept that forgets the average duration or has the sign flipped; `geom_abline()` with slope and
  intercept swapped, or with the standard-units line; slope and intercept swapped in a prediction;
  residual as predicted minus actual; for the RMSE, the mean squared error, the sum instead of the mean,
  unsquared residuals, the average size of the residuals, `sd()` of the residuals (divides by n - 1),
  and `sigma()` from `lm()` (divides by n - 2); an hour entered as 1; `below_3` and `above_3` swapped; a
  cluster line with the overall $r$ or the overall SDs; `predict_wait()` with `if` (errors on a vector),
  with the lines swapped, or with the single line; `r` left as a one-row tibble.

What I ran: the solutions pass every check; the blank notebook gives a hint at every check, with no test
errors; 42 notebooks with mistakes each get the intended hint, and five with other correct
approaches pass (the formula on `faithful_su` directly, `cor()` for the last step of 1.3, `lm()` with
`coef()`, `if_else()`, no `as_tibble()`). Same results on R 4.3.3 with ggplot2 4.0.3 and dplyr 1.2.1, and with ggplot2 3.4.4 and
dplyr 1.1.4. The hub has R 4.5.1, so do one full run there before release.

## Choices

- Data: R's built-in `faithful` instead of Data 8's CSV (same 272 rows, in the same order). The code
  uses its column names, `eruptions` and `waiting`; the notebook turns it into a tibble with
  `as_tibble()`. No `read_csv()`, so the lab doesn't load readr.
- Question 1.0 (ordering the steps of a regression) is back, now that RMSE comes before this lab. Data
  8's survey is dropped. Data 8's written questions 1.2, 4.2, and 5.4 are multiple choice; Question
  1.4's plot in standard units is a given cell.
- RMSE: 3.2 has a graded `faithful_rmse`, and Part 5 compares one line with two by RMSE (Data 8 suggests
  the average size of the residuals there).
- $r$ comes from the standard units in two graded steps, like 15.1's Steps 2 and 3: the products column,
  then the sum divided by $n - 1$. Dividing by $n - 1$ matches `sd()` in `standard_units()`, `cor()`,
  and Coding Assignment 5's Task 2; the text says 15.1 averaged the products and why this matches
  `cor()`. A given cell then compares `r` with `cor()`.
- Slope and intercept come from the formula once (2.1). `lm()` and `coef()` follow as a check. No
  reusable slope or intercept functions, since students write those in Coding Assignment 5: Data 8's
  `slope_of()` and `intercept_of()` became the 2.1 formula applied to each cluster (5.1).
- `predict_wait()` uses `case_when()` so it works inside `mutate()`; Data 8's `if`/`else` version
  only takes one number at a time.
- The line is drawn with `geom_abline()`, as in Coding Assignment 5's Task 6. `line_plot` builds on
  `faithful_scatter`.
- The video is a link, not an embed.
- Resources: your 15.1, 15.2, and 14.2 links, plus 15.3 (Root Mean Squared Error) for the RMSE. In-text
  links point to sections of `prediction.html`: Calculating r, The Equation of the Regression Line, and
  Root Mean Squared Error. There's no class-notes line.
- Moral support: `snooze.jpg`, "Good work! Have a rest."

## Run time and memory

About 7 seconds top to bottom here. Nothing in this lab is large.

## Gradescope

Upload `instructor/Lab06_Gradescope.zip` (md5 69ebcebe). It uses Otter-Grader 6.1.6 and r-ottr 1.5.2,
like Lab 5's. It was made with `otter generate` and then edited the same way as Labs 4 and 5:

- `tests/` holds the hub's 11 test files, byte-identical to `tests/` in this folder except the `points`
  lines: 100 points in all. Questions 1.0 and 2.2 are worth 5, every other question 10. Two-part
  questions are 5 + 5; Question 1.3 is 3 + 3 + 4 (guess, products, `r`); Question 3.1 is one test case
  worth 10.
- There is no `files/` folder, since the data come with R.
- `requirements.r` installs dplyr, ggplot2, and digest from CRAN, so the image gets the latest versions
  when it builds.
- Environment fixes from Lab 4: conda-forge is the only channel (no defaults, no r),
  `zlib-devel-amzn2-aarch64` is removed, there is no `mamba config` line, and `setup.sh` ends with a
  check that fails the build if otter-env, `import otter`, `import rpy2`, or any of ottr, testthat,
  digest, dplyr, ggplot2 are missing. The check prints the R and package versions in the build log.

Steps: upload the zip as the autograder for "Lab 06", wait for the build, check that the build log ends
with `BUILD CHECK PASSED`, then use Test Autograder with `lab06_solutions.ipynb` (expect 100/100) and
the blank `lab06.ipynb` (expect 0/100).

### How Otter grades this notebook

Same as Lab 5. Otter turns the notebook into an R script and runs it top to bottom, one line at a time,
skipping any line that errors. Every `ottr::check()` that runs is recorded, and the last one for each
question counts. That is normally the final "re-run all the autograder tests" cell, so a student's
Gradescope score is what that cell shows after the notebook runs from top to bottom. Two cases differ
from the hub:

1. **A cell that errors partway.** The hub stops the cell at the error; Otter keeps running the cell's
   other lines. Otter can only give more credit here.
2. **A deleted final cell.** Each question is then graded at its own check cell, so a fix made in a
   later cell doesn't count.

### What was checked

I graded 49 test notebooks two ways and compared every test case:

- **Gradescope path:** `otter run` with this zip (Otter 6.1.6, ottr 1.5.2). This runs Otter's grading
  code without the container.
- **Hub path:** each notebook run top to bottom in the R kernel with the hub's tests (ottr 1.6.0), then
  every test checked at the end.

Both paths ran on the latest releases (ggplot2 4.0.3, dplyr 1.2.1) and again on ggplot2 3.4.4 and dplyr
1.1.4, with the same scores. The two paths matched on 48 of the 49 notebooks:

| Notebook | Hub | Gradescope |
|---|---|---|
| solutions | 100 | 100 |
| blank | 0 | 0 |
| other correct ways: the formula typed on `faithful_su` directly, `cor()` for the last step of 1.3, `lm()` with `coef()`, `if_else()`, no `as_tibble()` | 100 | 100 |
| `standard_units()` without parentheses, or with the SD that divides by n | 43 | 43 |
| `product_su` from the original columns, with one column in standard units, or with the two added | 53 | 53 |
| `r` wrong: average of the products, plain sum, original columns, one column in standard units, `n` never defined, `cor()` of the whole tibble, or a one-row tibble | 56 | 56 |
| SD ratio upside down | 60 | 60 |
| intercept without the average duration, or with the sign flipped | 65 | 65 |
| axes swapped, wrong `shape_answer` | 85 | 85 |
| `below_3` and `above_3` swapped | 85 | 85 |
| one mistake in 3.1, 3.2's residuals, 4.1, or a cluster's line (each) | 90 | 90 |
| one mistake in 1.0, 2.2, 3.2's RMSE, or 5.2; labels left as column names; no title (each) | 95 | 95 |
| typo in the first line of the 3.1 cell | 90 | 90 |
| syntax error in the 3.2 cell | 90 | 90 |
| wrong `r`, fixed in a later cell, final re-run cell kept | 100 | 100 |
| same, final re-run cell deleted | 100 | 96 |

The one difference is the last row.

**Memory.** Each Gradescope-path run took under 4 seconds here and peaked at 300 MB, so Gradescope's
default 768 MB is enough.

## Changing an answer

Edit the plain value in `instructor/tests_unhashed/`, then put the new hash in the matching `tests/`
file: `digest::digest("A", algo = "sha256", serialize = FALSE)`. Numbers are hashed as text, rounded the
way the test rounds them (for example `sprintf("%.4f", x)`). Make the same change in
`instructor/autograder/tests/`, keeping that file's `points` lines, then rebuild the zip from inside
the folder (`cd instructor/autograder && zip -r ../Lab06_Gradescope.zip .`) and upload it again.
