# R packages for Lab 6: the two packages the notebook loads, plus digest for the hashed answer
# checks. These install from CRAN when Gradescope builds the image, so they are the latest versions.
install.packages(
  c("dplyr", "ggplot2", "digest"),
  repos = "https://cloud.r-project.org",
  Ncpus = max(1L, parallel::detectCores())
)
