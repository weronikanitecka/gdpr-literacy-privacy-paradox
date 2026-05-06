required_packages <- c(
  "haven",       # loading .sav / .dta Eurobarometer files
  "dplyr",       # data manipulation
  "tidyr",       # reshaping
  "ggplot2",     # figures
  "survey",      # post-stratification weighted regression
  "mediation",   # bootstrap mediation analysis
  "car",         # VIF diagnostics
  "brant"        # proportional odds (Brant test)
)

installed <- rownames(installed.packages())
to_install <- required_packages[!required_packages %in% installed]

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(required_packages, library, character.only = TRUE))
