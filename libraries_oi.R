# #######################################
# Load all necessary packages in addition to policy-rules-database/libraries.R
#########################################

if (!requireNamespace("devtools", quietly=TRUE)) {
  install.packages("devtools")
}

if (!requireNamespace("oiplot", quietly=TRUE)) {
  devtools::install_github('opportunityinsights/oiplot')
}

# Packages used in app.R / validate_parameters.R / functions_oi/
# that are NOT explicitly loaded in policy-rules-database/libraries.R
# (readr, tibble, stringr come via tidyverse in libraries.R, but app.R
#  avoids loading tidyverse for memory, so we list them explicitly)

if (!requireNamespace("readr", quietly=TRUE)) {
  install.packages("readr")
}

if (!requireNamespace("tibble", quietly=TRUE)) {
  install.packages("tibble")
}

if (!requireNamespace("stringr", quietly=TRUE)) {
  install.packages("stringr")
}

if (!requireNamespace("kableExtra", quietly=TRUE)) {
  install.packages("kableExtra")
}

if (!requireNamespace("future", quietly=TRUE)) {
  install.packages("future")
}

if (!requireNamespace("digest", quietly=TRUE)) {
  install.packages("digest")
}

library(oiplot)
library(readr)
library(tibble)
library(stringr)
library(kableExtra)
library(future)
library(digest)

set_oi_theme()
