packs <- c(
  "DT",
  "dplyr",
  "highcharter",
  "janitor",
  "lubridate",
  "magrittr",
  "purrr",
  "readxl",
  "scales",
  "shiny",
  "shinydashboard",
  "stringr",
  "tidyr"
)

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(packs, install_if_missing))

source("/app/cleaning_script.R")
