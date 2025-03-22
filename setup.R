# install and load packages for reproducibility
packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }


# vector of packages to load (add new packages to the end of this list)
packages <- c(
  "shiny",
  "shinydashboard",
  "bslib",
  "DT",
  "readxl",
  "dplyr",
  "ggplot2",
  "plotly",
  "scales",
  "rmarkdown",
  "tidyverse"
)

packageLoad(packages)