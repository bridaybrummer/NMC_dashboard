
# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

#increase timeout for package install
options(timeout = 300)
#remove the NMCleaner package locally 
remove.packages("NMCleaner")

devtools::install_github("bridaybrummer/NMCleaner", force = TRUE)

library(NMCleaner)


condition_df$condition%>%sort

# Use pacman to install and load packages
pacman::p_load(flextable,
  readxl,
  sf,
  tidyverse,
  dplyr,
  ggplot2,
  ggh4x,
  knitr,
  tinytex,
  haven,
  janitor,
  #summarytools,
  lubridate,
  grates,
  forcats,
  flextable,
  magrittr,
  gtsummary, 
  flextable
  #RecorLinkage
  #openai,
  #DescTools
)

search()

