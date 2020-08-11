library(tidyverse)
# change to results folder
setwd("results/")
# bind together all summary files
data <- list.files(
                    pattern = "*summary.csv"
                   ) %>%
  map_df(~read_csv(.)) %>%
  write_csv(., "direct_mod_power_analysis_grand_summary.csv")
