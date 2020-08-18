library(tidyverse)
# change to results folder
setwd("results/sim_100")
# bind together all summary files
full_data_100 <- list.files(
                    pattern = "direct_mod_power_analysis_fullData_final*"
                   ) %>%
  map_df(~read_csv(.))


full_data_400 <- list.files(
                       pattern = "direct_mod_power_analysis_fullData_final*"
                       ) %>%
  map_df(~read_csv(.))

full_data_500 <- rbind(full_data_100, full_data_400)

full_data_500_summary <- full_data_500 %>%
  filter(key == "syntax_critical") %>%
  mutate(check_syntax = ifelse((lower > 0) | (upper < 0 ), 1, 0)) %>%
  group_by(n.subj) %>%
  summarise(power_syntax = round(mean(check_syntax), 3))

write_csv(full_data_500_summary, "../direct_mod_power_analysis_fullData_3000iter_500sim_summary.csv")

