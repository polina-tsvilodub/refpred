library(tidyverse)
library(foreach)
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

# plot power as n iter increases
n.sims = seq(50, 500, by=50) 

get_intermediate_power = function(N) {
  summary <-  full_data_500 %>% 
    filter(!(n.subj %in% c(50, 100))) %>%
    group_by(n.subj) %>% slice(1:(10*N)) %>%
    filter(key == "syntax_critical") %>%
    mutate(check_syntax = ifelse((lower > 0) | (upper < 0 ), 1, 0)
    ) %>%
    group_by(n.subj) %>%
    summarise(power_syntax = round(mean(check_syntax), 3)) %>% mutate(n.sim=N) 
}

my_powers <- foreach(i = n.sims, .combine=rbind) %do% {get_intermediate_power(i)}  

my_powers <- my_powers %>% mutate(n.subj = as.factor(n.subj))
#write_csv(my_powers, "results/power_analysis_3000iter_running_power.csv")

ggplot(my_powers, aes(x = n.sim, y = power_syntax, color = n.subj) ) +
  geom_point() +
  geom_line() +
  ylab("Power") +
  ylim(0.6, 1) +
  xlab("Number of simulations") +
  ggtitle("Power as a function of number of simulations")
ggsave("../power_analysis_3000iter_500sim_plot.pdf", width = 5, height = 4)

