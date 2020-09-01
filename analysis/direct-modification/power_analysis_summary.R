library(tidyverse)
library(foreach)
# change to results folder
setwd("results/sim_200_bySubj-byItem")
# bind together all summary files
full_data_100 <- list.files(
                    pattern = "direct_mod_power_analysis_fullData_final*"
                   ) %>%
  map_df(~read_csv(.))


full_data_400 <- list.files(
                       pattern = "direct_mod_power_analysis_fullData_final*"
                       ) %>%
  map_df(~read_csv(.))

full_data_500 <- rbind(full_data_100, full_data_400) %>%
  group_by(n.subj, key) %>%
  mutate(
  seed = 1:length(n.subj)
)  %>% ungroup()

full_data_500_summary <- full_data_100 %>%
  filter(key == "syntax_critical") %>%
  mutate(check_syntax = ifelse((lower > 0) | (upper < 0 ), 1, 0)) %>%
  group_by(n.subj, n.item) %>%
  summarise(power_syntax = round(mean(check_syntax), 3))

write_csv(full_data_500_summary, "../direct_mod_power_analysis_bySubj-byItem_4000iter_200sim_summary.csv")

# plot power as n iter increases
n.sims = seq(0, 200, by=20) 

get_intermediate_power = function(N) {
  summary <-  full_data_100 %>% 
    filter(!(n.subj %in% c(50, 100))) %>%
    group_by(n.subj, n.item) %>% slice(1:(10*N)) %>%
    filter(key == "syntax_critical") %>%
    mutate(check_syntax = ifelse((lower > 0) | (upper < 0 ), 1, 0)
    ) %>%
    group_by(n.subj, n.item) %>%
    summarise(power_syntax = round(mean(check_syntax), 3)) %>% mutate(n.sim=N) 
}

my_powers <- foreach(i = n.sims, .combine=rbind) %do% {get_intermediate_power(i)}  

#my_powers <- my_powers %>% mutate(n.subj = as.factor(n.subj))
write_csv(my_powers, "../power_analysis_bySubj-byItem_4000iter_running_power.csv")

my_powers %>% mutate(n.subj = as.factor(n.subj), 
                     n.item = as.factor(n.item)) %>%
ggplot(., aes(x = n.sim, y = power_syntax, color = n.item) ) +
  geom_point() +
  geom_line() +
  ylab("Power") +
  ylim(0.6, 1) +
  xlab("Number of simulations") +
  facet_wrap(~n.subj, ncol = 1) +
  ggtitle("Power as a function of number of simulations")
ggsave("../power_analysis_bySubj-byItem_4000iter_running-power_plot.pdf", width = 5, height = 12)

