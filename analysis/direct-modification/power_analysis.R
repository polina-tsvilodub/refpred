library(tidyverse)
library(brms)
library(tidybayes)
library(broom)


# list of subject-Ns; argument passed is the index accessing the respective N
subjN_list <- c(100, 150, 200, 250, 300, 350, 400, 450, 500, 600)
Nitems_list <- c(5, 6, 8, 10)

# get the argument passed (assuming the index is the first one after --args)
subjN <- commandArgs(trailingOnly = T)[1]
itemN <- commandArgs(trailingOnly = T)[2]
subjN <- as.numeric(subjN)
itemN <- as.numeric(itemN)
# get the N subj at the position in list corresponding the passed index
currentSubj.N <- subjN_list[subjN]
currentItem.N <- Nitems_list[itemN]
print(paste("Number of subjects", currentSubj.N, sep=" "))
print(paste("Number of items", currentItem.N, sep=" "))

# add appropriate path
pilot_data <- read_csv("../../data/direct-modification/results_all_pilots_tidy.csv")
  #read_csv("../../data/direct-modification/results_double-modXrefUt-pilot1-2_tidy.csv") %>% 
 # filter(!(workerid %in% c(2243, 2245))) # exclude workers where not all trials are included, resulting in 45 unique workers

n_iter = 3000
n_sim = 5

# sum coding of main effects
pilot_data <- pilot_data %>%
  mutate(trial_dev = factor(trial_dev, levels = c("filler", "critical")),
         syntax_dev = factor(syntax_dev, levels = c("subj", "pred")))
contrasts(pilot_data$trial_dev) <- contr.sum(2)
contrasts(pilot_data$syntax_dev) <- contr.sum(2)

# fit full desired model on pilot data
pilot_model <- brm(
  response_num ~ syntax_dev * trial_dev + (1 + syntax_dev*trial_dev || workerid)
  + (1 + syntax_dev*trial_dev || target),
  data = pilot_data,
  family = "bernoulli",
  chains = 4,
  iter = n_iter,
  cores = 4,
  control = list(adapt_delta = 0.95),
  silent = T,
  refresh = 0
)

# add draws from the posterior predictive distribution, getting one sample per fit (n = 1)
# predictions are based on pilot data, i.e. grouped by original pilot input rows
# corresponds to brms::redict.brmsfit()

# the new draws are added in the column .prediction
# simulates 45 participants, as in the original dataset
predicted_data <- add_predicted_draws(model=pilot_model, newdata = pilot_data, n = 1) %>%
  mutate(workerid = paste(workerid, letters[1], sep = "_")) # mutate the workerid to simulate adding more participants

# deviation code main effects
contrasts(predicted_data$trial_dev) <- contr.sum(2)
contrasts(predicted_data$syntax_dev) <- contr.sum(2)

predicted_fit <- brm(
  `.prediction` ~ syntax_dev * trial_dev + (1 + syntax_dev*trial_dev || workerid)
  + (1 + syntax_dev*trial_dev || target),
  data = predicted_data,
  family = "bernoulli",
  chains = 4,
  iter = n_iter,
  cores = 4,
  control = list(adapt_delta = 0.95),
  silent = T,
  refresh = 0
)


# helper function to get 4 out of 5 possible target pairs per participant 
get_targets <- function(N, items){
  targets <- sample(c("sunflower_dandelion", "redwood_bonsai", "doberman_chihuahua", "Great Dane_pug", "eagle_hummingbird", "item1_item2", "item3_item4", "item5_item6", "item7_item8", "item9_item10")[1:items], 4, replace = F)
  unlist(str_split(targets, pattern="_"))
}

# helper function to get posterior predictive draws for participant number N
get_new_data <- function(N, pilot_model, pilot_data, items) {
  
  trials <- rep(c("critical", "filler"), each = 4)
  syntax <- rep(c("subj", "subj", "pred", "pred"), times = 2)
  
  new.data <- tibble(
    workerid = 1:N,
    target = map(workerid, get_targets, items)
  ) %>%
    unnest(cols = target) %>%
    mutate(
      trial_dev = as.factor(rep(trials, times = N)),
      syntax_dev = as.factor(rep(syntax, times = N))
    )
  
  data <- add_predicted_draws(model=pilot_model,
                              newdata = new.data,
                              allow_new_levels = T, # allow sampling new workerids and new items
                              sample_new_levels = "gaussian", # sample new REs based on estimated REs
                              n = 1)
  return(data)
}

# create file path for streaming output
stream_out <- paste("results/direct_mod_power_analysis_fullData_stream_", currentSubj.N, "subj_", currentItem.N, "items_", n_iter,  "iter_", n_sim, "sim.csv", sep="")

# simulate data and update model fit
sim_data_fit <- function(seed, N, items) {
  set.seed(seed)
  print(paste("iteration:", seed, " , subjects:", N, "items:", items, sep=" "))
  
  # predictions are based on pilot data, i.e. grouped by original pilot input rows
  # corresponds to brms::predict.brmsfit()
  data <- get_new_data(N, pilot_model = pilot_model, pilot_data = pilot_data, items = items) %>%
    mutate(trial_dev = factor(trial_dev, levels = c("filler", "critical")),
           syntax_dev = factor(syntax_dev, levels = c("subj", "pred")))

  # deviation code main effects
  contrasts(data$trial_dev) <- contr.sum(2)
  contrasts(data$syntax_dev) <- contr.sum(2)

  # update model fit with new data
  update(predicted_fit,
         newdata = data,
         seed = seed,
         cores = 4,
         silent = T,
         refresh = 0
         ) %>%
    # extract posterior draws
    spread_draws(b_Intercept, b_syntax_dev1, b_trial_dev1, `b_syntax_dev1:trial_dev1`) %>%
    # extract contrasts of interest, especially effect of syntax by-trial
    mutate(critical_subj = b_Intercept + b_syntax_dev1 - b_trial_dev1 - `b_syntax_dev1:trial_dev1`,
           critical_pred = b_Intercept - b_syntax_dev1 - b_trial_dev1 + `b_syntax_dev1:trial_dev1`,
           syntax_critical = critical_subj - critical_pred, # subject vs pred syntax
           filler_subj = b_Intercept + b_syntax_dev1 + b_trial_dev1 + `b_syntax_dev1:trial_dev1`,
           filler_pred = b_Intercept - b_syntax_dev1 + b_trial_dev1 - `b_syntax_dev1:trial_dev1`,
           syntax_filler = filler_subj - filler_pred) %>% # subject vs predicate syntax
    select(b_Intercept, b_syntax_dev1, b_trial_dev1, `b_syntax_dev1:trial_dev1`, critical_subj, critical_pred, syntax_critical, filler_subj, filler_pred, syntax_filler) %>%
    gather(key, val) %>%
    group_by(key) %>%
    # compute summary statistics
    summarise(
      mean = mean(val),
      lower = quantile(val, probs = 0.025),
      upper = quantile(val, probs = 0.975)
    ) %>%
    tibble() %>%
    mutate(
      n.subj = N,
      n.item = items, 
      seed = seed
    ) %>%
    # write out results for each subject N and each seed
    write_csv(., stream_out, append = T, col_names = (seed == 1)) %>% select(-n.subj, -seed, -n.item)
}

# helper function iterating over seeds (=iterations of the simulation)
sim.power <- function(n.subj, n.sim, n.item) {
  print(n.subj)
  sim <- tibble( seed = 1:n.sim) %>%
    mutate(
      tidy = map(seed, sim_data_fit, N = n.subj, items = n.item)
    ) %>%
    unnest(tidy)
}

# iterate over subject number passed to script 
analyse_power <- tibble(n.subj = currentSubj.N, n.item = currentItem.N) %>%
  mutate(
    tidy = map(n.subj, sim.power, n.sim = n_sim, n.item)
  ) %>%
  unnest(tidy)

write_csv(analyse_power, paste("results/direct_mod_power_analysis_fullData_final_", currentSubj.N, "subj_", currentItem.N, "items_", n_iter,  "iter_", n_sim, "sim.csv", sep=""), append = T, col_names = T)

analyse_power %>%
  filter(key == "syntax_critical") %>%
  mutate(check_syntax = ifelse((lower > 0) | (upper < 0 ), 1, 0)) %>%
  group_by(n.subj, n.item) %>%
  summarise(power_syntax = mean(check_syntax)) -> analyse_power_summary

analyse_power_summary %>% write_csv(paste("results/direct_mod_power_analysis_fullData_", currentSubj.N, "subj_", currentItem.N, "items_", n_iter, "iter_", n_sim, "sim_summary.csv", sep = ""))
