library(tidyverse)
library(brms)
library(tidybayes)
library(broom)

# add appropriate path
pilot_data <- read_csv("../../data/direct-modification/results_double-modXrefUt-pilot1-2_tidy.csv") %>% 
  filter(!(workerid %in% c(2243, 2245))) # exclude workers where not all trials are included, resulting in 45 unique workers

n_iter = 3000
n_sim = 5

# sum coding of main effects
pilot_data <- pilot_data %>%
  mutate(trial_dev = factor(trial_type, levels = c("filler", "critical")),
         syntax_dev = factor(syntax, levels = c("subj", "pred")))
contrasts(pilot_data$trial_dev) <- contr.sum(2)
contrasts(pilot_data$syntax_dev) <- contr.sum(2)

# fit full desired model on pilot data
pilot_model <- brm(
  response_num ~ syntax_dev * trial_dev + (1 + syntax_dev*trial_dev | workerid)
  + (1 + syntax_dev*trial_dev | target),
  data = pilot_data,
  family = "bernoulli",
  chains = 4,
  iter = n_iter,
  cores = 4,
  control = list(adapt_delta = 0.95)
)

predicted_draws <- pilot_model %>%
  spread_draws(b_Intercept, b_syntax_dev1, b_trial_dev1, `b_syntax_dev1:trial_dev1`) %>%
  mutate(critical_subj = b_Intercept + b_syntax_dev1 - b_trial_dev1 - `b_syntax_dev1:trial_dev1`,
         critical_pred = b_Intercept - b_syntax_dev1 - b_trial_dev1 + `b_syntax_dev1:trial_dev1`,
         syntax_critical = critical_subj - critical_pred,
         filler_subj = b_Intercept + b_syntax_dev1 + b_trial_dev1 + `b_syntax_dev1:trial_dev1`,
         filler_pred = b_Intercept - b_syntax_dev1 + b_trial_dev1 - `b_syntax_dev1:trial_dev1`,
         syntax_filler = filler_subj - filler_pred) %>%
  select(b_Intercept, b_syntax_dev1, b_trial_dev1, `b_syntax_dev1:trial_dev1`, critical_subj, critical_pred, syntax_critical, filler_subj, filler_pred, syntax_filler) %>%
  gather(key, val) %>%
  group_by(key) %>%
  summarise(
    mean = mean(val),
    lower = quantile(val, probs = 0.025),
    upper = quantile(val, probs = 0.975)
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
  `.prediction` ~ syntax_dev * trial_dev + (1 + syntax_dev*trial_dev | workerid)
  + (1 + syntax_dev*trial_dev | target),
  data = predicted_data,
  family = "bernoulli",
  chains = 4,
  iter = n_iter,
  cores = 4,
  control = list(adapt_delta = 0.97)
)

# extract contrasts of interest
predicted_fit_draws <- predicted_fit %>%
  spread_draws(b_Intercept, b_syntax_dev1, b_trial_dev1, `b_syntax_dev1:trial_dev1`) %>%
  mutate(critical_subj = b_Intercept + b_syntax_dev1 - b_trial_dev1 - `b_syntax_dev1:trial_dev1`,
         critical_pred = b_Intercept - b_syntax_dev1 - b_trial_dev1 + `b_syntax_dev1:trial_dev1`,
         syntax_critical = critical_subj - critical_pred, # subject vs predicate
         filler_subj = b_Intercept + b_syntax_dev1 + b_trial_dev1 + `b_syntax_dev1:trial_dev1`,
         filler_pred = b_Intercept - b_syntax_dev1 + b_trial_dev1 - `b_syntax_dev1:trial_dev1`,
         syntax_filler = filler_subj - filler_pred) %>% # subject vs predicate
  select(b_Intercept, b_syntax_dev1, b_trial_dev1, `b_syntax_dev1:trial_dev1`, critical_subj, critical_pred, syntax_critical, filler_subj, filler_pred, syntax_filler) %>%
  gather(key, val) %>%
  group_by(key) %>%
  summarise(
    mean = mean(val),
    lower = quantile(val, probs = 0.025),
    upper = quantile(val, probs = 0.975)
  )

# helper function to get posterior predictive draws for a subset of participants of size N
# d is the a dataframe with posterior predictive draws for 47 participants based on the pilot data set
get_new_data <- function(N, pilot_model, pilot_data) {
  new.data <- tibble()
  workers <- sample(unique(pilot_data$workerid), N, replace = N > 45)
  for(i in 1:N) { new.data <- rbind(new.data, pilot_data %>% filter(workerid == workers[i])) }
  data <- add_predicted_draws(model=pilot_model,
                              newdata = new.data,
                              n = 1) 
  worker_nums <- rep(1:N, each = 8) # 8 is nr of trials per participant
  data[["workerid"]] <- worker_nums
  return(data)
}

# simulate data and updat
# n is number of participants to be added to the pilot baseline of 47, cannot exceed 47
sim_data_fit <- function(seed, N) {
  set.seed(seed)

  # add draws from the posterior predictive distribution, getting one sample per fit (n = 1)
  # predictions are based on pilot data, i.e. grouped by original pilot input rows
  # corresponds to brms::redict.brmsfit()

  # simulate data from original pilot data
  # add draws from the posterior predictive distribution, getting one sample per fit (n = 1)
  # predictions are based on pilot data, i.e. grouped by original pilot input rows
  # corresponds to brms::redict.brmsfit()
  data <- get_new_data(N, pilot_model = pilot_model, pilot_data = pilot_data)

  # deviation code main effects
  contrasts(data$trial_dev) <- contr.sum(2)
  contrasts(data$syntax_dev) <- contr.sum(2)

  # update model fit with new data
  update(predicted_fit,
         newdata = data,
         seed = seed) %>%
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
    )

}

# helper function iterating over seeds (=iterations of the simulation)
sim.power <- function(n.subj, n.sim) {
  print(n.subj)
  sim <- tibble( seed = 1:n.sim) %>%
    mutate(
      tidy = map(seed, sim_data_fit, N = n.subj)
    ) %>%
    unnest(tidy)
}

# iterate over different subject numbers (total of 100 - 300, in steps of 20)
analyse_power <- tibble(n.subj = seq(10, 300, by= 20)) %>%
  mutate(
    tidy = map(n.subj, sim.power, n.sim = n_sim)
  ) %>%
  unnest(tidy)

analyse_power %>%
  filter(key == "syntax_critical") %>%
  mutate(check_syntax = ifelse((lower > 0) | (upper < 0 ), 1, 0)) %>%
  group_by(n.subj) %>%
  summarise(power_syntax = mean(check_syntax),
            `95lower` = quantile(check_syntax, probs = 0.025),
            `95upper` = quantile(check_syntax, probs = 0.975)) -> analyse_power_summary

analyse_power %>% write_csv(paste("results/direct_mod_power_analysis_", n_iter, "iter_", n_sim, "sim.csv", sep = ""))
analyse_power_summary %>% write_csv(paste("results/direct_mod_power_analysis_", n_iter, "iter_", n_sim, "sim_summary.csv", sep = ""))
