# Syntax Rating Experiment Analysis

# libraries
library(tidyverse)
library(tidyboot)
library(brms)
library(lmerTest)

# read the data
d_rate <- read_csv('./data/results_exp2-rating.csv')

# exclude participants who report difficulties 
d_rate %>% select(submission_id, comments, problems) %>% distinct() %>% View()
# d_rate <- subset(d_rate, !(submission_id %in% c(....)))

# exclude data from non-native English speakers and those where the language information is missing
d_rate %>% distinct(languages)

d_rate_filt <- d_rate %>% filter(grepl("en", languages, ignore.case = T))

# warmup trials 
d_rate_warmup <- d_rate_filt %>% filter(trial_name == "custom_warmup")

# get IDs of participants who give equal rating of both sentences
d_rate_warmup %>% rowwise() %>% filter(response1 == response2) %>% View()
# subset valid participants
# d_rate_filt <- subset(d_rate_filt, !(submission_id %in% c(....)))

# main trials
d_rate_main <- d_rate_filt %>% filter((trial_name == "custom_slider1") |
                                      (trial_name == "custom_slider2")) %>%
  select(submission_id, trial_number, trial_name, NP, syntax, sentence_order, .... )

# turn NP, syntax to factors 
d_rate_main <- d_rate_main %>% mutate(NP = factor(NP, levels = c(0, 1),
                                                  labels = c("sub", "basic")),
                                      syntax = factor(syntax, 
                                                      levels = c("subject", "predicate")),
                                      order = factor(sentence_order, levels= c("0|1", "1|0"),
                                                     labels = c("prenom_pred", "pred_prenom")))

# plot
bar.width = 0.8
d_rate_main %>%
  group_by(np, congruence, condition) %>% 
  tidyboot_mean(column = response1) %>% 
  ggplot(aes(x = np, y = mean, fill = condition, ymin = ci_lower, ymax = ci_upper)) +
  geom_col(position = position_dodge(bar.width), 
           width = bar.width, alpha = 0.3, color = 'black') +
  geom_point(data = d_rating_full, 
             position = position_jitterdodge(),
             inherit.aes = F, aes(x = np, y = response1, color = condition),
             alpha = 0.25)+
  geom_linerange(position = position_dodge(bar.width))+ 
  facet_wrap(~congruence) +
  xlab("NP position") + 
  ylab("mean sentence rating") +
  ggtitle("Mean ratings by syntactic condition and congruence")

# contrast coding

# lmer model

# Bayesian model 