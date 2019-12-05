# Syntax Rating Experiment Analysis

# libraries
library(tidyverse)
library(tidyboot)
library(brms)
library(lmerTest)

# read the data
d_rate <- read_csv('./data/results.csv')

d_rate <- d_rate[!(names(d_rate) %in% c("worker_id", "hit_id", "assignment_id"))]
# exclude participants who report difficulties 
d_rate %>% select(submission_id, comments, problems) %>% distinct() %>% View()
# d_rate <- subset(d_rate, !(submission_id %in% c(....)))

# exclude data from non-native English speakers and those where the language information is missing
d_rate %>% distinct(languages)

d_rate_filt <- d_rate %>% filter(grepl("en", languages, ignore.case = T))

# warmup trials 
d_rate_warmup <- d_rate %>% filter(trial_name == "custom_slider_warmup")

# get IDs of participants who give equal rating of both sentences
d_rate_warmup %>% rowwise() %>% filter(response1 == response2) %>% View()
# subset valid participants
# d_rate_filt <- subset(d_rate_filt, !(submission_id %in% c(....)))

# main trials
d_rate_main <- d_rate %>% filter((trial_name == "custom_slider1") |
                                      (trial_name == "custom_slider2")) #%>%
  #select(submission_id, trial_number, trial_name, NP, syntax, sentence_order, .... )

# turn NP, syntax to factors 
d_rate_main <- d_rate_main %>% mutate(NP = factor(np, levels = c(0, 1),
                                                  labels = c("sub", "basic")),
                                      target_size = factor(target_size, levels = c(0, 1),
                                                           labels = c("small", "big")))

d_rate_main_subj1 <- d_rate_main %>% filter(first_condition == "pred") %>% mutate(syntax = "subject") # response1 is for subject NP condition in subj/pred order
d_rate_main_pred1 <- d_rate_main %>% filter(first_condition == "pred") %>%
  mutate(response1 = response2, syntax = "predicate") # response1 for predicate NP condition
d_rate_main_pred2 <- d_rate_main %>% filter(first_condition == "prenom")  %>% mutate(syntax = "predicate")# response1 for predicate NP condition in pred/subj order
d_rate_main_subj2 <- d_rate_main %>% filter(first_condition == "prenom")  %>% 
  mutate(response1 = response2, syntax = "subject") # response1 for subject NP condition
# check is there are differences by order of sliders
d_rate_main_full <- rbind(rbind(d_rate_main_subj1, d_rate_main_pred1), rbind(d_rate_main_pred2, d_rate_main_subj2)) %>% 
  select(submission_id, trial_name, trial_number, syntax, NP, target_size, sentence_order, response1, domain, item)
d_rate_main_full %>% group_by(syntax) %>% tidyboot_mean(response1)

# plot
bar.width = 0.8
d_rate_main_full %>%
  group_by(NP, syntax) %>% 
  tidyboot_mean(column = response1) %>% 
  ggplot(aes(x = NP, y = mean, fill = syntax, ymin = ci_lower, ymax = ci_upper)) +
  geom_col(position = position_dodge(bar.width), 
           width = bar.width, alpha = 0.3, color = 'black') +
  geom_point(data = d_rate_main_full, 
             position = position_jitterdodge(),
             inherit.aes = F, aes(x = NP, y = response1, color = syntax),
             alpha = 0.25)+
  geom_linerange(position = position_dodge(bar.width))+ 
  xlab("NP position") + 
  ylab("mean sentence rating") +
  ggtitle("Mean ratings by syntactic condition")

# contrast coding
contrasts(d_rate_main_full$syntax) = matrix(c(-1,1))
contrasts(d_rate_main_full$NP) = matrix(c(-1,1))
# lmer model
lm.fit.full <- glmer(response1 ~ condition * np * congruence + 
                       (1 + condition*np*congruence || submission_id) + 
                       (1 + condition*np*congruence  || item),
                       data = d_rate_main_full,
                       REML = F)

summary(lm.fit.full)
# Bayesian model 
b.lm.fir <- brm(response1 ~ condition * np * congruence + 
                  (1 + condition*np*congruence || submission_id) + 
                  (1 + condition*np*congruence  || item),
                data = d_rate_main_full)