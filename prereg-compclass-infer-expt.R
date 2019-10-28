# Comparison Class Inference Experiment Analysis
# libraries
library(tidyverse)
library(tidyboot)
library(brms)
library(lmerTest)

# read data
d_infer <- rbind( read_csv('./data/results_exp3-infer-pred.csv'), 
                  read_csv('./data/results_exp3-infer-prenom.csv')) 

# exclude participants who report difficulties 
d_infer %>% select(submission_id, comments, problems) %>% distinct() %>% View()
# d_infer <- subset(d_infer, !(submission_id %in% c(....)))

# exclude data from non-native English speakers and those where the language information is missing
d_infer %>% distinct(languages)
d_infer_filt <- d_infer %>% 
  filter(grepl("eng", languages, ignore.case = T)) %>%
  select(submission_id, trial_name, trial_number, size, item, botresponse, response,
         condition,  picture, context, NP)

# exclude participants who need more than 4 attempts per warmup
d_infer_warmup <- d_infer_filt %>% 
  filter( (trial_name == "warmup1") | (trial_name == "warmup2"))
d_infer_warmup %>% group_by(submission_id, trial_number) %>% count() %>%
  ungroup() %>% filter ( tidyboot_mean(column = n)/2 > 4)

d_infer_main <- d_infer_filt %>% filter((trial_name == "custom_main_text1")| 
                                          (trial_name == "custom_main_text2")) %>%
  mutate(context = factor(pic_spec, levels = c(0, 1), 
                          labels = c("basic-level parade", 
                                     "subordinate parade")),
         NP = ifelse(ref_spec == 2, "one", 
                     ifelse(ref_spec == 1, "subordinate", "basic")) ) %>%
  select(submission_id, trial_number, ref_spec, context, item, response, condition,
         context_picture, target_size, NP)

# categorize responses
d_infer_main %>% distinct(response) %>% View()
d_infer_main_responseCat <- d_infer_main %>%
  rowwise() %>%
  mutate(  
    response_cat =
      ifelse( # do be extended dependent on responses provided
        tolower(response) %in% c("birds", "bird", "dogs", "fish", "flowers", 
                                 "flower", "trees","tree", "animal",
                                 "fishes", "dog", "plant"), "basic", "sub"),
    
    response_num = ifelse(response_cat == "sub", 0, 1),
    response_label = "basic"
  )

# plot
bar.width = 0.8
d_infer_main_responseCat %>%  
  group_by(response_label, context, NP, condition) %>%
  tidyboot_mean(column = response_num) %>% ungroup() %>% 
  mutate(condition = factor(condition, levels = c("prenominal",  "predicative"), 
                            labels = c("Predicate",  "Subject"))) %>%
  ggplot(aes(x=condition, y = mean, fill = NP, ymin = ci_lower, ymax = ci_upper)) +
  geom_col(position = position_dodge(bar.width), width = bar.width) +
  geom_linerange(position = position_dodge(bar.width)) + 
  xlab("NP condition") +
  ylab("proportion of basic-level responses")+
  facet_grid(~context)  

# contrast coding
contrasts(d_infer_main_responseCat$syntax) = matrix(c(-2/3, 1/3, 1/3, -1/3, -1/3, 2/3),
                                                    ncol = 2)

# lmer model
lm.infer.fit <- glmer(response_num ~ condition*NP*context  + 
                      (1 + condition*NP*context || submission_id) + 
                      (1 + condition*NP*context || item), 
                      data = d_infer_main_responseCat, 
                      family = "binomial" , REML = F)
summary(lm.infer.fit)

# Bayesian
lm.b.infer.fit <- brm(response_num ~ condition*NP*context  + 
                        (1 + condition*NP*context || submission_id) + 
                        (1 + condition*NP*context || item), 
                      data = d_infer_main_responseCat, 
                      family = "bernoulli" )
summary(lm.b.infer.fit)