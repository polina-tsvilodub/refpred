# NP production experiment analysis

# libraries
library(tidyverse)
library(tidyboot)
library(brms)
library(lmerTest)

# read the data
d_prod <- rbind( read_csv('./data/results_exp1-pred.csv'), 
                  read_csv('./data/results_exp1-prenom.csv')) 

# exclude participants who report difficulties 
d_prod %>% select(submission_id, comments, problems) %>% distinct() %>% View()
# d_prod <- subset(d_prod, !(submission_id %in% c(....)))

# exclude data from non-native English speakers and those where the language information is missing
d_prod %>% distinct(languages)
d_prod_filt <- d_prod %>% 
  filter(grepl("eng", languages, ignore.case = T)) %>%
  select(submission_id, trial_name, trial_number, size, item, botresponse, response,
         condition,  picture) %>% 
  mutate(size=factor(size), syntax = factor(condition))

# exclude participants who need more than 4 attempts per warmup
d_prod_warmup <- d_prod_filt %>% 
  filter( (trial_name == "warmup1") | (trial_name == "warmup2"))
d_prod_warmup %>% group_by(submission_id, trial_number) %>% count() %>%
  ungroup() %>% filter( tidyboot_mean(column = n)/2 > 4)

# main trials 
d_prod_main <- d_prod_filt %>% filter((trial_name =="main1") | (trial_name== "main2")) %>%
  select(submission_id, trial_number, response,  size, item, syntax, condition, picture)

# look at the responses
d_prod_main %>% distinct(response) %>% View()
# exlclude invalid responses 
d_prod_main_valid <- subset(d_prod_main, !(response %in% c(...))) 

# categprize responses
d_prod_main_responseCat <- d_prod_main_valid %>%
  rowwise() %>%
  mutate( 
    response_cat =
      ifelse(
        # to be extended depending on participants'responses
        tolower(response) %in% c("bird", "birds", "dog", "dogs", "fish","fishes",
                                 "flower", "flowers", "tree", "trees", "animal", "plant"),
        "basic", "sub"),
    resp_cat = ifelse(response_cat == "sub", 1, 0),
    response_label = "sub"
  )

# plot
d_prod_main_responseCat %>%
  group_by(syntax, response_label) %>%
  tidyboot_mean(column = resp_cat) %>% # calculate proportion of subordinate labels in the different conditions 
  ungroup() %>%
  mutate(syntax = factor(syntax, 
                         levels = c("prenominal", "predicative"),
                         labels= c("Predicate NP", "Subject NP"))
  ) %>% 
  ggplot(aes(x = syntax, fill = syntax,
             y = mean, ymin = ci_lower, ymax = ci_upper)) +
  geom_col(position = position_dodge(0.8)) +
  geom_linerange(position = position_dodge(0.8)) +
  labs( y = "Proportion subordinate responses") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  ggtitle("The proportion of subordinate responses by syntactic condition")

# contrast coding of syntax 
# subject NP -1. predicate NP 1
contrasts(d_prod_main_responseCat$syntax) = matrix(c(-1,1))

# lmer model
prod.lm.fit <- glmer(resp_cat ~  syntax + (1 + syntax || submission_id) + 
                       (1 + syntax ||picture), 
                     data = d_prod_main_responseCat, 
                     family="binomial", REML = F)
summary(prod.lm.fit)

# Bayesian model

# set priors
Prior <- c(set_prior("beta(1,1)", class = "b", lb = 0, ub = 1),
  set_prior("student_t(3,0,10)", class = "sd"))

b.prod.lm <- brm(resp_cat ~   0 + syntax + (1 + syntax | submission_id) +
                      (1 + syntax | item), 
                    data = d_prod_main_responseCat, 
                    family = "bernoulli",
                    prior = Prior, 
                    sample_prior = T)

summary(b.prod.lm)

# nullhypothesis that proportions of subordinate responses do not differ
h1_1 <- hypothesis(b.prod.lm, "0 - syntaxpredicative = 0 + syntaxprenominal") 

print(h1_1, digits = 3)

# probability of alternative hypothesis given the posterior compared to the prior
1/h1_1$hypothesis$Evid.Ratio