Direct Modification Forced Choice Pilot
================
Polina Tsvilodub
4/9/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidyboot)
library(brms)
```

    ## Loading required package: Rcpp

    ## Loading 'brms' package (version 2.13.0). Useful instructions
    ## can be found by typing help('brms'). A more detailed introduction
    ## to the package is available through vignette('brms_overview').

    ## 
    ## Attaching package: 'brms'

    ## The following object is masked from 'package:stats':
    ## 
    ##     ar

``` r
d <- read_csv("~/Documents/Research/refpred/data/direct-modification/results_41_double-mod-FC-wFilers-noTarget-pilot_N50.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   submission_id = col_double(),
    ##   experiment_id = col_double(),
    ##   enjoyment = col_double(),
    ##   trials = col_double(),
    ##   age = col_double(),
    ##   RT = col_double(),
    ##   trial_number = col_double(),
    ##   startTime = col_double(),
    ##   attempts = col_double(),
    ##   fairprice = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
#d %>% select(-prolific_id) %>% write_csv("~/Documents/Research/refpred/data/direct-modification/results_41_double-mod-FC-wFilers-noTarget-pilot_N50.csv")
```

``` r
# exclusions
d %>% distinct(comments) 
```

    ## # A tibble: 16 x 1
    ##    comments                                                                     
    ##    <chr>                                                                        
    ##  1  <NA>                                                                        
    ##  2 "I still don't understand the \"(Not shown)\" in the description"            
    ##  3 "n/a"                                                                        
    ##  4 "N/A"                                                                        
    ##  5 "Enjoyed the study."                                                         
    ##  6 "I saw two ways to answer each question. I am unsure if there was a wrong an…
    ##  7 "N/A. Thank you!"                                                            
    ##  8 "Thanks."                                                                    
    ##  9 "none"                                                                       
    ## 10 "NOTHING"                                                                    
    ## 11 "No"                                                                         
    ## 12 "None"                                                                       
    ## 13 "No comments to add."                                                        
    ## 14 "no"                                                                         
    ## 15 "This made me realize that I don't consciously recognize which subject I'm d…
    ## 16 "it felt difficult to decide which one without further context, thought of a…

``` r
d %>% distinct(problems) 
```

    ## # A tibble: 16 x 1
    ##    problems                                                                     
    ##    <chr>                                                                        
    ##  1 No                                                                           
    ##  2 No.                                                                          
    ##  3 No glitches                                                                  
    ##  4 n/a                                                                          
    ##  5 <NA>                                                                         
    ##  6 no                                                                           
    ##  7 None                                                                         
    ##  8 Nope!                                                                        
    ##  9 none                                                                         
    ## 10 NO                                                                           
    ## 11 There were no problems                                                       
    ## 12 No glitches. Smooth sailing.                                                 
    ## 13 None at all.                                                                 
    ## 14 no it all went well                                                          
    ## 15 No it just took a while for the survey to start and i thought it was a site …
    ## 16 No, everything ran smoothly.

``` r
d %>% distinct(languages)
```

    ## # A tibble: 7 x 1
    ##   languages          
    ##   <chr>              
    ## 1 English            
    ## 2 English, Filipino  
    ## 3 <NA>               
    ## 4 english            
    ## 5 ENGLISH            
    ## 6 BANGALI            
    ## 7 English, Vietnamese

``` r
d %>% distinct(submission_id) %>% count() %>% pull()
```

    ## [1] 50

``` r
d_native <- d %>% filter(grepl("en", languages, ignore.case = T))
d_native %>% distinct(submission_id) %>% count() %>% pull()
```

    ## [1] 47

``` r
# only accept workers who get the paraphrase warmup trial with max. 3 attempts
d_warmup_catch <- d_native %>% filter(trial_name == "comp_class_warmup") %>% group_by(submission_id) %>% filter(attempts > 3)
d_clean <- anti_join(d_native, d_warmup_catch, by = "submission_id")
# nr of subjects excluded based on CC warmup trial
d_clean %>% distinct(submission_id) %>% count() %>% pull()
```

    ## [1] 46

``` r
d_main <- d_clean %>% filter(!is.na(trial_type)) 

d_main %>% count(trial_type, item_noun, syntax)
```

    ## # A tibble: 40 x 4
    ##    trial_type item_noun          syntax     n
    ##    <chr>      <chr>              <chr>  <int>
    ##  1 critical   birds_rescue       pred      15
    ##  2 critical   birds_rescue       subj      24
    ##  3 critical   buildings_landmark pred      19
    ##  4 critical   buildings_landmark subj      21
    ##  5 critical   dogs1_rescue       pred      22
    ##  6 critical   dogs1_rescue       subj      14
    ##  7 critical   dogs1_service      pred      19
    ##  8 critical   dogs1_service      subj      17
    ##  9 critical   dogs2_present      pred      22
    ## 10 critical   dogs2_present      subj      16
    ## # … with 30 more rows

``` r
d_main %>% count(trial_type, syntax, adj)
```

    ## # A tibble: 8 x 4
    ##   trial_type syntax adj       n
    ##   <chr>      <chr>  <chr> <int>
    ## 1 critical   pred   big      92
    ## 2 critical   pred   small    92
    ## 3 critical   subj   big      92
    ## 4 critical   subj   small    92
    ## 5 filler     pred   big      92
    ## 6 filler     pred   small    92
    ## 7 filler     subj   big      92
    ## 8 filler     subj   small    92

``` r
d_main_cat <- d_main %>%
  rowwise() %>%
  mutate(response_cat = case_when(grepl(target, response) ~ "subordinate",
                                  (target == "doberman" & response == "dobermen") ~ "subordinate",
                                  (target == "Great Dane" & response == "Great") ~ "subordinate",
                                  TRUE ~ "basic"
                                  ),
         response_num = ifelse(response_cat == "basic", 1, 0))
```

``` r
d_main_cat_summary <- d_main_cat %>% group_by(syntax, trial_type) %>%
  tidyboot_mean(column = response_num)
```

    ## Warning: `as_data_frame()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

``` r
d_main_cat_summary %>%
  ggplot(., aes(x = syntax, y = mean, fill = syntax, ymin = ci_lower, ymax = ci_upper,)) +
  geom_col(alpha = 0.7, color = "black") +
  geom_linerange() +
  facet_wrap(~trial_type) +
  ylab("Proportion of basic-level responses") +
  ggtitle("Proportion of basic-level responses by-syntax.\n Error bars indicate 95% bootstrapped CIs")
```

![](direct-modification-FC-woTarget-pilot1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
d_main_cat_size_summary <- d_main_cat %>% group_by(syntax, adj, trial_type) %>%
  tidyboot_mean(column = response_num)
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

``` r
d_main_cat_size_summary %>%
  ggplot(., aes(x = syntax, y = mean, fill = syntax, ymin = ci_lower, ymax = ci_upper,)) +
  geom_col(alpha = 0.7, color = "black") +
  geom_linerange() +
  ylab("Proportion of basic-level responses") +
  facet_wrap(trial_type~adj) +
  ggtitle("Proportion of basic-level responses by-syntax.\n Error bars indicate 95% bootstrapped CIs")
```

![](direct-modification-FC-woTarget-pilot1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
d_main_cat_item_summary <- d_main_cat %>% group_by(syntax, item_noun, trial_type) %>%
  tidyboot_mean(column = response_num)
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

``` r
d_main_cat_item_summary %>%
  ggplot(., aes(x = syntax, y = mean, fill = syntax, ymin = ci_lower, ymax = ci_upper,)) +
  geom_col(alpha = 0.7, color = "black") +
  geom_linerange() +
  ylab("Proportion of basic-level responses") +
  facet_wrap(item_noun~trial_type, ncol = 2) +
  ggtitle("Proportion of basic-level responses by-syntax.\n Error bars indicate 95% bootstrapped CIs")
```

![](direct-modification-FC-woTarget-pilot1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
d_main_cat <- d_main_cat %>% mutate(
  unique_target = ifelse(trial_type == "critical", paste(target, ref_np, sep = "_"), target),
  syntax = factor(syntax, levels = c("subj", "pred")),
  trial_type = factor(trial_type)
)
# critical 1, filler -1
contrasts(d_main_cat$trial_type) <- contr.sum(2)
# subj 1, -1 pred
contrasts(d_main_cat$syntax) <- contr.sum(2)
model <- brm(response_num ~ syntax*trial_type + (1 + syntax*trial_type || submission_id) + 
               (1 + syntax*trial_type || unique_target ),
             data = d_main_cat,
             family = "bernoulli",
             iter = 3000,
             cores = 4)
```

    ## Compiling the C++ model

    ## Trying to compile a simple C file

    ## Start sampling

``` r
summary(model)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: response_num ~ syntax * trial_type + (1 + syntax * trial_type || submission_id) + (1 + syntax * trial_type || unique_target) 
    ##    Data: d_main_cat (Number of observations: 736) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 46) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               1.26      0.20     0.92     1.67 1.00     2439
    ## sd(syntax1)                 0.25      0.15     0.01     0.58 1.00     1766
    ## sd(trial_type1)             0.74      0.15     0.46     1.07 1.00     2775
    ## sd(syntax1:trial_type1)     0.18      0.12     0.01     0.46 1.00     2534
    ##                         Tail_ESS
    ## sd(Intercept)               3361
    ## sd(syntax1)                 2245
    ## sd(trial_type1)             4222
    ## sd(syntax1:trial_type1)     3155
    ## 
    ## ~unique_target (Number of levels: 34) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               0.26      0.17     0.01     0.61 1.00     1676
    ## sd(syntax1)                 0.17      0.13     0.01     0.47 1.00     2605
    ## sd(trial_type1)             0.26      0.17     0.01     0.61 1.00     1638
    ## sd(syntax1:trial_type1)     0.17      0.13     0.01     0.47 1.00     2476
    ##                         Tail_ESS
    ## sd(Intercept)               3092
    ## sd(syntax1)                 3098
    ## sd(trial_type1)             2438
    ## sd(syntax1:trial_type1)     2775
    ## 
    ## Population-Level Effects: 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept               0.05      0.22    -0.38     0.47 1.00     2619     3656
    ## syntax1                 0.55      0.12     0.33     0.79 1.00     7394     4757
    ## trial_type1             0.13      0.17    -0.19     0.47 1.00     4706     4048
    ## syntax1:trial_type1    -0.17      0.11    -0.39     0.04 1.00     8493     4761
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Get critical effect of syntax in the critical condition:

``` r
model %>% tidybayes::spread_draws(b_Intercept, b_syntax1, b_trial_type1, `b_syntax1:trial_type1`) %>%
  mutate(critical_subj = b_Intercept + b_syntax1 + b_trial_type1,
         critical_pred = b_Intercept - b_syntax1 + b_trial_type1,
         syntax_critical = critical_subj - critical_pred # subject vs predicate 
         ) %>% 
  select(b_Intercept, b_syntax1, critical_subj, critical_pred, syntax_critical) %>%
  gather(key, val) %>%
  group_by(key) %>%
 filter(key == "syntax_critical") %>% summarize(prob = mean(val > 0))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 1 x 2
    ##   key              prob
    ##   <chr>           <dbl>
    ## 1 syntax_critical     1
