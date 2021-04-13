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
  trial_type = factor(trial_type),
  adj = factor(adj, levels = c("big", "small"))
)
# critical 1, filler -1
contrasts(d_main_cat$trial_type) <- contr.sum(2)
# subj 1, -1 pred
contrasts(d_main_cat$syntax) <- contr.sum(2)
model <- brm(response_num ~ syntax*trial_type + (1 + syntax*trial_type || submission_id) + 
               (1 + syntax*trial_type || item), # random effects by-item (flowers, dogs, buildings etc) 
             data = d_main_cat,
             family = "bernoulli",
             control = list(adapt_delta = 0.9),
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
    ## Formula: response_num ~ syntax * trial_type + (1 + syntax * trial_type || submission_id) + (1 + syntax * trial_type || item) 
    ##    Data: d_main_cat (Number of observations: 736) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 7) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               0.34      0.22     0.03     0.88 1.00     1812
    ## sd(syntax1)                 0.15      0.13     0.01     0.50 1.00     2946
    ## sd(trial_type1)             0.14      0.13     0.01     0.47 1.00     2993
    ## sd(syntax1:trial_type1)     0.17      0.15     0.01     0.53 1.00     2752
    ##                         Tail_ESS
    ## sd(Intercept)               1913
    ## sd(syntax1)                 3322
    ## sd(trial_type1)             3624
    ## sd(syntax1:trial_type1)     3555
    ## 
    ## ~submission_id (Number of levels: 46) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               1.22      0.19     0.90     1.64 1.00     2524
    ## sd(syntax1)                 0.26      0.16     0.01     0.58 1.00     1767
    ## sd(trial_type1)             0.74      0.16     0.45     1.06 1.00     2308
    ## sd(syntax1:trial_type1)     0.18      0.12     0.01     0.45 1.00     2675
    ##                         Tail_ESS
    ## sd(Intercept)               3998
    ## sd(syntax1)                 2356
    ## sd(trial_type1)             3087
    ## sd(syntax1:trial_type1)     2929
    ## 
    ## Population-Level Effects: 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept               0.04      0.25    -0.45     0.54 1.00     2923     4112
    ## syntax1                 0.54      0.13     0.29     0.80 1.00     6502     4221
    ## trial_type1             0.14      0.16    -0.18     0.45 1.00     4592     4547
    ## syntax1:trial_type1    -0.17      0.12    -0.43     0.07 1.00     6583     4027
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Get critical effect of syntax in the critical condition:

``` r
model %>% tidybayes::spread_draws(b_Intercept, b_syntax1, b_trial_type1, `b_syntax1:trial_type1`) %>%
  mutate(critical_subj = b_Intercept + b_syntax1 + b_trial_type1,
         critical_pred = b_Intercept - b_syntax1 + b_trial_type1,
         syntax_critical = critical_subj - critical_pred, # subject vs predicate 
         filler_subj = b_Intercept + b_syntax1 - b_trial_type1,
         filler_pred = b_Intercept - b_syntax1 - b_trial_type1,
         syntax_filler = filler_subj - filler_pred
         ) %>% 
  select(b_Intercept, b_syntax1, critical_subj, critical_pred, syntax_critical, syntax_filler) %>%
  gather(key, val) %>%
  group_by(key) %>%
 filter(key == "syntax_filler" | key == "syntax_critical") %>% summarize(prob = mean(val > 0))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   key              prob
    ##   <chr>           <dbl>
    ## 1 syntax_critical 0.999
    ## 2 syntax_filler   0.999

Exploratory model with main effect of size:

``` r
# big 1, small -1
contrasts(d_main_cat$adj) <- contr.sum(2)

model_size <- brm(response_num ~ syntax*trial_type*adj + (1 + syntax*trial_type*adj || submission_id) + 
               (1 + syntax*trial_type*adj || item), # random effects by-item (flowers, dogs, buildings etc) 
             data = d_main_cat,
             family = "bernoulli",
             control = list(adapt_delta = 0.96),
             iter = 3000,
             cores = 4)
```

    ## Compiling the C++ model

    ## Trying to compile a simple C file

    ## Start sampling

``` r
summary(model_size)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: response_num ~ syntax * trial_type * adj + (1 + syntax * trial_type * adj || submission_id) + (1 + syntax * trial_type * adj || item) 
    ##    Data: d_main_cat (Number of observations: 736) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~item (Number of levels: 7) 
    ##                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                    0.42      0.25     0.06     1.03 1.00     1501
    ## sd(syntax1)                      0.16      0.15     0.01     0.53 1.00     2794
    ## sd(trial_type1)                  0.15      0.14     0.01     0.50 1.00     2922
    ## sd(adj1)                         0.46      0.29     0.04     1.15 1.00     1090
    ## sd(syntax1:trial_type1)          0.20      0.17     0.01     0.62 1.00     2177
    ## sd(syntax1:adj1)                 0.18      0.16     0.01     0.56 1.00     2311
    ## sd(trial_type1:adj1)             0.15      0.13     0.01     0.49 1.00     2830
    ## sd(syntax1:trial_type1:adj1)     0.22      0.18     0.01     0.68 1.00     2152
    ##                              Tail_ESS
    ## sd(Intercept)                    1488
    ## sd(syntax1)                      3443
    ## sd(trial_type1)                  2580
    ## sd(adj1)                         1006
    ## sd(syntax1:trial_type1)          2141
    ## sd(syntax1:adj1)                 3058
    ## sd(trial_type1:adj1)             3337
    ## sd(syntax1:trial_type1:adj1)     2868
    ## 
    ## ~submission_id (Number of levels: 46) 
    ##                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                    1.42      0.23     1.02     1.93 1.00     1780
    ## sd(syntax1)                      0.29      0.17     0.02     0.65 1.00     1355
    ## sd(trial_type1)                  0.87      0.18     0.56     1.27 1.00     1930
    ## sd(adj1)                         0.34      0.18     0.02     0.70 1.00     1116
    ## sd(syntax1:trial_type1)          0.22      0.15     0.01     0.55 1.00     1715
    ## sd(syntax1:adj1)                 0.21      0.14     0.01     0.52 1.00     1754
    ## sd(trial_type1:adj1)             0.21      0.14     0.01     0.54 1.00     1836
    ## sd(syntax1:trial_type1:adj1)     0.42      0.18     0.06     0.78 1.00     1119
    ##                              Tail_ESS
    ## sd(Intercept)                    3144
    ## sd(syntax1)                      1942
    ## sd(trial_type1)                  3451
    ## sd(adj1)                         1916
    ## sd(syntax1:trial_type1)          2420
    ## sd(syntax1:adj1)                 2475
    ## sd(trial_type1:adj1)             2230
    ## sd(syntax1:trial_type1:adj1)     1223
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                    0.06      0.29    -0.53     0.63 1.00     1787
    ## syntax1                      0.62      0.14     0.34     0.90 1.00     4008
    ## trial_type1                  0.15      0.18    -0.22     0.52 1.00     2779
    ## adj1                        -0.02      0.23    -0.49     0.45 1.00     2946
    ## syntax1:trial_type1         -0.20      0.15    -0.49     0.09 1.00     4011
    ## syntax1:adj1                 0.09      0.14    -0.18     0.36 1.00     4842
    ## trial_type1:adj1            -0.13      0.13    -0.38     0.14 1.00     5155
    ## syntax1:trial_type1:adj1     0.21      0.17    -0.10     0.55 1.00     3524
    ##                          Tail_ESS
    ## Intercept                    2795
    ## syntax1                      3902
    ## trial_type1                  3612
    ## adj1                         2996
    ## syntax1:trial_type1          3648
    ## syntax1:adj1                 3595
    ## trial_type1:adj1             3408
    ## syntax1:trial_type1:adj1     3488
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Get effects of syntax and effects of size by trial-type:

``` r
model_size %>% tidybayes::spread_draws(b_Intercept, b_syntax1, b_trial_type1, b_adj1, `b_syntax1:adj1`, `b_trial_type1:adj1`,
                                       `b_syntax1:trial_type1`, `b_syntax1:trial_type1:adj1`) %>%
  mutate(critical_subj = b_Intercept + b_syntax1 + b_trial_type1,
         critical_pred = b_Intercept - b_syntax1 + b_trial_type1,
         syntax_critical = critical_subj - critical_pred, # subject vs predicate 
         filler_subj = b_Intercept + b_syntax1 - b_trial_type1,
         filler_pred = b_Intercept - b_syntax1 - b_trial_type1,
         syntax_filler = filler_subj - filler_pred,
         critical_big = b_Intercept + b_trial_type1 + b_adj1,
         critical_small = b_Intercept + b_trial_type1 - b_adj1,
         critical_size = critical_big - critical_small,
         filler_big = b_Intercept - b_trial_type1 + b_adj1,
         filler_small =  b_Intercept - b_trial_type1 - b_adj1,
         filler_size = filler_big - filler_small
         ) %>% 
  select(b_Intercept, b_syntax1, critical_subj, critical_pred, syntax_critical, syntax_filler, critical_size, filler_size) %>%
  gather(key, val) %>%
  group_by(key) %>%
 filter(key == "syntax_filler" | key == "syntax_critical" | key == "critical_size" | key == "critical_size") %>% 
  summarize(prob = mean(val > 0))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 3 x 2
    ##   key              prob
    ##   <chr>           <dbl>
    ## 1 critical_size   0.463
    ## 2 syntax_critical 1.00 
    ## 3 syntax_filler   1.00
