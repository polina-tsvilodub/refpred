Direct Modification Power Analysis
================
Polina Tsvilodub
7/13/2020

In this document a simulation-based power analysis for the direct
modification study series can be found.

This power analysis is based on pilot data from pilots 5 and 6 (see
`modificationXrefUt-pilot2.Rmd`). The maximal model including main
effects of syntax (subject vs predicate-N), trial type (critical
vs. filler), their interaction and maximal random effect structure
(by-subject and by-item random intercepts and random slope effects of
the main predictors and their interaction) is assumed.

The power analysis proceeds as follows: the desired model described
above is fit on (tidy & classified) pilot data (n = 47 subjects); then,
posterior predictive samples for a given number of participants are
generated from the fitted model based on the pilot data. The desired
model is re-computed on these posterior samples and the parameters of
interest (i.e. the syntax coefficient and the interaction coefficient)
are extracted. This process is repeated (1000 iterations), and the power
for the given number of participants is calculated as the proportion of
critical coefficients that were estimated in the predicted direction
(i.e. CrI for effect syntax \> 0, and CrI for the interaction estimate
including 0). The number of simulated participants is incremented,
starting at 47 (the number used in the pilots).

### Pilot Model

``` r
# read pilot data
pilot5 <- read_csv("../../data/direct-modification/results_35_modXrefUt_pilot1_nonMatchClassified_tidy.csv") %>%
  rename('workerid' = submission_id, 
         'response_num' = response_numMatch
          ) %>% select(-NP_match)
```

    ## Parsed with column specification:
    ## cols(
    ##   submission_id = col_double(),
    ##   trial_number = col_double(),
    ##   context_picture = col_character(),
    ##   response = col_character(),
    ##   target_size = col_character(),
    ##   adj = col_character(),
    ##   syntax = col_character(),
    ##   target = col_character(),
    ##   item = col_character(),
    ##   adj_cond = col_character(),
    ##   trial_type = col_character(),
    ##   NP_match = col_character(),
    ##   response_numMatch = col_double()
    ## )

``` r
pilot6 <- read_csv("../../data/direct-modification/results_35_double-modXrefUt-pilot2_tidy.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   workerid = col_character(),
    ##   trial_number = col_double(),
    ##   context_picture = col_character(),
    ##   response = col_character(),
    ##   target_size = col_character(),
    ##   adj = col_character(),
    ##   syntax = col_character(),
    ##   target = col_character(),
    ##   item = col_character(),
    ##   adj_cond = col_character(),
    ##   trial_type = col_character(),
    ##   response_num = col_double()
    ## )

``` r
pilot_data <- rbind(pilot5, pilot6)
```

First, the seed model is computed on pilot data. Main effects are
deviation-coded.

``` r
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
  iter = 2000, 
  cores = 4,
  control = list(adapt_delta = 0.95)
)
```

    ## Compiling the C++ model

    ## Trying to compile a simple C file

    ## Start sampling

``` r
summary(pilot_model)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: response_num ~ syntax_dev * trial_dev + (1 + syntax_dev * trial_dev | workerid) + (1 + syntax_dev * trial_dev | target) 
    ##    Data: pilot_data (Number of observations: 373) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~target (Number of levels: 10) 
    ##                                         Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                               0.28      0.23     0.01     0.87
    ## sd(syntax_dev1)                             0.41      0.33     0.02     1.25
    ## sd(trial_dev1)                              0.62      0.39     0.04     1.52
    ## sd(syntax_dev1:trial_dev1)                  0.66      0.42     0.05     1.71
    ## cor(Intercept,syntax_dev1)                  0.01      0.45    -0.82     0.83
    ## cor(Intercept,trial_dev1)                   0.05      0.44    -0.79     0.83
    ## cor(syntax_dev1,trial_dev1)                -0.07      0.45    -0.84     0.78
    ## cor(Intercept,syntax_dev1:trial_dev1)       0.06      0.45    -0.79     0.84
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)    -0.00      0.44    -0.81     0.79
    ## cor(trial_dev1,syntax_dev1:trial_dev1)      0.14      0.43    -0.71     0.85
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.00     2664     2583
    ## sd(syntax_dev1)                         1.00     1883     2641
    ## sd(trial_dev1)                          1.00     1472     1525
    ## sd(syntax_dev1:trial_dev1)              1.00     1651     2224
    ## cor(Intercept,syntax_dev1)              1.00     4212     2892
    ## cor(Intercept,trial_dev1)               1.00     3255     3215
    ## cor(syntax_dev1,trial_dev1)             1.00     3131     3280
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     2953     3314
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00     3003     3610
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     3116     3474
    ## 
    ## ~workerid (Number of levels: 47) 
    ##                                         Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                               4.19      0.90     2.77     6.25
    ## sd(syntax_dev1)                             0.70      0.44     0.04     1.63
    ## sd(trial_dev1)                              1.28      0.50     0.41     2.43
    ## sd(syntax_dev1:trial_dev1)                  0.53      0.42     0.02     1.58
    ## cor(Intercept,syntax_dev1)                  0.01      0.42    -0.76     0.77
    ## cor(Intercept,trial_dev1)                   0.49      0.30    -0.22     0.91
    ## cor(syntax_dev1,trial_dev1)                -0.01      0.40    -0.76     0.74
    ## cor(Intercept,syntax_dev1:trial_dev1)      -0.32      0.44    -0.93     0.65
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)     0.01      0.44    -0.78     0.81
    ## cor(trial_dev1,syntax_dev1:trial_dev1)     -0.07      0.41    -0.79     0.73
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.00     1108     2235
    ## sd(syntax_dev1)                         1.00     1055     1903
    ## sd(trial_dev1)                          1.00     1555     1789
    ## sd(syntax_dev1:trial_dev1)              1.00     1460     2013
    ## cor(Intercept,syntax_dev1)              1.00     5346     3357
    ## cor(Intercept,trial_dev1)               1.00     3237     3217
    ## cor(syntax_dev1,trial_dev1)             1.00     1310     2165
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     3489     2985
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00     3311     2997
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     3805     3226
    ## 
    ## Population-Level Effects: 
    ##                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                  2.28      0.74     0.97     3.84 1.00     1464
    ## syntax_dev1                0.96      0.37     0.28     1.80 1.00     2211
    ## trial_dev1                 0.31      0.44    -0.56     1.23 1.00     2470
    ## syntax_dev1:trial_dev1    -0.02      0.38    -0.83     0.71 1.00     2470
    ##                        Tail_ESS
    ## Intercept                  2242
    ## syntax_dev1                2222
    ## trial_dev1                 2398
    ## syntax_dev1:trial_dev1     2563
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

We are mainly interested in the effect of syntax in the critical
condition:

``` r
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
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
predicted_draws  
```

    ## # A tibble: 10 x 4
    ##    key                         mean  lower upper
    ##    <chr>                      <dbl>  <dbl> <dbl>
    ##  1 b_Intercept               2.28    0.974 3.84 
    ##  2 b_syntax_dev1             0.964   0.283 1.80 
    ##  3 b_syntax_dev1:trial_dev1 -0.0199 -0.829 0.707
    ##  4 b_trial_dev1              0.309  -0.559 1.23 
    ##  5 critical_pred             0.991  -0.369 2.50 
    ##  6 critical_subj             2.96    1.27  5.48 
    ##  7 filler_pred               1.65   -0.200 3.82 
    ##  8 filler_subj               3.54    1.55  6.19 
    ##  9 syntax_critical           1.97    0.188 4.38 
    ## 10 syntax_filler             1.89   -0.164 4.17

Here, the first batch of posterior samples is drawn from the fitted
model. A new model is fit on these samples.

``` r
# add draws from the posterior predictive distribution, getting one sample per fit (n = 1)
# predictions are based on pilot data, i.e. grouped by original pilot input rows  
# corresponds to brms::redict.brmsfit()

# the new draws are added in the column .prediction
# simulates 47 participants, as in the original dataset 
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
  iter = 3000, 
  cores = 4,
  control = list(adapt_delta = 0.94)
)
```

    ## Compiling the C++ model

    ## recompiling to avoid crashing R session

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -D_REENTRANT  -DBOOST_DISABLE_ASSERTS -DBOOST_PENDING_INTEGER_LOG2_HPP -include stan/math/prim/mat/fun/Eigen.hpp   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/3.6/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1

    ## Start sampling

``` r
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
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
summary(predicted_fit)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: .prediction ~ syntax_dev * trial_dev + (1 + syntax_dev * trial_dev | workerid) + (1 + syntax_dev * trial_dev | target) 
    ##    Data: predicted_data (Number of observations: 373) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~target (Number of levels: 10) 
    ##                                         Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                               0.74      0.63     0.02     2.33
    ## sd(syntax_dev1)                             0.71      0.61     0.03     2.23
    ## sd(trial_dev1)                              4.75      1.92     2.14     9.53
    ## sd(syntax_dev1:trial_dev1)                  1.27      0.92     0.08     3.46
    ## cor(Intercept,syntax_dev1)                  0.03      0.45    -0.79     0.83
    ## cor(Intercept,trial_dev1)                  -0.05      0.45    -0.83     0.79
    ## cor(syntax_dev1,trial_dev1)                 0.09      0.44    -0.76     0.83
    ## cor(Intercept,syntax_dev1:trial_dev1)       0.09      0.44    -0.76     0.84
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)     0.03      0.45    -0.80     0.82
    ## cor(trial_dev1,syntax_dev1:trial_dev1)      0.16      0.40    -0.68     0.83
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.00     2153     2934
    ## sd(syntax_dev1)                         1.00     2632     2886
    ## sd(trial_dev1)                          1.00     1145     1756
    ## sd(syntax_dev1:trial_dev1)              1.00     1877     2574
    ## cor(Intercept,syntax_dev1)              1.00     5637     3976
    ## cor(Intercept,trial_dev1)               1.00     1019     2328
    ## cor(syntax_dev1,trial_dev1)             1.00     1343     2517
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     3062     3485
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00     3139     4252
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     5211     4740
    ## 
    ## ~workerid (Number of levels: 47) 
    ##                                         Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                              14.05      5.24     6.83    27.67
    ## sd(syntax_dev1)                             1.34      1.03     0.07     3.88
    ## sd(trial_dev1)                              3.96      1.56     1.76     7.80
    ## sd(syntax_dev1:trial_dev1)                  3.03      1.46     0.59     6.56
    ## cor(Intercept,syntax_dev1)                 -0.18      0.44    -0.88     0.72
    ## cor(Intercept,trial_dev1)                   0.09      0.33    -0.55     0.67
    ## cor(syntax_dev1,trial_dev1)                -0.19      0.38    -0.83     0.62
    ## cor(Intercept,syntax_dev1:trial_dev1)       0.02      0.36    -0.66     0.70
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)    -0.14      0.40    -0.84     0.67
    ## cor(trial_dev1,syntax_dev1:trial_dev1)      0.09      0.29    -0.50     0.63
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.01      743     1630
    ## sd(syntax_dev1)                         1.00     1095     2428
    ## sd(trial_dev1)                          1.00     1059     2049
    ## sd(syntax_dev1:trial_dev1)              1.00      777      851
    ## cor(Intercept,syntax_dev1)              1.00     5081     4300
    ## cor(Intercept,trial_dev1)               1.00     3751     4322
    ## cor(syntax_dev1,trial_dev1)             1.01      701     1298
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     3264     3374
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00      836     1714
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     3314     4074
    ## 
    ## Population-Level Effects: 
    ##                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                  5.79      2.78     1.40    12.52 1.00      942
    ## syntax_dev1                2.84      1.23     1.00     5.79 1.00     1243
    ## trial_dev1                 0.15      2.01    -3.83     4.06 1.00     2099
    ## syntax_dev1:trial_dev1    -1.32      1.13    -3.83     0.68 1.00     2051
    ##                        Tail_ESS
    ## Intercept                  1503
    ## syntax_dev1                1692
    ## trial_dev1                 2152
    ## syntax_dev1:trial_dev1     1938
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

### Simulate data & fit the model

``` r
# helper function to get posterior predictive draws for a subset of participants of size N, N <= 47 
# d is the a dataframe with posterior predictive draws for 47 participants based on the pilot data set 
get_new_data <- function(d, N) {
    if(N == 0) {
      data <- d
    } else {
      data <- add_predicted_draws(model=pilot_model, 
                                     newdata = pilot_data %>% 
                                       filter(workerid %in% sample(unique(pilot_data$workerid), N, replace = F)),
                                     n = 1) %>% 
                 mutate(workerid = paste(workerid, letters[1], sep = "_")) %>%
                   rbind(., d)
    }
    
  }
```

Here, new data are simulated over `seed` number of iterations; for each
new data set, the model fit on the first posterior samples batch
(`predicted_fit`) is updated. The summary statistics of estimated
coefficients are saved as a nested data frame.

``` r
# simulate data and updat
# n is number of participants to be added to the pilot baseline of 47, cannot exceed 47
sim_data_fit <- function(seed, N) {
  set.seed(seed)
  
# add draws from the posterior predictive distribution, getting one sample per fit (n = 1)
# predictions are based on pilot data, i.e. grouped by original pilot input rows  
# corresponds to brms::redict.brmsfit()

# the new draws are added in the column .prediction
# simulates 47 participants, as in the original dataset
  d <- add_predicted_draws(model=pilot_model, newdata = pilot_data, n = 1)
  
  # possibly add more participants
  data <- get_new_data(d, N)
  
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
```

### Power analysis

Small simulation over 100 iterations for 70 participants:

``` r
sim1 <-
  tibble(seed = 1:100) %>% 
  mutate(tidy = map(seed, sim_data_fit, 23)) %>% 
  unnest(tidy)
```

    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000217 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.17 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.7631 seconds (Warm-up)
    ## Chain 1:                11.5784 seconds (Sampling)
    ## Chain 1:                23.3416 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000153 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.7959 seconds (Warm-up)
    ## Chain 2:                13.4677 seconds (Sampling)
    ## Chain 2:                24.2636 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00015 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.4821 seconds (Warm-up)
    ## Chain 3:                12.335 seconds (Sampling)
    ## Chain 3:                23.817 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000162 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.8804 seconds (Warm-up)
    ## Chain 4:                8.87976 seconds (Sampling)
    ## Chain 4:                20.7602 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000166 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.3832 seconds (Warm-up)
    ## Chain 1:                13.9813 seconds (Sampling)
    ## Chain 1:                27.3644 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000166 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.2082 seconds (Warm-up)
    ## Chain 2:                14.0456 seconds (Sampling)
    ## Chain 2:                28.2538 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000152 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.3169 seconds (Warm-up)
    ## Chain 3:                13.978 seconds (Sampling)
    ## Chain 3:                27.295 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000155 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.226 seconds (Warm-up)
    ## Chain 4:                11.7487 seconds (Sampling)
    ## Chain 4:                24.9747 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000164 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.1568 seconds (Warm-up)
    ## Chain 1:                10.2787 seconds (Sampling)
    ## Chain 1:                22.4355 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000152 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.94227 seconds (Warm-up)
    ## Chain 2:                14.2203 seconds (Sampling)
    ## Chain 2:                24.1626 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000201 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 2.01 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.6201 seconds (Warm-up)
    ## Chain 3:                7.51891 seconds (Sampling)
    ## Chain 3:                18.139 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000166 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.8142 seconds (Warm-up)
    ## Chain 4:                7.12723 seconds (Sampling)
    ## Chain 4:                17.9415 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000153 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.97477 seconds (Warm-up)
    ## Chain 1:                6.96456 seconds (Sampling)
    ## Chain 1:                16.9393 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000154 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.7334 seconds (Warm-up)
    ## Chain 2:                11.8068 seconds (Sampling)
    ## Chain 2:                22.5402 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00015 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.3496 seconds (Warm-up)
    ## Chain 3:                6.94574 seconds (Sampling)
    ## Chain 3:                17.2954 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000153 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.0722 seconds (Warm-up)
    ## Chain 4:                13.84 seconds (Sampling)
    ## Chain 4:                23.9122 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000168 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.68 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.7065 seconds (Warm-up)
    ## Chain 1:                14.0812 seconds (Sampling)
    ## Chain 1:                29.7876 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000137 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.75 seconds (Warm-up)
    ## Chain 2:                14.0225 seconds (Sampling)
    ## Chain 2:                28.7725 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000166 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.6138 seconds (Warm-up)
    ## Chain 3:                14.062 seconds (Sampling)
    ## Chain 3:                28.6758 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000171 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.71 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.2236 seconds (Warm-up)
    ## Chain 4:                13.6058 seconds (Sampling)
    ## Chain 4:                27.8294 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000151 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.1944 seconds (Warm-up)
    ## Chain 1:                13.4665 seconds (Sampling)
    ## Chain 1:                26.6609 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000157 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.2876 seconds (Warm-up)
    ## Chain 2:                13.4842 seconds (Sampling)
    ## Chain 2:                27.7718 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000152 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.6052 seconds (Warm-up)
    ## Chain 3:                12.793 seconds (Sampling)
    ## Chain 3:                26.3981 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000137 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.9564 seconds (Warm-up)
    ## Chain 4:                13.484 seconds (Sampling)
    ## Chain 4:                27.4404 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000166 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.2696 seconds (Warm-up)
    ## Chain 1:                13.5115 seconds (Sampling)
    ## Chain 1:                28.7811 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000141 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.41 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.8663 seconds (Warm-up)
    ## Chain 2:                13.5296 seconds (Sampling)
    ## Chain 2:                27.3959 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000149 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.49 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.6381 seconds (Warm-up)
    ## Chain 3:                13.473 seconds (Sampling)
    ## Chain 3:                28.1111 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000152 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 15.5252 seconds (Warm-up)
    ## Chain 4:                13.5158 seconds (Sampling)
    ## Chain 4:                29.041 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000167 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.5814 seconds (Warm-up)
    ## Chain 1:                13.4735 seconds (Sampling)
    ## Chain 1:                28.0549 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000141 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.41 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.8885 seconds (Warm-up)
    ## Chain 2:                13.4817 seconds (Sampling)
    ## Chain 2:                27.3702 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00015 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.8026 seconds (Warm-up)
    ## Chain 3:                13.4737 seconds (Sampling)
    ## Chain 3:                27.2763 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00014 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.1122 seconds (Warm-up)
    ## Chain 4:                13.2542 seconds (Sampling)
    ## Chain 4:                27.3664 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000167 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.2001 seconds (Warm-up)
    ## Chain 1:                12.9296 seconds (Sampling)
    ## Chain 1:                26.1297 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000145 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.45 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.675 seconds (Warm-up)
    ## Chain 2:                10.7984 seconds (Sampling)
    ## Chain 2:                23.4734 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000156 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.1638 seconds (Warm-up)
    ## Chain 3:                14.5306 seconds (Sampling)
    ## Chain 3:                27.6944 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000183 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.83 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.2934 seconds (Warm-up)
    ## Chain 4:                7.65638 seconds (Sampling)
    ## Chain 4:                20.9498 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000167 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.3858 seconds (Warm-up)
    ## Chain 1:                14.2263 seconds (Sampling)
    ## Chain 1:                27.6121 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000159 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.59 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.6314 seconds (Warm-up)
    ## Chain 2:                14.1814 seconds (Sampling)
    ## Chain 2:                26.8129 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000187 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.87 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.8146 seconds (Warm-up)
    ## Chain 3:                14.1222 seconds (Sampling)
    ## Chain 3:                27.9367 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000154 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.419 seconds (Warm-up)
    ## Chain 4:                14.2316 seconds (Sampling)
    ## Chain 4:                27.6506 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000163 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.9711 seconds (Warm-up)
    ## Chain 1:                13.7019 seconds (Sampling)
    ## Chain 1:                26.673 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000155 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.77176 seconds (Warm-up)
    ## Chain 2:                6.95658 seconds (Sampling)
    ## Chain 2:                16.7283 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000131 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.31 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.2815 seconds (Warm-up)
    ## Chain 3:                7.37208 seconds (Sampling)
    ## Chain 3:                17.6535 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000154 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.28021 seconds (Warm-up)
    ## Chain 4:                6.94727 seconds (Sampling)
    ## Chain 4:                16.2275 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000153 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.1975 seconds (Warm-up)
    ## Chain 1:                14.0812 seconds (Sampling)
    ## Chain 1:                26.2787 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00016 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.228 seconds (Warm-up)
    ## Chain 2:                9.45721 seconds (Sampling)
    ## Chain 2:                20.6852 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000149 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.49 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.7078 seconds (Warm-up)
    ## Chain 3:                14.1929 seconds (Sampling)
    ## Chain 3:                26.9007 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00022 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.2 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.7202 seconds (Warm-up)
    ## Chain 4:                14.1718 seconds (Sampling)
    ## Chain 4:                25.892 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000162 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.0888 seconds (Warm-up)
    ## Chain 1:                8.01398 seconds (Sampling)
    ## Chain 1:                20.1028 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000149 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.49 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.8445 seconds (Warm-up)
    ## Chain 2:                13.8851 seconds (Sampling)
    ## Chain 2:                25.7296 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000151 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.2275 seconds (Warm-up)
    ## Chain 3:                6.97743 seconds (Sampling)
    ## Chain 3:                18.2049 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000234 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.34 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.955 seconds (Warm-up)
    ## Chain 4:                13.5766 seconds (Sampling)
    ## Chain 4:                25.5316 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000168 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.68 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.906 seconds (Warm-up)
    ## Chain 1:                14.1088 seconds (Sampling)
    ## Chain 1:                28.0148 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000157 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 15.1771 seconds (Warm-up)
    ## Chain 2:                14.1312 seconds (Sampling)
    ## Chain 2:                29.3084 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000156 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.8566 seconds (Warm-up)
    ## Chain 3:                14.1298 seconds (Sampling)
    ## Chain 3:                27.9865 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000155 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.6074 seconds (Warm-up)
    ## Chain 4:                13.8437 seconds (Sampling)
    ## Chain 4:                28.4511 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000167 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.95255 seconds (Warm-up)
    ## Chain 1:                7.05711 seconds (Sampling)
    ## Chain 1:                17.0097 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000141 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.41 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.31223 seconds (Warm-up)
    ## Chain 2:                6.99574 seconds (Sampling)
    ## Chain 2:                16.308 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000149 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.49 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.93383 seconds (Warm-up)
    ## Chain 3:                7.02162 seconds (Sampling)
    ## Chain 3:                16.9554 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00016 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.7518 seconds (Warm-up)
    ## Chain 4:                8.05447 seconds (Sampling)
    ## Chain 4:                18.8063 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000167 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.83418 seconds (Warm-up)
    ## Chain 1:                6.97774 seconds (Sampling)
    ## Chain 1:                16.8119 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000153 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.0675 seconds (Warm-up)
    ## Chain 2:                12.8034 seconds (Sampling)
    ## Chain 2:                22.8708 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000173 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.73 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.9797 seconds (Warm-up)
    ## Chain 3:                8.28755 seconds (Sampling)
    ## Chain 3:                19.2672 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000153 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.0952 seconds (Warm-up)
    ## Chain 4:                13.0076 seconds (Sampling)
    ## Chain 4:                24.1028 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000163 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.1279 seconds (Warm-up)
    ## Chain 1:                13.1575 seconds (Sampling)
    ## Chain 1:                23.2854 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00015 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.0266 seconds (Warm-up)
    ## Chain 2:                11.0152 seconds (Sampling)
    ## Chain 2:                22.0418 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000152 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.9226 seconds (Warm-up)
    ## Chain 3:                12.3734 seconds (Sampling)
    ## Chain 3:                24.2961 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000147 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.47 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.4717 seconds (Warm-up)
    ## Chain 4:                13.3986 seconds (Sampling)
    ## Chain 4:                24.8703 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000171 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.71 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.0122 seconds (Warm-up)
    ## Chain 1:                13.6186 seconds (Sampling)
    ## Chain 1:                27.6308 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000153 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 15.2489 seconds (Warm-up)
    ## Chain 2:                13.6593 seconds (Sampling)
    ## Chain 2:                28.9082 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000165 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.6359 seconds (Warm-up)
    ## Chain 3:                13.6634 seconds (Sampling)
    ## Chain 3:                28.2993 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000179 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.79 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.4108 seconds (Warm-up)
    ## Chain 4:                13.6481 seconds (Sampling)
    ## Chain 4:                28.0588 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000164 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.4652 seconds (Warm-up)
    ## Chain 1:                13.9334 seconds (Sampling)
    ## Chain 1:                26.3986 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000143 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.43 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.6098 seconds (Warm-up)
    ## Chain 2:                13.998 seconds (Sampling)
    ## Chain 2:                27.6078 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000141 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.41 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.2669 seconds (Warm-up)
    ## Chain 3:                13.9333 seconds (Sampling)
    ## Chain 3:                27.2002 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000149 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.49 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.4522 seconds (Warm-up)
    ## Chain 4:                13.2717 seconds (Sampling)
    ## Chain 4:                26.724 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000148 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.48 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.1931 seconds (Warm-up)
    ## Chain 1:                14.0872 seconds (Sampling)
    ## Chain 1:                28.2803 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000151 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.2972 seconds (Warm-up)
    ## Chain 2:                7.09679 seconds (Sampling)
    ## Chain 2:                18.394 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000152 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.9592 seconds (Warm-up)
    ## Chain 3:                7.06516 seconds (Sampling)
    ## Chain 3:                18.0244 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000135 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.4475 seconds (Warm-up)
    ## Chain 4:                13.1308 seconds (Sampling)
    ## Chain 4:                25.5783 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000166 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.6385 seconds (Warm-up)
    ## Chain 1:                13.8923 seconds (Sampling)
    ## Chain 1:                26.5309 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00015 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.4035 seconds (Warm-up)
    ## Chain 2:                13.8414 seconds (Sampling)
    ## Chain 2:                27.2449 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000161 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.8525 seconds (Warm-up)
    ## Chain 3:                13.8755 seconds (Sampling)
    ## Chain 3:                26.7279 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000249 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.49 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.3248 seconds (Warm-up)
    ## Chain 4:                13.4054 seconds (Sampling)
    ## Chain 4:                25.7302 seconds (Total)
    ## Chain 4:

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.94 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000152 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.6177 seconds (Warm-up)
    ## Chain 1:                8.20391 seconds (Sampling)
    ## Chain 1:                20.8216 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000167 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.6588 seconds (Warm-up)
    ## Chain 2:                8.47029 seconds (Sampling)
    ## Chain 2:                20.1291 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000204 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 2.04 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.5122 seconds (Warm-up)
    ## Chain 3:                12.6324 seconds (Sampling)
    ## Chain 3:                24.1446 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000157 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.4208 seconds (Warm-up)
    ## Chain 4:                13.887 seconds (Sampling)
    ## Chain 4:                25.3078 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000264 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.1727 seconds (Warm-up)
    ## Chain 1:                14.0475 seconds (Sampling)
    ## Chain 1:                27.2202 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000143 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.43 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.0921 seconds (Warm-up)
    ## Chain 2:                14.0891 seconds (Sampling)
    ## Chain 2:                27.1812 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00016 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.598 seconds (Warm-up)
    ## Chain 3:                14.0746 seconds (Sampling)
    ## Chain 3:                28.6725 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000151 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.5016 seconds (Warm-up)
    ## Chain 4:                14.0044 seconds (Sampling)
    ## Chain 4:                27.506 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000168 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.68 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.6086 seconds (Warm-up)
    ## Chain 1:                14.1405 seconds (Sampling)
    ## Chain 1:                28.7491 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000156 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.7453 seconds (Warm-up)
    ## Chain 2:                14.1558 seconds (Sampling)
    ## Chain 2:                28.9011 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000153 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.1039 seconds (Warm-up)
    ## Chain 3:                14.1597 seconds (Sampling)
    ## Chain 3:                28.2636 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000272 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.72 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.4756 seconds (Warm-up)
    ## Chain 4:                14.1674 seconds (Sampling)
    ## Chain 4:                27.6431 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000162 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.9512 seconds (Warm-up)
    ## Chain 1:                13.6758 seconds (Sampling)
    ## Chain 1:                26.627 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00015 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.3138 seconds (Warm-up)
    ## Chain 2:                13.0268 seconds (Sampling)
    ## Chain 2:                26.3406 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00014 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.6914 seconds (Warm-up)
    ## Chain 3:                12.7703 seconds (Sampling)
    ## Chain 3:                25.4617 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000144 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.44 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.6756 seconds (Warm-up)
    ## Chain 4:                12.4408 seconds (Sampling)
    ## Chain 4:                24.1164 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000154 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.22871 seconds (Warm-up)
    ## Chain 1:                12.835 seconds (Sampling)
    ## Chain 1:                22.0637 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000144 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.44 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 8.67678 seconds (Warm-up)
    ## Chain 2:                8.72781 seconds (Sampling)
    ## Chain 2:                17.4046 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000146 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.46 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.50561 seconds (Warm-up)
    ## Chain 3:                6.40078 seconds (Sampling)
    ## Chain 3:                15.9064 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000137 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.4513 seconds (Warm-up)
    ## Chain 4:                6.43426 seconds (Sampling)
    ## Chain 4:                15.8856 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000147 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.47 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.0824 seconds (Warm-up)
    ## Chain 1:                12.5968 seconds (Sampling)
    ## Chain 1:                26.6792 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000161 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.723 seconds (Warm-up)
    ## Chain 2:                12.7534 seconds (Sampling)
    ## Chain 2:                25.4764 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000135 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.1782 seconds (Warm-up)
    ## Chain 3:                12.7221 seconds (Sampling)
    ## Chain 3:                24.9002 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000164 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.1944 seconds (Warm-up)
    ## Chain 4:                12.7734 seconds (Sampling)
    ## Chain 4:                25.9678 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000151 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.996 seconds (Warm-up)
    ## Chain 1:                12.4907 seconds (Sampling)
    ## Chain 1:                24.4867 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000136 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.36 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.7006 seconds (Warm-up)
    ## Chain 2:                12.457 seconds (Sampling)
    ## Chain 2:                25.1576 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000135 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.3687 seconds (Warm-up)
    ## Chain 3:                12.4319 seconds (Sampling)
    ## Chain 3:                24.8007 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000215 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.15 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.6582 seconds (Warm-up)
    ## Chain 4:                12.4087 seconds (Sampling)
    ## Chain 4:                25.0669 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000147 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.47 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.6131 seconds (Warm-up)
    ## Chain 1:                11.8776 seconds (Sampling)
    ## Chain 1:                22.4908 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000161 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.8627 seconds (Warm-up)
    ## Chain 2:                12.4832 seconds (Sampling)
    ## Chain 2:                24.3459 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000162 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.3846 seconds (Warm-up)
    ## Chain 3:                12.4583 seconds (Sampling)
    ## Chain 3:                24.8429 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000133 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.33 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.5263 seconds (Warm-up)
    ## Chain 4:                12.4787 seconds (Sampling)
    ## Chain 4:                24.0051 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000152 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.1336 seconds (Warm-up)
    ## Chain 1:                11.5363 seconds (Sampling)
    ## Chain 1:                21.6699 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000144 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.44 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.2284 seconds (Warm-up)
    ## Chain 2:                12.7114 seconds (Sampling)
    ## Chain 2:                23.9398 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00014 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.795 seconds (Warm-up)
    ## Chain 3:                12.6798 seconds (Sampling)
    ## Chain 3:                23.4748 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000161 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.3212 seconds (Warm-up)
    ## Chain 4:                8.45603 seconds (Sampling)
    ## Chain 4:                19.7772 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000155 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.94485 seconds (Warm-up)
    ## Chain 1:                12.4645 seconds (Sampling)
    ## Chain 1:                22.4093 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000136 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.36 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.6008 seconds (Warm-up)
    ## Chain 2:                12.4312 seconds (Sampling)
    ## Chain 2:                24.032 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000143 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.43 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.98055 seconds (Warm-up)
    ## Chain 3:                12.512 seconds (Sampling)
    ## Chain 3:                22.4926 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00014 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.6345 seconds (Warm-up)
    ## Chain 4:                6.71495 seconds (Sampling)
    ## Chain 4:                18.3495 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000156 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.99654 seconds (Warm-up)
    ## Chain 1:                6.3556 seconds (Sampling)
    ## Chain 1:                16.3521 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000162 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.02957 seconds (Warm-up)
    ## Chain 2:                12.3612 seconds (Sampling)
    ## Chain 2:                21.3908 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000145 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.45 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.2863 seconds (Warm-up)
    ## Chain 3:                12.5844 seconds (Sampling)
    ## Chain 3:                22.8707 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000144 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.44 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.5231 seconds (Warm-up)
    ## Chain 4:                6.32327 seconds (Sampling)
    ## Chain 4:                15.8464 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000149 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.49 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.4151 seconds (Warm-up)
    ## Chain 1:                10.45 seconds (Sampling)
    ## Chain 1:                21.8651 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000138 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.4144 seconds (Warm-up)
    ## Chain 2:                12.623 seconds (Sampling)
    ## Chain 2:                23.0374 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000137 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.6934 seconds (Warm-up)
    ## Chain 3:                11.6834 seconds (Sampling)
    ## Chain 3:                24.3769 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000138 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.5047 seconds (Warm-up)
    ## Chain 4:                12.4012 seconds (Sampling)
    ## Chain 4:                23.9059 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000145 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.45 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.0885 seconds (Warm-up)
    ## Chain 1:                12.4861 seconds (Sampling)
    ## Chain 1:                22.5746 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000137 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.9419 seconds (Warm-up)
    ## Chain 2:                12.6074 seconds (Sampling)
    ## Chain 2:                22.5493 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000136 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.36 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.2728 seconds (Warm-up)
    ## Chain 3:                12.3386 seconds (Sampling)
    ## Chain 3:                22.6115 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00014 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.4634 seconds (Warm-up)
    ## Chain 4:                12.3721 seconds (Sampling)
    ## Chain 4:                22.8355 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000146 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.46 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.294 seconds (Warm-up)
    ## Chain 1:                12.3459 seconds (Sampling)
    ## Chain 1:                22.6399 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000137 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.9767 seconds (Warm-up)
    ## Chain 2:                12.423 seconds (Sampling)
    ## Chain 2:                23.3996 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000131 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.31 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.3005 seconds (Warm-up)
    ## Chain 3:                12.254 seconds (Sampling)
    ## Chain 3:                24.5545 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000136 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.36 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.1371 seconds (Warm-up)
    ## Chain 4:                11.7308 seconds (Sampling)
    ## Chain 4:                21.8679 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000145 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.45 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.472 seconds (Warm-up)
    ## Chain 1:                12.6109 seconds (Sampling)
    ## Chain 1:                26.0829 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000487 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 4.87 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.5791 seconds (Warm-up)
    ## Chain 2:                12.6172 seconds (Sampling)
    ## Chain 2:                26.1963 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000142 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.42 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.1529 seconds (Warm-up)
    ## Chain 3:                12.54 seconds (Sampling)
    ## Chain 3:                26.693 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000135 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.0935 seconds (Warm-up)
    ## Chain 4:                12.5892 seconds (Sampling)
    ## Chain 4:                25.6826 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000147 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.47 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.5303 seconds (Warm-up)
    ## Chain 1:                12.3617 seconds (Sampling)
    ## Chain 1:                23.892 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000146 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.46 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.0615 seconds (Warm-up)
    ## Chain 2:                12.4136 seconds (Sampling)
    ## Chain 2:                24.4751 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000144 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.44 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.0448 seconds (Warm-up)
    ## Chain 3:                12.4434 seconds (Sampling)
    ## Chain 3:                24.4882 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00014 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.4134 seconds (Warm-up)
    ## Chain 4:                12.4547 seconds (Sampling)
    ## Chain 4:                23.8681 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000155 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.3311 seconds (Warm-up)
    ## Chain 1:                12.5633 seconds (Sampling)
    ## Chain 1:                25.8943 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000137 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.6234 seconds (Warm-up)
    ## Chain 2:                12.5524 seconds (Sampling)
    ## Chain 2:                24.1759 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000143 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.43 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.1727 seconds (Warm-up)
    ## Chain 3:                12.5718 seconds (Sampling)
    ## Chain 3:                26.7445 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000141 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.41 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.547 seconds (Warm-up)
    ## Chain 4:                12.5549 seconds (Sampling)
    ## Chain 4:                25.1019 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000169 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.69 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.7872 seconds (Warm-up)
    ## Chain 1:                6.78778 seconds (Sampling)
    ## Chain 1:                18.575 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000134 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.34 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.91 seconds (Warm-up)
    ## Chain 2:                6.27756 seconds (Sampling)
    ## Chain 2:                18.1875 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000469 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 4.69 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.5121 seconds (Warm-up)
    ## Chain 3:                12.3851 seconds (Sampling)
    ## Chain 3:                23.8972 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000133 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.33 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.5793 seconds (Warm-up)
    ## Chain 4:                12.3291 seconds (Sampling)
    ## Chain 4:                23.9084 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000162 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 8.34954 seconds (Warm-up)
    ## Chain 1:                6.22166 seconds (Sampling)
    ## Chain 1:                14.5712 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000136 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.36 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.5751 seconds (Warm-up)
    ## Chain 2:                12.3796 seconds (Sampling)
    ## Chain 2:                21.9547 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000135 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 8.61077 seconds (Warm-up)
    ## Chain 3:                6.40297 seconds (Sampling)
    ## Chain 3:                15.0137 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000137 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.25632 seconds (Warm-up)
    ## Chain 4:                6.30221 seconds (Sampling)
    ## Chain 4:                15.5585 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000146 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.46 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.4296 seconds (Warm-up)
    ## Chain 1:                12.5115 seconds (Sampling)
    ## Chain 1:                26.9411 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000142 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.42 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.9645 seconds (Warm-up)
    ## Chain 2:                12.4629 seconds (Sampling)
    ## Chain 2:                26.4274 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000162 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.7044 seconds (Warm-up)
    ## Chain 3:                12.4785 seconds (Sampling)
    ## Chain 3:                26.1829 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000137 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.5468 seconds (Warm-up)
    ## Chain 4:                12.4923 seconds (Sampling)
    ## Chain 4:                27.0392 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000183 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.83 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.2417 seconds (Warm-up)
    ## Chain 1:                12.5147 seconds (Sampling)
    ## Chain 1:                25.7563 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000135 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.2588 seconds (Warm-up)
    ## Chain 2:                12.4315 seconds (Sampling)
    ## Chain 2:                26.6903 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000139 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.0368 seconds (Warm-up)
    ## Chain 3:                12.4055 seconds (Sampling)
    ## Chain 3:                26.4423 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000138 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.2636 seconds (Warm-up)
    ## Chain 4:                12.4305 seconds (Sampling)
    ## Chain 4:                25.6941 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000157 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.73284 seconds (Warm-up)
    ## Chain 1:                12.3343 seconds (Sampling)
    ## Chain 1:                22.0671 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000142 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.42 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.849 seconds (Warm-up)
    ## Chain 2:                12.4567 seconds (Sampling)
    ## Chain 2:                23.3057 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000135 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.5341 seconds (Warm-up)
    ## Chain 3:                6.22043 seconds (Sampling)
    ## Chain 3:                16.7545 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000135 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.46389 seconds (Warm-up)
    ## Chain 4:                6.48762 seconds (Sampling)
    ## Chain 4:                15.9515 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000151 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.1674 seconds (Warm-up)
    ## Chain 1:                11.113 seconds (Sampling)
    ## Chain 1:                21.2804 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000137 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.3216 seconds (Warm-up)
    ## Chain 2:                6.25176 seconds (Sampling)
    ## Chain 2:                16.5734 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000135 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.99795 seconds (Warm-up)
    ## Chain 3:                12.3389 seconds (Sampling)
    ## Chain 3:                22.3369 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000136 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.36 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.9886 seconds (Warm-up)
    ## Chain 4:                12.4086 seconds (Sampling)
    ## Chain 4:                23.3972 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.00015 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.17169 seconds (Warm-up)
    ## Chain 1:                6.36142 seconds (Sampling)
    ## Chain 1:                15.5331 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00014 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.4106 seconds (Warm-up)
    ## Chain 2:                6.54376 seconds (Sampling)
    ## Chain 2:                16.9543 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000189 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.89 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 8.17623 seconds (Warm-up)
    ## Chain 3:                6.3381 seconds (Sampling)
    ## Chain 3:                14.5143 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000141 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.41 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 8.36594 seconds (Warm-up)
    ## Chain 4:                6.39051 seconds (Sampling)
    ## Chain 4:                14.7564 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000145 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.45 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.7362 seconds (Warm-up)
    ## Chain 1:                9.9464 seconds (Sampling)
    ## Chain 1:                21.6826 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000152 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.2833 seconds (Warm-up)
    ## Chain 2:                12.416 seconds (Sampling)
    ## Chain 2:                22.6993 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000141 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.41 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.5107 seconds (Warm-up)
    ## Chain 3:                12.2464 seconds (Sampling)
    ## Chain 3:                22.7571 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000151 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.9681 seconds (Warm-up)
    ## Chain 4:                6.22387 seconds (Sampling)
    ## Chain 4:                17.192 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000148 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.48 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.7063 seconds (Warm-up)
    ## Chain 1:                6.75734 seconds (Sampling)
    ## Chain 1:                19.4636 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000138 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.7644 seconds (Warm-up)
    ## Chain 2:                11.7314 seconds (Sampling)
    ## Chain 2:                23.4958 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00014 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.4192 seconds (Warm-up)
    ## Chain 3:                12.6697 seconds (Sampling)
    ## Chain 3:                24.0889 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000139 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.4463 seconds (Warm-up)
    ## Chain 4:                12.5454 seconds (Sampling)
    ## Chain 4:                24.9917 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000154 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.5532 seconds (Warm-up)
    ## Chain 1:                12.5243 seconds (Sampling)
    ## Chain 1:                23.0776 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000139 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.9006 seconds (Warm-up)
    ## Chain 2:                12.396 seconds (Sampling)
    ## Chain 2:                23.2966 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000138 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.1124 seconds (Warm-up)
    ## Chain 3:                6.52562 seconds (Sampling)
    ## Chain 3:                17.6381 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000163 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.6215 seconds (Warm-up)
    ## Chain 4:                12.5542 seconds (Sampling)
    ## Chain 4:                24.1757 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000146 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.46 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.6423 seconds (Warm-up)
    ## Chain 1:                12.6238 seconds (Sampling)
    ## Chain 1:                26.2662 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000137 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.0067 seconds (Warm-up)
    ## Chain 2:                12.6779 seconds (Sampling)
    ## Chain 2:                25.6846 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000134 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.34 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.8568 seconds (Warm-up)
    ## Chain 3:                12.6898 seconds (Sampling)
    ## Chain 3:                26.5466 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00014 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.65 seconds (Warm-up)
    ## Chain 4:                12.644 seconds (Sampling)
    ## Chain 4:                25.294 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000151 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.731 seconds (Warm-up)
    ## Chain 1:                12.5232 seconds (Sampling)
    ## Chain 1:                24.2542 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000139 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.715 seconds (Warm-up)
    ## Chain 2:                12.551 seconds (Sampling)
    ## Chain 2:                24.266 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000138 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.1539 seconds (Warm-up)
    ## Chain 3:                6.32913 seconds (Sampling)
    ## Chain 3:                19.483 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000147 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.47 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.9535 seconds (Warm-up)
    ## Chain 4:                12.585 seconds (Sampling)
    ## Chain 4:                25.5385 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.00017 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.7 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 8.64035 seconds (Warm-up)
    ## Chain 1:                6.67212 seconds (Sampling)
    ## Chain 1:                15.3125 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000138 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 8.68285 seconds (Warm-up)
    ## Chain 2:                6.31849 seconds (Sampling)
    ## Chain 2:                15.0013 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000137 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 8.21384 seconds (Warm-up)
    ## Chain 3:                6.33549 seconds (Sampling)
    ## Chain 3:                14.5493 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000145 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.45 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 8.83099 seconds (Warm-up)
    ## Chain 4:                12.3599 seconds (Sampling)
    ## Chain 4:                21.1909 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000152 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.16 seconds (Warm-up)
    ## Chain 1:                12.2379 seconds (Sampling)
    ## Chain 1:                24.3978 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000138 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.1279 seconds (Warm-up)
    ## Chain 2:                8.411 seconds (Sampling)
    ## Chain 2:                18.5389 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000154 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.1099 seconds (Warm-up)
    ## Chain 3:                12.3473 seconds (Sampling)
    ## Chain 3:                22.4572 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000137 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.6431 seconds (Warm-up)
    ## Chain 4:                7.12409 seconds (Sampling)
    ## Chain 4:                17.7672 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000152 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.0032 seconds (Warm-up)
    ## Chain 1:                6.34825 seconds (Sampling)
    ## Chain 1:                16.3515 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000139 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.96233 seconds (Warm-up)
    ## Chain 2:                6.28872 seconds (Sampling)
    ## Chain 2:                16.251 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000138 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.5357 seconds (Warm-up)
    ## Chain 3:                12.8635 seconds (Sampling)
    ## Chain 3:                23.3992 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000157 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.6641 seconds (Warm-up)
    ## Chain 4:                10.6072 seconds (Sampling)
    ## Chain 4:                21.2713 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000147 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.47 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.6258 seconds (Warm-up)
    ## Chain 1:                12.3055 seconds (Sampling)
    ## Chain 1:                26.9313 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000144 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.44 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.0999 seconds (Warm-up)
    ## Chain 2:                24.3075 seconds (Sampling)
    ## Chain 2:                38.4074 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000139 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.9676 seconds (Warm-up)
    ## Chain 3:                12.3311 seconds (Sampling)
    ## Chain 3:                27.2987 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000143 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.43 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.2517 seconds (Warm-up)
    ## Chain 4:                12.2516 seconds (Sampling)
    ## Chain 4:                26.5033 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000153 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.6554 seconds (Warm-up)
    ## Chain 1:                12.3176 seconds (Sampling)
    ## Chain 1:                24.973 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000134 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.34 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.0817 seconds (Warm-up)
    ## Chain 2:                12.148 seconds (Sampling)
    ## Chain 2:                24.2297 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000137 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.2373 seconds (Warm-up)
    ## Chain 3:                12.3028 seconds (Sampling)
    ## Chain 3:                24.5401 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00014 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.4 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.2907 seconds (Warm-up)
    ## Chain 4:                12.2721 seconds (Sampling)
    ## Chain 4:                24.5629 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000152 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.5617 seconds (Warm-up)
    ## Chain 1:                12.5885 seconds (Sampling)
    ## Chain 1:                23.1502 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000138 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.7084 seconds (Warm-up)
    ## Chain 2:                7.99104 seconds (Sampling)
    ## Chain 2:                18.6995 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000143 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.43 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.8044 seconds (Warm-up)
    ## Chain 3:                12.2272 seconds (Sampling)
    ## Chain 3:                24.0316 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000134 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.34 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.0904 seconds (Warm-up)
    ## Chain 4:                12.5981 seconds (Sampling)
    ## Chain 4:                23.6885 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000149 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.49 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.5305 seconds (Warm-up)
    ## Chain 1:                8.94707 seconds (Sampling)
    ## Chain 1:                20.4776 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000142 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.42 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 8.73098 seconds (Warm-up)
    ## Chain 2:                6.52722 seconds (Sampling)
    ## Chain 2:                15.2582 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000139 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 8.62679 seconds (Warm-up)
    ## Chain 3:                6.73205 seconds (Sampling)
    ## Chain 3:                15.3588 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000142 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.42 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.72472 seconds (Warm-up)
    ## Chain 4:                11.0651 seconds (Sampling)
    ## Chain 4:                20.7898 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000188 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.88 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.847 seconds (Warm-up)
    ## Chain 1:                12.3599 seconds (Sampling)
    ## Chain 1:                23.2069 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000474 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 4.74 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.38836 seconds (Warm-up)
    ## Chain 2:                12.3351 seconds (Sampling)
    ## Chain 2:                21.7235 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000137 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.65491 seconds (Warm-up)
    ## Chain 3:                8.83442 seconds (Sampling)
    ## Chain 3:                18.4893 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000162 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.46786 seconds (Warm-up)
    ## Chain 4:                6.19235 seconds (Sampling)
    ## Chain 4:                15.6602 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000146 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.46 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.7419 seconds (Warm-up)
    ## Chain 1:                11.3659 seconds (Sampling)
    ## Chain 1:                23.1078 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000139 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.39 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.5173 seconds (Warm-up)
    ## Chain 2:                11.8977 seconds (Sampling)
    ## Chain 2:                23.415 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000135 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.35 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.2284 seconds (Warm-up)
    ## Chain 3:                12.3526 seconds (Sampling)
    ## Chain 3:                23.581 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000131 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.31 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.2409 seconds (Warm-up)
    ## Chain 4:                12.3519 seconds (Sampling)
    ## Chain 4:                23.5928 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000145 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.45 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.75331 seconds (Warm-up)
    ## Chain 1:                12.5654 seconds (Sampling)
    ## Chain 1:                22.3187 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000137 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.3837 seconds (Warm-up)
    ## Chain 2:                12.6587 seconds (Sampling)
    ## Chain 2:                23.0424 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000137 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.37 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.96994 seconds (Warm-up)
    ## Chain 3:                10.4396 seconds (Sampling)
    ## Chain 3:                20.4096 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000138 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.38 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.4965 seconds (Warm-up)
    ## Chain 4:                12.6883 seconds (Sampling)
    ## Chain 4:                24.1848 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000152 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.7736 seconds (Warm-up)
    ## Chain 1:                14.2794 seconds (Sampling)
    ## Chain 1:                27.053 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00052 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 5.2 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.4196 seconds (Warm-up)
    ## Chain 2:                14.1526 seconds (Sampling)
    ## Chain 2:                28.5721 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000152 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.6247 seconds (Warm-up)
    ## Chain 3:                14.7469 seconds (Sampling)
    ## Chain 3:                29.3716 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000255 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.55 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.1599 seconds (Warm-up)
    ## Chain 4:                14.4736 seconds (Sampling)
    ## Chain 4:                28.6335 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000162 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.2983 seconds (Warm-up)
    ## Chain 1:                14.808 seconds (Sampling)
    ## Chain 1:                27.1063 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000197 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.97 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.5069 seconds (Warm-up)
    ## Chain 2:                7.33018 seconds (Sampling)
    ## Chain 2:                19.8371 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000154 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.1278 seconds (Warm-up)
    ## Chain 3:                7.62341 seconds (Sampling)
    ## Chain 3:                19.7512 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000157 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.4773 seconds (Warm-up)
    ## Chain 4:                14.7419 seconds (Sampling)
    ## Chain 4:                26.2193 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000409 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 4.09 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.5767 seconds (Warm-up)
    ## Chain 1:                7.75145 seconds (Sampling)
    ## Chain 1:                19.3281 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000156 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.0945 seconds (Warm-up)
    ## Chain 2:                14.0179 seconds (Sampling)
    ## Chain 2:                26.1124 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000252 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 2.52 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.8917 seconds (Warm-up)
    ## Chain 3:                12.6073 seconds (Sampling)
    ## Chain 3:                25.499 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000156 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.1183 seconds (Warm-up)
    ## Chain 4:                7.73355 seconds (Sampling)
    ## Chain 4:                19.8519 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000163 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.7412 seconds (Warm-up)
    ## Chain 1:                12.9346 seconds (Sampling)
    ## Chain 1:                25.6758 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000156 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.9564 seconds (Warm-up)
    ## Chain 2:                14.856 seconds (Sampling)
    ## Chain 2:                27.8124 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00016 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.6657 seconds (Warm-up)
    ## Chain 3:                13.7131 seconds (Sampling)
    ## Chain 3:                27.3788 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00017 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.7 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.0607 seconds (Warm-up)
    ## Chain 4:                7.65113 seconds (Sampling)
    ## Chain 4:                21.7118 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000165 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.59 seconds (Warm-up)
    ## Chain 1:                15.039 seconds (Sampling)
    ## Chain 1:                27.629 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000167 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.0964 seconds (Warm-up)
    ## Chain 2:                7.58641 seconds (Sampling)
    ## Chain 2:                19.6828 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000161 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.7177 seconds (Warm-up)
    ## Chain 3:                15.0728 seconds (Sampling)
    ## Chain 3:                28.7905 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000156 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.6966 seconds (Warm-up)
    ## Chain 4:                14.9199 seconds (Sampling)
    ## Chain 4:                28.6166 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000173 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.73 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.7037 seconds (Warm-up)
    ## Chain 1:                8.59701 seconds (Sampling)
    ## Chain 1:                22.3007 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000214 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 2.14 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.463 seconds (Warm-up)
    ## Chain 2:                12.5744 seconds (Sampling)
    ## Chain 2:                26.0374 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000155 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.9174 seconds (Warm-up)
    ## Chain 3:                14.3922 seconds (Sampling)
    ## Chain 3:                28.3096 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000242 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.42 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.1922 seconds (Warm-up)
    ## Chain 4:                11.5083 seconds (Sampling)
    ## Chain 4:                24.7005 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000167 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.2304 seconds (Warm-up)
    ## Chain 1:                14.4009 seconds (Sampling)
    ## Chain 1:                29.6314 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000163 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 16.4078 seconds (Warm-up)
    ## Chain 2:                14.2863 seconds (Sampling)
    ## Chain 2:                30.6941 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000159 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.59 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 15.0257 seconds (Warm-up)
    ## Chain 3:                14.275 seconds (Sampling)
    ## Chain 3:                29.3007 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00015 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.5 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 15.7709 seconds (Warm-up)
    ## Chain 4:                15.0992 seconds (Sampling)
    ## Chain 4:                30.8701 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000371 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 3.71 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.6376 seconds (Warm-up)
    ## Chain 1:                14.651 seconds (Sampling)
    ## Chain 1:                30.2885 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000157 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 16.4409 seconds (Warm-up)
    ## Chain 2:                14.3747 seconds (Sampling)
    ## Chain 2:                30.8156 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000163 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 16.0627 seconds (Warm-up)
    ## Chain 3:                14.4047 seconds (Sampling)
    ## Chain 3:                30.4674 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000161 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 15.7944 seconds (Warm-up)
    ## Chain 4:                13.914 seconds (Sampling)
    ## Chain 4:                29.7083 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000192 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.92 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.4741 seconds (Warm-up)
    ## Chain 1:                13.6016 seconds (Sampling)
    ## Chain 1:                25.0757 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000153 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.9525 seconds (Warm-up)
    ## Chain 2:                7.19901 seconds (Sampling)
    ## Chain 2:                19.1515 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000156 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.1303 seconds (Warm-up)
    ## Chain 3:                13.8122 seconds (Sampling)
    ## Chain 3:                24.9425 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000155 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.9383 seconds (Warm-up)
    ## Chain 4:                13.2971 seconds (Sampling)
    ## Chain 4:                25.2354 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000163 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.4442 seconds (Warm-up)
    ## Chain 1:                15.2648 seconds (Sampling)
    ## Chain 1:                30.709 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000193 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.93 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 15.0874 seconds (Warm-up)
    ## Chain 2:                14.9896 seconds (Sampling)
    ## Chain 2:                30.077 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000155 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.9653 seconds (Warm-up)
    ## Chain 3:                8.39753 seconds (Sampling)
    ## Chain 3:                22.3629 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000158 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.4922 seconds (Warm-up)
    ## Chain 4:                14.3598 seconds (Sampling)
    ## Chain 4:                28.852 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000208 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.08 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.87883 seconds (Warm-up)
    ## Chain 1:                7.4374 seconds (Sampling)
    ## Chain 1:                17.3162 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000156 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.73232 seconds (Warm-up)
    ## Chain 2:                7.48021 seconds (Sampling)
    ## Chain 2:                17.2125 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000161 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.44045 seconds (Warm-up)
    ## Chain 3:                7.41247 seconds (Sampling)
    ## Chain 3:                16.8529 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000203 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 2.03 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.07293 seconds (Warm-up)
    ## Chain 4:                7.2484 seconds (Sampling)
    ## Chain 4:                16.3213 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000163 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.4072 seconds (Warm-up)
    ## Chain 1:                14.1858 seconds (Sampling)
    ## Chain 1:                27.593 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000163 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.4539 seconds (Warm-up)
    ## Chain 2:                14.3147 seconds (Sampling)
    ## Chain 2:                28.7686 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000154 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.54 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.6058 seconds (Warm-up)
    ## Chain 3:                8.46676 seconds (Sampling)
    ## Chain 3:                22.0726 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000162 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.9188 seconds (Warm-up)
    ## Chain 4:                14.351 seconds (Sampling)
    ## Chain 4:                28.2698 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000168 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.68 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 10.9098 seconds (Warm-up)
    ## Chain 1:                11.1546 seconds (Sampling)
    ## Chain 1:                22.0644 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000157 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.1787 seconds (Warm-up)
    ## Chain 2:                14.6264 seconds (Sampling)
    ## Chain 2:                25.8051 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000177 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.77 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.23 seconds (Warm-up)
    ## Chain 3:                7.50098 seconds (Sampling)
    ## Chain 3:                18.731 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000157 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.134 seconds (Warm-up)
    ## Chain 4:                14.8898 seconds (Sampling)
    ## Chain 4:                26.0239 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000169 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.69 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.1836 seconds (Warm-up)
    ## Chain 1:                14.0193 seconds (Sampling)
    ## Chain 1:                26.2029 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000164 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.6415 seconds (Warm-up)
    ## Chain 2:                14.1961 seconds (Sampling)
    ## Chain 2:                25.8376 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000158 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.5081 seconds (Warm-up)
    ## Chain 3:                11.0066 seconds (Sampling)
    ## Chain 3:                22.5147 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000157 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.3211 seconds (Warm-up)
    ## Chain 4:                9.32628 seconds (Sampling)
    ## Chain 4:                21.6474 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000165 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.7615 seconds (Warm-up)
    ## Chain 1:                11.5684 seconds (Sampling)
    ## Chain 1:                26.33 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000151 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.8374 seconds (Warm-up)
    ## Chain 2:                14.31 seconds (Sampling)
    ## Chain 2:                29.1474 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000185 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.85 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 15.1254 seconds (Warm-up)
    ## Chain 3:                14.3169 seconds (Sampling)
    ## Chain 3:                29.4422 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000163 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.775 seconds (Warm-up)
    ## Chain 4:                14.3314 seconds (Sampling)
    ## Chain 4:                29.1064 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000162 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.1997 seconds (Warm-up)
    ## Chain 1:                14.2511 seconds (Sampling)
    ## Chain 1:                27.4508 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000163 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.6012 seconds (Warm-up)
    ## Chain 2:                14.3593 seconds (Sampling)
    ## Chain 2:                26.9605 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000156 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.5577 seconds (Warm-up)
    ## Chain 3:                14.3994 seconds (Sampling)
    ## Chain 3:                28.9571 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000163 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.8365 seconds (Warm-up)
    ## Chain 4:                14.0634 seconds (Sampling)
    ## Chain 4:                27.8999 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000162 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.1446 seconds (Warm-up)
    ## Chain 1:                7.32598 seconds (Sampling)
    ## Chain 1:                18.4706 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000159 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.59 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.0616 seconds (Warm-up)
    ## Chain 2:                7.3207 seconds (Sampling)
    ## Chain 2:                17.3823 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000163 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.1399 seconds (Warm-up)
    ## Chain 3:                14.2353 seconds (Sampling)
    ## Chain 3:                24.3752 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000161 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.8322 seconds (Warm-up)
    ## Chain 4:                14.7647 seconds (Sampling)
    ## Chain 4:                26.5968 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.00017 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.7 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.8404 seconds (Warm-up)
    ## Chain 1:                7.27281 seconds (Sampling)
    ## Chain 1:                19.1133 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000158 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.9878 seconds (Warm-up)
    ## Chain 2:                14.2657 seconds (Sampling)
    ## Chain 2:                25.2534 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000167 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.1656 seconds (Warm-up)
    ## Chain 3:                9.3777 seconds (Sampling)
    ## Chain 3:                21.5433 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000161 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.9904 seconds (Warm-up)
    ## Chain 4:                7.28226 seconds (Sampling)
    ## Chain 4:                18.2726 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000169 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.69 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.9496 seconds (Warm-up)
    ## Chain 1:                13.2045 seconds (Sampling)
    ## Chain 1:                25.1541 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00016 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.5901 seconds (Warm-up)
    ## Chain 2:                14.4099 seconds (Sampling)
    ## Chain 2:                27 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000151 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.3811 seconds (Warm-up)
    ## Chain 3:                15.0696 seconds (Sampling)
    ## Chain 3:                28.4507 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000169 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.69 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.9652 seconds (Warm-up)
    ## Chain 4:                7.39381 seconds (Sampling)
    ## Chain 4:                19.359 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.00017 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.7 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.2219 seconds (Warm-up)
    ## Chain 1:                8.2769 seconds (Sampling)
    ## Chain 1:                23.4988 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000163 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.5913 seconds (Warm-up)
    ## Chain 2:                14.2403 seconds (Sampling)
    ## Chain 2:                27.8316 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000166 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.6609 seconds (Warm-up)
    ## Chain 3:                14.1122 seconds (Sampling)
    ## Chain 3:                26.7731 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000159 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.59 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.2966 seconds (Warm-up)
    ## Chain 4:                14.1397 seconds (Sampling)
    ## Chain 4:                27.4364 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000169 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.69 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.8329 seconds (Warm-up)
    ## Chain 1:                9.76844 seconds (Sampling)
    ## Chain 1:                21.6013 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000162 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.2415 seconds (Warm-up)
    ## Chain 2:                12.9714 seconds (Sampling)
    ## Chain 2:                25.213 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000156 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.9718 seconds (Warm-up)
    ## Chain 3:                14.4438 seconds (Sampling)
    ## Chain 3:                27.4157 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000165 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.0004 seconds (Warm-up)
    ## Chain 4:                8.24265 seconds (Sampling)
    ## Chain 4:                21.2431 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000164 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.2116 seconds (Warm-up)
    ## Chain 1:                14.3293 seconds (Sampling)
    ## Chain 1:                27.5409 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000156 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.4901 seconds (Warm-up)
    ## Chain 2:                7.19986 seconds (Sampling)
    ## Chain 2:                21.6899 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000156 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.4777 seconds (Warm-up)
    ## Chain 3:                14.3624 seconds (Sampling)
    ## Chain 3:                28.8401 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000152 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.4068 seconds (Warm-up)
    ## Chain 4:                14.3821 seconds (Sampling)
    ## Chain 4:                27.7889 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000165 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.7972 seconds (Warm-up)
    ## Chain 1:                14.0546 seconds (Sampling)
    ## Chain 1:                25.8518 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000158 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.13 seconds (Warm-up)
    ## Chain 2:                14.1849 seconds (Sampling)
    ## Chain 2:                27.3149 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000167 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.1165 seconds (Warm-up)
    ## Chain 3:                13.9656 seconds (Sampling)
    ## Chain 3:                28.0821 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000162 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.0695 seconds (Warm-up)
    ## Chain 4:                14.1828 seconds (Sampling)
    ## Chain 4:                27.2523 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000182 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.82 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.4638 seconds (Warm-up)
    ## Chain 1:                14.4281 seconds (Sampling)
    ## Chain 1:                27.8919 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000171 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.71 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.9882 seconds (Warm-up)
    ## Chain 2:                7.21301 seconds (Sampling)
    ## Chain 2:                20.2012 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000152 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.3812 seconds (Warm-up)
    ## Chain 3:                14.38 seconds (Sampling)
    ## Chain 3:                26.7612 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000155 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.1449 seconds (Warm-up)
    ## Chain 4:                11.1174 seconds (Sampling)
    ## Chain 4:                25.2623 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000163 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.9019 seconds (Warm-up)
    ## Chain 1:                8.86721 seconds (Sampling)
    ## Chain 1:                22.7691 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000155 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.4068 seconds (Warm-up)
    ## Chain 2:                14.2794 seconds (Sampling)
    ## Chain 2:                27.6862 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000158 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.708 seconds (Warm-up)
    ## Chain 3:                13.4141 seconds (Sampling)
    ## Chain 3:                27.1221 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000163 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.9446 seconds (Warm-up)
    ## Chain 4:                14.229 seconds (Sampling)
    ## Chain 4:                28.1736 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000173 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.73 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.0655 seconds (Warm-up)
    ## Chain 1:                14.3678 seconds (Sampling)
    ## Chain 1:                29.4333 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000193 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.93 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.9426 seconds (Warm-up)
    ## Chain 2:                14.3579 seconds (Sampling)
    ## Chain 2:                29.3005 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000157 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 15.1371 seconds (Warm-up)
    ## Chain 3:                14.3773 seconds (Sampling)
    ## Chain 3:                29.5144 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000164 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.6763 seconds (Warm-up)
    ## Chain 4:                13.8895 seconds (Sampling)
    ## Chain 4:                28.5658 seconds (Total)
    ## Chain 4:

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.94 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    
    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000166 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 12.1748 seconds (Warm-up)
    ## Chain 1:                14.37 seconds (Sampling)
    ## Chain 1:                26.5448 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000156 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.3295 seconds (Warm-up)
    ## Chain 2:                14.2974 seconds (Sampling)
    ## Chain 2:                26.6269 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000164 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.8448 seconds (Warm-up)
    ## Chain 3:                14.3078 seconds (Sampling)
    ## Chain 3:                27.1525 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000166 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.9725 seconds (Warm-up)
    ## Chain 4:                14.2953 seconds (Sampling)
    ## Chain 4:                27.2678 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000164 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.7845 seconds (Warm-up)
    ## Chain 1:                7.18602 seconds (Sampling)
    ## Chain 1:                18.9705 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000158 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.5418 seconds (Warm-up)
    ## Chain 2:                7.20713 seconds (Sampling)
    ## Chain 2:                19.7489 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000164 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 10.8004 seconds (Warm-up)
    ## Chain 3:                7.1949 seconds (Sampling)
    ## Chain 3:                17.9953 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000151 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.51 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.3689 seconds (Warm-up)
    ## Chain 4:                7.16251 seconds (Sampling)
    ## Chain 4:                18.5314 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000164 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 14.0083 seconds (Warm-up)
    ## Chain 1:                7.22276 seconds (Sampling)
    ## Chain 1:                21.2311 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000201 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 2.01 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.2182 seconds (Warm-up)
    ## Chain 2:                14.3371 seconds (Sampling)
    ## Chain 2:                28.5553 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000162 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.9354 seconds (Warm-up)
    ## Chain 3:                13.5716 seconds (Sampling)
    ## Chain 3:                28.507 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00016 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 15.2711 seconds (Warm-up)
    ## Chain 4:                14.5836 seconds (Sampling)
    ## Chain 4:                29.8548 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.00017 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.7 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.8689 seconds (Warm-up)
    ## Chain 1:                8.0069 seconds (Sampling)
    ## Chain 1:                19.8758 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000161 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 10.8429 seconds (Warm-up)
    ## Chain 2:                14.3528 seconds (Sampling)
    ## Chain 2:                25.1958 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000177 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.77 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.2169 seconds (Warm-up)
    ## Chain 3:                14.3756 seconds (Sampling)
    ## Chain 3:                25.5925 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000166 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.7573 seconds (Warm-up)
    ## Chain 4:                14.2122 seconds (Sampling)
    ## Chain 4:                24.9696 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000165 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 16.6179 seconds (Warm-up)
    ## Chain 1:                14.0588 seconds (Sampling)
    ## Chain 1:                30.6767 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000152 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 15.48 seconds (Warm-up)
    ## Chain 2:                14.0212 seconds (Sampling)
    ## Chain 2:                29.5012 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000157 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 16.1458 seconds (Warm-up)
    ## Chain 3:                15.0424 seconds (Sampling)
    ## Chain 3:                31.1882 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.00017 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.7 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 17.3562 seconds (Warm-up)
    ## Chain 4:                15.0357 seconds (Sampling)
    ## Chain 4:                32.3919 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000164 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 15.5242 seconds (Warm-up)
    ## Chain 1:                14.3008 seconds (Sampling)
    ## Chain 1:                29.825 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000172 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.72 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.333 seconds (Warm-up)
    ## Chain 2:                14.0266 seconds (Sampling)
    ## Chain 2:                28.3596 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00016 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 14.4506 seconds (Warm-up)
    ## Chain 3:                14.0959 seconds (Sampling)
    ## Chain 3:                28.5465 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000191 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.91 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 14.9299 seconds (Warm-up)
    ## Chain 4:                14.1375 seconds (Sampling)
    ## Chain 4:                29.0674 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000173 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.73 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.9087 seconds (Warm-up)
    ## Chain 1:                7.23846 seconds (Sampling)
    ## Chain 1:                19.1472 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000159 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.59 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.8771 seconds (Warm-up)
    ## Chain 2:                7.21273 seconds (Sampling)
    ## Chain 2:                19.0898 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000162 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.4481 seconds (Warm-up)
    ## Chain 3:                11.7161 seconds (Sampling)
    ## Chain 3:                23.1642 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000161 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.61 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.8856 seconds (Warm-up)
    ## Chain 4:                14.0789 seconds (Sampling)
    ## Chain 4:                25.9645 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000165 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.0699 seconds (Warm-up)
    ## Chain 1:                14.2336 seconds (Sampling)
    ## Chain 1:                27.3036 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000189 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.89 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.4932 seconds (Warm-up)
    ## Chain 2:                12.2093 seconds (Sampling)
    ## Chain 2:                23.7025 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.00016 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.076 seconds (Warm-up)
    ## Chain 3:                7.66887 seconds (Sampling)
    ## Chain 3:                19.7449 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000173 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.73 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.6273 seconds (Warm-up)
    ## Chain 4:                7.601 seconds (Sampling)
    ## Chain 4:                20.2283 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000165 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.9318 seconds (Warm-up)
    ## Chain 1:                14.2745 seconds (Sampling)
    ## Chain 1:                26.2063 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000165 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.4603 seconds (Warm-up)
    ## Chain 2:                14.1994 seconds (Sampling)
    ## Chain 2:                26.6597 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000158 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.8901 seconds (Warm-up)
    ## Chain 3:                13.872 seconds (Sampling)
    ## Chain 3:                27.7621 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000158 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.9789 seconds (Warm-up)
    ## Chain 4:                13.642 seconds (Sampling)
    ## Chain 4:                25.6209 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000167 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.0787 seconds (Warm-up)
    ## Chain 1:                7.17175 seconds (Sampling)
    ## Chain 1:                18.2504 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000158 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.0954 seconds (Warm-up)
    ## Chain 2:                11.5509 seconds (Sampling)
    ## Chain 2:                23.6463 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000158 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.5635 seconds (Warm-up)
    ## Chain 3:                14.3208 seconds (Sampling)
    ## Chain 3:                26.8843 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000163 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.63 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.4477 seconds (Warm-up)
    ## Chain 4:                14.1323 seconds (Sampling)
    ## Chain 4:                25.5799 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000162 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.2852 seconds (Warm-up)
    ## Chain 1:                14.218 seconds (Sampling)
    ## Chain 1:                25.5032 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000159 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.59 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.0903 seconds (Warm-up)
    ## Chain 2:                7.3541 seconds (Sampling)
    ## Chain 2:                18.4444 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000207 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 2.07 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.9101 seconds (Warm-up)
    ## Chain 3:                7.08306 seconds (Sampling)
    ## Chain 3:                18.9931 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000165 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.65 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 10.9503 seconds (Warm-up)
    ## Chain 4:                14.2688 seconds (Sampling)
    ## Chain 4:                25.2191 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000185 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.85 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.4493 seconds (Warm-up)
    ## Chain 1:                14.4753 seconds (Sampling)
    ## Chain 1:                27.9246 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000155 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.55 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 12.9716 seconds (Warm-up)
    ## Chain 2:                7.41024 seconds (Sampling)
    ## Chain 2:                20.3818 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000162 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 12.7277 seconds (Warm-up)
    ## Chain 3:                14.0563 seconds (Sampling)
    ## Chain 3:                26.7839 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000152 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.52 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 12.8321 seconds (Warm-up)
    ## Chain 4:                14.1511 seconds (Sampling)
    ## Chain 4:                26.9832 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000166 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.3807 seconds (Warm-up)
    ## Chain 1:                7.5098 seconds (Sampling)
    ## Chain 1:                20.8905 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000153 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.53 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.4596 seconds (Warm-up)
    ## Chain 2:                14.3163 seconds (Sampling)
    ## Chain 2:                27.7759 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000156 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.4868 seconds (Warm-up)
    ## Chain 3:                14.2211 seconds (Sampling)
    ## Chain 3:                27.7079 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000166 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.66 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.0785 seconds (Warm-up)
    ## Chain 4:                14.6234 seconds (Sampling)
    ## Chain 4:                27.7019 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000164 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.64 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 13.9195 seconds (Warm-up)
    ## Chain 1:                7.36139 seconds (Sampling)
    ## Chain 1:                21.2809 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000159 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.59 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 13.47 seconds (Warm-up)
    ## Chain 2:                13.5953 seconds (Sampling)
    ## Chain 2:                27.0654 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000197 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.97 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 13.0331 seconds (Warm-up)
    ## Chain 3:                7.26857 seconds (Sampling)
    ## Chain 3:                20.3017 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '3c916a50fcaae592b5576d11ab8bea20' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000157 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  300 / 3000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  600 / 3000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  900 / 3000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 1200 / 3000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 3000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 2100 / 3000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 2400 / 3000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 2700 / 3000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 13.5051 seconds (Warm-up)
    ## Chain 4:                14.4989 seconds (Sampling)
    ## Chain 4:                28.004 seconds (Total)
    ## Chain 4:

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
# power for effect of syntax
# power for the syntax effect for 47 participants is around 0.75
sim1 %>%
  filter(key == "syntax_critical") %>%
  mutate(check_syntax = ifelse(lower > 0, 1, 0)) %>%
  summarise(power_syntax = mean(check_syntax))
```

    ## # A tibble: 1 x 1
    ##   power_syntax
    ##          <dbl>
    ## 1         0.55

``` r
# power for interaction 
#sim1 %>%
#  filter(term == "b_syntax_dev1:trial_dev1") %>%
#  mutate(check_interaction = ifelse(((lower < 0) & (upper > 0)), 1, 0)) %>%
#  summarise(power_interaction = mean(check_interaction))
```

Simulation of 80 participants over 100 iterations:

``` r
# simulate data and analysis for 80 participants
sim2 <-
  tibble(seed = 1:100) %>% 
  mutate(tidy = map(seed, sim_data_fit, 33)) %>% 
  unnest(tidy)
```

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta above 0.94 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.94 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    
    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#tail-ess

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.94 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## `summarise()` ungrouping output (override with `.groups` argument)
    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Start sampling

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
# power for effect of syntax
sim2 %>%
  filter(key == "syntax_critical") %>%
  mutate(check_syntax = ifelse(lower > 0, 1, 0)) %>%
  summarise(power_syntax = mean(check_syntax))
```

    ## # A tibble: 1 x 1
    ##   power_syntax
    ##          <dbl>
    ## 1         0.66

``` r
# power for interaction 
#sim2 %>%
#  filter(key == "b_syntax_devpred:trial_devcritical") %>%
#  mutate(check_interaction = ifelse(((lower < 0) & (upper > 0)), 1, 0)) %>%
#  summarise(power_interaction = mean(check_interaction))
```
