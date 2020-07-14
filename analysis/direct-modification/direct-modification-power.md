Direct Modification Power Analysis
================
Polina Tsvilodub
7/13/2020

In this document a simulation-based power analysis for the direct
modification study series can be found.

This power analysis is based on pilot data from pilots 5 and 6 (see
`modificationXrefUt-pilot2.Rmd`). The maximal model including *main
effects of syntax* (subject vs predicate-N), *trial type* (critical
vs. filler), their *interaction and maximal random effect structure*
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
  cores = 4
  #control = list(adapt_delta = 0.95)
)
```

    ## Compiling the C++ model

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
    ## sd(Intercept)                               0.29      0.23     0.01     0.88
    ## sd(syntax_dev1)                             0.40      0.32     0.02     1.16
    ## sd(trial_dev1)                              0.59      0.38     0.03     1.47
    ## sd(syntax_dev1:trial_dev1)                  0.64      0.43     0.04     1.66
    ## cor(Intercept,syntax_dev1)                  0.01      0.45    -0.80     0.82
    ## cor(Intercept,trial_dev1)                   0.05      0.46    -0.80     0.84
    ## cor(syntax_dev1,trial_dev1)                -0.08      0.44    -0.84     0.75
    ## cor(Intercept,syntax_dev1:trial_dev1)       0.08      0.45    -0.78     0.85
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)    -0.02      0.44    -0.82     0.79
    ## cor(trial_dev1,syntax_dev1:trial_dev1)      0.13      0.43    -0.74     0.85
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.00     2240     2110
    ## sd(syntax_dev1)                         1.00     1637     2204
    ## sd(trial_dev1)                          1.01      931     1207
    ## sd(syntax_dev1:trial_dev1)              1.00      950     1417
    ## cor(Intercept,syntax_dev1)              1.00     3493     2980
    ## cor(Intercept,trial_dev1)               1.00     2089     2607
    ## cor(syntax_dev1,trial_dev1)             1.00     2291     3015
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     1845     2471
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00     2488     2904
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     2309     2879
    ## 
    ## ~workerid (Number of levels: 47) 
    ##                                         Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                               4.11      0.90     2.68     6.18
    ## sd(syntax_dev1)                             0.67      0.43     0.04     1.64
    ## sd(trial_dev1)                              1.24      0.52     0.34     2.41
    ## sd(syntax_dev1:trial_dev1)                  0.51      0.40     0.01     1.51
    ## cor(Intercept,syntax_dev1)                 -0.00      0.42    -0.77     0.78
    ## cor(Intercept,trial_dev1)                   0.48      0.32    -0.30     0.91
    ## cor(syntax_dev1,trial_dev1)                -0.01      0.40    -0.76     0.73
    ## cor(Intercept,syntax_dev1:trial_dev1)      -0.31      0.45    -0.93     0.66
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)     0.02      0.44    -0.81     0.80
    ## cor(trial_dev1,syntax_dev1:trial_dev1)     -0.07      0.42    -0.80     0.74
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.01      821     1503
    ## sd(syntax_dev1)                         1.00      750     1635
    ## sd(trial_dev1)                          1.00      903      899
    ## sd(syntax_dev1:trial_dev1)              1.00     1006     1381
    ## cor(Intercept,syntax_dev1)              1.00     3664     2513
    ## cor(Intercept,trial_dev1)               1.00     1729     2085
    ## cor(syntax_dev1,trial_dev1)             1.00     1136     1898
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     2495     2368
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00     2650     2811
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     2986     2664
    ## 
    ## Population-Level Effects: 
    ##                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                  2.21      0.76     0.83     3.85 1.00      650
    ## syntax_dev1                0.94      0.36     0.30     1.73 1.00     1906
    ## trial_dev1                 0.30      0.44    -0.48     1.21 1.00     1560
    ## syntax_dev1:trial_dev1    -0.01      0.37    -0.76     0.72 1.00     1521
    ##                        Tail_ESS
    ## Intercept                  1179
    ## syntax_dev1                2020
    ## trial_dev1                 2067
    ## syntax_dev1:trial_dev1     1758
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

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
    ## sd(Intercept)                               0.57      0.48     0.02     1.78
    ## sd(syntax_dev1)                             0.51      0.41     0.02     1.54
    ## sd(trial_dev1)                              0.54      0.44     0.02     1.61
    ## sd(syntax_dev1:trial_dev1)                  1.76      0.77     0.63     3.64
    ## cor(Intercept,syntax_dev1)                 -0.02      0.45    -0.82     0.80
    ## cor(Intercept,trial_dev1)                   0.07      0.45    -0.78     0.85
    ## cor(syntax_dev1,trial_dev1)                -0.04      0.45    -0.83     0.80
    ## cor(Intercept,syntax_dev1:trial_dev1)      -0.03      0.43    -0.80     0.78
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)     0.13      0.44    -0.74     0.86
    ## cor(trial_dev1,syntax_dev1:trial_dev1)      0.13      0.43    -0.72     0.85
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.00     2237     2822
    ## sd(syntax_dev1)                         1.00     2985     3928
    ## sd(trial_dev1)                          1.00     2443     3546
    ## sd(syntax_dev1:trial_dev1)              1.00     1830     3398
    ## cor(Intercept,syntax_dev1)              1.00     7085     4818
    ## cor(Intercept,trial_dev1)               1.00     5731     4445
    ## cor(syntax_dev1,trial_dev1)             1.00     5938     4869
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     2158     2979
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00     2606     3994
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     2853     4654
    ## 
    ## ~workerid (Number of levels: 47) 
    ##                                         Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                               5.75      1.63     3.38     9.75
    ## sd(syntax_dev1)                             1.38      0.75     0.14     3.09
    ## sd(trial_dev1)                              2.60      0.83     1.32     4.49
    ## sd(syntax_dev1:trial_dev1)                  1.12      0.68     0.07     2.70
    ## cor(Intercept,syntax_dev1)                 -0.14      0.38    -0.80     0.60
    ## cor(Intercept,trial_dev1)                   0.10      0.30    -0.49     0.65
    ## cor(syntax_dev1,trial_dev1)                -0.22      0.34    -0.81     0.49
    ## cor(Intercept,syntax_dev1:trial_dev1)       0.10      0.40    -0.69     0.80
    ## cor(syntax_dev1,syntax_dev1:trial_dev1)    -0.29      0.39    -0.90     0.57
    ## cor(trial_dev1,syntax_dev1:trial_dev1)      0.18      0.37    -0.59     0.82
    ##                                         Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)                           1.00     1238     2335
    ## sd(syntax_dev1)                         1.01      970     1444
    ## sd(trial_dev1)                          1.00     1561     2583
    ## sd(syntax_dev1:trial_dev1)              1.00     1347     2720
    ## cor(Intercept,syntax_dev1)              1.00     4897     4165
    ## cor(Intercept,trial_dev1)               1.00     3444     4349
    ## cor(syntax_dev1,trial_dev1)             1.00      969     1850
    ## cor(Intercept,syntax_dev1:trial_dev1)   1.00     5399     4409
    ## cor(syntax_dev1,syntax_dev1:trial_dev1) 1.00     2560     3313
    ## cor(trial_dev1,syntax_dev1:trial_dev1)  1.00     4255     4556
    ## 
    ## Population-Level Effects: 
    ##                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                  3.27      1.21     1.29     6.06 1.00     1664
    ## syntax_dev1                1.61      0.66     0.54     3.15 1.00     2151
    ## trial_dev1                 0.33      0.68    -0.99     1.74 1.00     3413
    ## syntax_dev1:trial_dev1    -0.37      0.78    -1.99     1.16 1.00     3322
    ##                        Tail_ESS
    ## Intercept                  2525
    ## syntax_dev1                2147
    ## trial_dev1                 3369
    ## syntax_dev1:trial_dev1     3406
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

### Simulate data & fit the model

``` r
# helper function to get posterior predictive draws for a subset of participants of size N, N <= 47 
# d is the a data frame with posterior predictive draws for 47 participants based on the pilot data set 
get_new_data <- function(d, N) {
    if(N == 0) {
      data <- d
    } else { # if N > 0
      data <- add_predicted_draws(model=pilot_model, 
                                     newdata = pilot_data %>% # get a random subset of pilot data to compute posterior samples on
                                       filter(workerid %in% sample(unique(pilot_data$workerid), N, replace = F)),
                                     n = 1) %>% 
                 mutate(workerid = paste(workerid, letters[1], sep = "_")) %>%
                   rbind(., d) # append the samples computed on the data subset to the full batch of posterior samples drawn in this iteration 
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
  # update model fit with new data
  update(predicted_fit,
         newdata = data,
         seed = seed) %>% 
    tidy(prob = 0.95)
}
```

### Power analysis

Here is a small test run of the simulation for 47 participants:

``` r
sim1 <-
  tibble(seed = 1:5) %>% 
  mutate(tidy = map(seed, sim_data_fit, 0)) %>% 
  unnest(tidy)
```

    ## Start sampling
    ## Start sampling
    ## Start sampling
    ## Start sampling
    ## Start sampling

``` r
# power for effect of syntax
# power for the syntax effect for 47 participants is around 0.75
sim1 %>%
  filter(term == "b_syntax_dev1") %>%
  mutate(check_syntax = ifelse(lower > 0, 1, 0)) %>%
  summarise(power_syntax = mean(check_syntax))
```

    ## # A tibble: 1 x 1
    ##   power_syntax
    ##          <dbl>
    ## 1          0.6

``` r
# power for interaction 
sim1 %>%
  filter(term == "b_syntax_dev1:trial_dev1") %>%
  mutate(check_interaction = ifelse(((lower < 0) & (upper > 0)), 1, 0)) %>%
  summarise(power_interaction = mean(check_interaction))
```

    ## # A tibble: 1 x 1
    ##   power_interaction
    ##               <dbl>
    ## 1                 1

Simulation of 60 participants over 1000 iterations:

``` r
# simulate data and analysis for 60 participants
sim2 <-
  tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed, sim_data_fit, 13)) %>% 
  unnest(tidy)
```

``` r
# power for effect of syntax
sim2 %>%
  filter(term == "b_syntax_dev1") %>%
  mutate(check_syntax = ifelse(lower > 0, 1, 0)) %>%
  summarise(power_syntax = mean(check_syntax))

# power for interaction 
sim2 %>%
  filter(term == "b_syntax_dev1:trial_dev1") %>%
  mutate(check_interaction = ifelse(((lower < 0) & (upper > 0)), 1, 0)) %>%
  summarise(power_interaction = mean(check_interaction))
```
