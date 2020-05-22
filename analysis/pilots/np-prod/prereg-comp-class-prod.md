Comparison Class Production
================
Polina Tsvilodub
8/12/2019

## Experiment outline

This script analyses the data from a comparison class production
experiment with (n=X).

In a between-subject design we manipulate the syntactic frame in which
the subjects elicit the comparison class with respect to size of a
target obejct: “That X is big” (predicative condition) and “That’s a big
X” (prenominal condition). We use both adjectives ‘big’ and ‘small’,
depending on the target. The targets are chosen such that they are
obviously big (or small) members of their superordinate category, but
normal-sized for their subordinate category. A stimulus consists of a
context group, for example different-sized dogs (superordinate
category), and the target, a normal-sized great dane (a big dog) or a
normal-sized chihuahua (a small dog) (subordinate categories).

Due to syntactic frame manipulation, the participans are expected to use
different comparison classes (superordinate labels versus subordinate
labels of the target) in order to communicate the situation presented to
them in the stimulus.

We expect *more superordinate labels to occur in the prenominal
condition*, since the NP restricts the comparison class more strongly to
the category used in the NP. In contrast, we expect *a similar
proportion of superordinate and subordinate labels in the predicative
condition*, since the comparison class is less
    restricted.

# Data analysis

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.7
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts -------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidyboot)
```

    ## Warning: package 'tidyboot' was built under R version 3.5.3

``` r
# read in data
d1 <- read_csv('./../data/results_8_exp1-post-prereg-pred.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   submission_id = col_integer(),
    ##   experiment_id = col_integer(),
    ##   enjoyment = col_integer(),
    ##   age = col_integer(),
    ##   trial_number = col_integer(),
    ##   startTime = col_double(),
    ##   attempts = col_integer(),
    ##   fairprice = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
d2 <- read_csv('./../data/results_10_exp1-post-prereg-prenom.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   submission_id = col_integer(),
    ##   experiment_id = col_integer(),
    ##   enjoyment = col_integer(),
    ##   age = col_integer(),
    ##   trial_number = col_integer(),
    ##   startTime = col_double(),
    ##   attempts = col_integer(),
    ##   fairprice = col_double()
    ## )
    ## See spec(...) for full column specifications.

``` r
d <- rbind(d1, d2)

#first look at the data
glimpse(d)
```

    ## Observations: 1,029
    ## Variables: 41
    ## $ submission_id   <int> 341, 341, 341, 341, 341, 341, 341, 341, 341, 3...
    ## $ response1       <chr> "swordfish", "tall tree", "cedar", "pine", "tr...
    ## $ startDate       <chr> "Wed Aug 21 2019 14:55:05 GMT-0500 (CDT)", "We...
    ## $ context_picture <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ experiment_id   <int> 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8...
    ## $ picture2        <chr> "warmup/goldfish.png", "warmup/bonsai.jpg", "w...
    ## $ correct1        <chr> "swordfish", "redwood|sequoia", "redwood|sequo...
    ## $ question1       <chr> "This is a", "This is a", "This is a", "This i...
    ## $ problems        <chr> "no", "no", "no", "no", "no", "no", "no", "no"...
    ## $ enjoyment       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ hit_id          <chr> "3ZZAYRN1I6FJ5KHUN3YSSTTS1K6OTT", "3ZZAYRN1I6F...
    ## $ item            <chr> "fish", "trees", "trees", "trees", "trees", "t...
    ## $ botresponse     <chr> "susan", "susan", "susan", "susan", "susan", "...
    ## $ assignment_id   <chr> "3S4AW7T80C6U5YX0LCMUGO9UECL4LV", "3S4AW7T80C6...
    ## $ correct2        <chr> "goldfish", "bonsai", "bonsai", "bonsai", "bon...
    ## $ understand      <chr> "yes", "yes", "yes", "yes", "yes", "yes", "yes...
    ## $ text            <chr> "Please label the pictures below.", "Please la...
    ## $ condition       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ sentence        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ age             <int> 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65...
    ## $ response        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ comments        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ picture1        <chr> "warmup/swordfish.jpg", "warmup/sequoia.jpg", ...
    ## $ response3       <chr> "fish", "trees", "trees", "trees", "trees", "e...
    ## $ education       <chr> "graduated_college", "graduated_college", "gra...
    ## $ worker_id       <chr> "A2BLQ1GVEHJR8T", "A2BLQ1GVEHJR8T", "A2BLQ1GVE...
    ## $ languages       <chr> "English", "English", "English", "English", "E...
    ## $ sex             <chr> "female", "female", "female", "female", "femal...
    ## $ RT              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ correct3        <chr> "fish", "trees", "trees", "trees", "trees", "t...
    ## $ context         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ trial_number    <int> 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 1, 2, 3, 1...
    ## $ startTime       <dbl> 1.566417e+12, 1.566417e+12, 1.566417e+12, 1.56...
    ## $ attempts        <int> 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, NA, NA, NA...
    ## $ picture         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ question3       <chr> "This is a", "This is a", "This is a", "This i...
    ## $ fairprice       <dbl> 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00...
    ## $ question2       <chr> "These are both", "These are both", "These are...
    ## $ size            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ response2       <chr> "goldfish", "bonsai", "bonsai", "bonsai", "bon...
    ## $ trial_name      <chr> "warmup1", "warmup1", "warmup1", "warmup1", "w...

``` r
# comments
d_comments <- d %>%
  distinct(submission_id, problems, comments, fairprice)

# number of big / small targets per condition
d %>% group_by(condition, size) %>% count()
```

    ## # A tibble: 5 x 3
    ## # Groups:   condition, size [5]
    ##   condition   size      n
    ##   <chr>       <chr> <int>
    ## 1 predicative big      90
    ## 2 predicative small    90
    ## 3 prenominal  big      90
    ## 4 prenominal  small    90
    ## 5 <NA>        <NA>    669

The participants recruited via MTurk were paid $1.00.

\#\#Spliting data into main and warm-up, excluding participants

Subject exclusion

``` r
# make sure how participants indicate their native language 
# sometimes participants use only "en" or "eng" for english
# excluded non-native speakers 
d %>% distinct(d$languages) %>% View()


d_filt <- d %>% 
  filter(grepl("eng", languages, ignore.case = T)) %>%
  select(submission_id, trial_name, trial_number, size, item, botresponse, response, condition,  picture) %>% mutate(size=factor(size), syntax = factor(condition))

glimpse(d_filt)
```

    ## Observations: 1,029
    ## Variables: 10
    ## $ submission_id <int> 341, 341, 341, 341, 341, 341, 341, 341, 341, 341...
    ## $ trial_name    <chr> "warmup1", "warmup1", "warmup1", "warmup1", "war...
    ## $ trial_number  <int> 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 1, 2, 3, 1, ...
    ## $ size          <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ item          <chr> "fish", "trees", "trees", "trees", "trees", "tre...
    ## $ botresponse   <chr> "susan", "susan", "susan", "susan", "susan", "su...
    ## $ response      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ condition     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ picture       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ syntax        <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...

``` r
# exclude participants if necessary based on botcaptcha 
d_filt %>% distinct(d_filt$botresponse) %>% View()

# extract main trials 
d_main <- d_filt %>% filter((trial_name =="main1") & (trial_number== 1)  ) %>% 
  select(submission_id, trial_number, response,  size, item, syntax, picture)
```

## Categorizing the data

``` r
# question1 and question2 are the sentence parts coded in the experiment 
# look at the different responses provided and categorize them 
d_main %>% distinct(d_main$response) %>% View()

# exclude if responses are not referring to the target 
d_main_valid <- subset(d_main, !(response %in% c("rose", "duck"))) 

d_main_responseCat <- d_main_valid %>%
  rowwise() %>%
  mutate( # categorize responses 
    response_cat =
      ifelse(
      tolower(response) %in% c("bird", "birds", "dog", "dogs", "fish","one plant", "flower", "flowers", "tree", "trees", "animal", "plant"), "super", "sub"),
    
    resp_cat = ifelse(response_cat == "sub", 1, 0),
    response_label = "sub"
  )
```

## Response category distribution

``` r
d_main_responseCat %>% mutate(condition = factor(syntax, 
                            levels = c("prenominal", "predicative"),
                            labels= c("That's a big X", "That X is big"))) %>% ggplot(aes(x=response_cat)) +geom_bar(position=position_dodge()) + facet_wrap(~syntax)
```

![](prereg-comp-class-prod_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Proportion of subordinate responses by condition, separated by congruency

The proportion of subordinate responses is the dependent variable we
test by manipulating the syntactic condition: “That’s a big X”
(prenominal) versus “That X is big” (predicative).

``` r
d_main_summary <- d_main_responseCat %>%
  group_by(syntax, response_label) %>%
  tidyboot_mean(column = resp_cat) %>% # calculate proportion of subordinate labels in the different conditions 
  ungroup() %>%
  mutate(condition = factor(syntax, 
                            levels = c("prenominal", "predicative"),
                            labels= c("That's a big X", "That X is big"))
        )
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

``` r
ggplot(d_main_summary, aes(x = syntax, fill = syntax,
                           y = mean, ymin = ci_lower, ymax = ci_upper))+
  geom_col(position = position_dodge(0.8))+
  geom_linerange(position = position_dodge(0.8))+
  labs( y = "Proportion subordinate responses")+
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))+
  ggtitle("The proportion of subordinate responses by syntactic condition")
```

![](prereg-comp-class-prod_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
 # facet_wrap(~condition)
```

## Bayesian stats

``` r
library(brms)
```

    ## Warning: package 'brms' was built under R version 3.5.3

    ## Loading required package: Rcpp

    ## Loading 'brms' package (version 2.8.0). Useful instructions
    ## can be found by typing help('brms'). A more detailed introduction
    ## to the package is available through vignette('brms_overview').

``` r
# effect coding
#contrasts(d_main_responseCat$syntax)=contr.sum(2)
contrasts(d_main_responseCat$syntax)=matrix(c(-1, 1))
# fit regression model
lm.bayes <- brm(resp_cat ~ syntax + (1| submission_id) + (1 + syntax|picture), data = d_main_responseCat, family = "bernoulli")
```

    ## Compiling the C++ model

    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '1b1423e832ba07da4efffd5a83eb433e' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.869 seconds (Warm-up)
    ## Chain 1:                6.413 seconds (Sampling)
    ## Chain 1:                16.282 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '1b1423e832ba07da4efffd5a83eb433e' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 14.383 seconds (Warm-up)
    ## Chain 2:                17.726 seconds (Sampling)
    ## Chain 2:                32.109 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '1b1423e832ba07da4efffd5a83eb433e' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.982 seconds (Warm-up)
    ## Chain 3:                7.143 seconds (Sampling)
    ## Chain 3:                17.125 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '1b1423e832ba07da4efffd5a83eb433e' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 7.927 seconds (Warm-up)
    ## Chain 4:                8.869 seconds (Sampling)
    ## Chain 4:                16.796 seconds (Total)
    ## Chain 4:

    ## Warning: There were 4 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ## Warning: Examine the pairs() plot to diagnose sampling problems

``` r
summary(lm.bayes)
```

    ## Warning: There were 4 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help.
    ## See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: resp_cat ~ syntax + (1 | submission_id) + (1 + syntax | picture) 
    ##    Data: d_main_responseCat (Number of observations: 60) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~picture (Number of levels: 20) 
    ##                        Estimate Est.Error l-95% CI u-95% CI Eff.Sample
    ## sd(Intercept)              7.89      6.87     0.32    25.33       1512
    ## sd(syntax1)                7.53      6.64     0.24    23.93       1340
    ## cor(Intercept,syntax1)    -0.08      0.58    -0.97     0.93       1837
    ##                        Rhat
    ## sd(Intercept)          1.00
    ## sd(syntax1)            1.00
    ## cor(Intercept,syntax1) 1.00
    ## 
    ## ~submission_id (Number of levels: 60) 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sd(Intercept)    52.76     40.79    11.61   168.52        900 1.00
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## Intercept    -0.28      6.60   -13.89    13.22       1323 1.00
    ## syntax1      -3.67     11.01   -30.22    16.69        678 1.01
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#extract posterior samples
pos_samples <- posterior_samples(lm.bayes)
head(pos_samples)
```

    ##   b_Intercept b_syntax1 sd_picture__Intercept sd_picture__syntax1
    ## 1   9.5512972  7.724869              5.699118            7.929026
    ## 2   6.1680425  1.597429              1.740104            3.859528
    ## 3  11.7593002  7.589353              9.579525            5.430816
    ## 4  -0.4853585 -8.408536              6.446033            4.655848
    ## 5  -1.4928372 -2.231281             13.933849            4.513829
    ## 6  -4.1381670 -2.123137              9.360096            2.547370
    ##   sd_submission_id__Intercept cor_picture__Intercept__syntax1
    ## 1                    60.18670                     -0.26020653
    ## 2                    30.59656                      0.40785454
    ## 3                    52.49696                      0.00567682
    ## 4                    43.43714                      0.25680135
    ## 5                    54.10259                     -0.33994704
    ## 6                    20.14003                      0.46188498
    ##   r_picture[images/bonsai.png,Intercept]
    ## 1                              0.2290264
    ## 2                              2.2141447
    ## 3                              1.3458323
    ## 4                             -3.1517612
    ## 5                             -2.7322531
    ## 6                              2.2432718
    ##   r_picture[images/canary.png,Intercept]
    ## 1                             -0.3855657
    ## 2                              1.4033296
    ## 3                             -0.1308449
    ## 4                             -0.8044058
    ## 5                            -24.9553071
    ## 6                              0.5319982
    ##   r_picture[images/chihuahua.png,Intercept]
    ## 1                                 -4.008851
    ## 2                                 -1.356676
    ## 3                                 -2.844630
    ## 4                                 -2.516547
    ## 5                                -18.060839
    ## 6                                  6.185862
    ##   r_picture[images/clownfish.png,Intercept]
    ## 1                                -0.3036790
    ## 2                                -0.7267916
    ## 3                               -15.7402192
    ## 4                                 2.6522632
    ## 5                               -10.4345590
    ## 6                                 6.1095187
    ##   r_picture[images/daisy.png,Intercept]
    ## 1                              5.218318
    ## 2                             -0.489555
    ## 3                              4.742063
    ## 4                             -5.293856
    ## 5                             -8.975164
    ## 6                              3.647296
    ##   r_picture[images/dandelion.png,Intercept]
    ## 1                                -0.6098727
    ## 2                                 1.3279034
    ## 3                                -1.8741781
    ## 4                                -1.2246721
    ## 5                               -10.6373721
    ## 6                                 9.9721829
    ##   r_picture[images/doberman.png,Intercept]
    ## 1                               -5.1143906
    ## 2                                2.0454591
    ## 3                                0.6883688
    ## 4                               -7.8562202
    ## 5                               -1.6767219
    ## 6                               -3.1570098
    ##   r_picture[images/eagle.png,Intercept]
    ## 1                            -0.6576848
    ## 2                            -1.3014684
    ## 3                           -11.9102691
    ## 4                             8.0778490
    ## 5                            -7.8088259
    ## 6                             5.0069399
    ##   r_picture[images/goldfish.png,Intercept]
    ## 1                               -2.6882746
    ## 2                               -1.2246079
    ## 3                               -2.9216268
    ## 4                              -10.8417469
    ## 5                              -11.2859076
    ## 6                                0.7091038
    ##   r_picture[images/goose.png,Intercept]
    ## 1                             1.9649198
    ## 2                             0.9213259
    ## 3                            10.3939876
    ## 4                           -10.6942028
    ## 5                           -13.5505361
    ## 6                            25.1473969
    ##   r_picture[images/great-dane.png,Intercept]
    ## 1                                  -5.181700
    ## 2                                  -1.147454
    ## 3                                 -16.454987
    ## 4                                   6.834615
    ## 5                                   9.170909
    ## 6                                   4.597228
    ##   r_picture[images/hummingbird.png,Intercept]
    ## 1                                  -5.0030090
    ## 2                                   0.4026219
    ## 3                                   6.3067210
    ## 4                                  -1.5957616
    ## 5                                   6.3189820
    ## 6                                 -11.0222881
    ##   r_picture[images/peony.png,Intercept]
    ## 1                             7.1963896
    ## 2                             2.9887325
    ## 3                            13.4114149
    ## 4                             0.6674791
    ## 5                           -10.1685604
    ## 6                            -3.3445765
    ##   r_picture[images/pug.png,Intercept]
    ## 1                            1.485102
    ## 2                           -2.335323
    ## 3                          -11.779110
    ## 4                           -3.058692
    ## 5                           -4.197136
    ## 6                           -4.552093
    ##   r_picture[images/sequoia.png,Intercept]
    ## 1                             -13.0173632
    ## 2                              -2.7286006
    ## 3                             -17.7218607
    ## 4                              -0.8865145
    ## 5                              -5.2423703
    ## 6                              -0.8510725
    ##   r_picture[images/sparrow.png,Intercept]
    ## 1                              -1.2020475
    ## 2                              -2.0708115
    ## 3                               0.9673095
    ## 4                              -2.6808134
    ## 5                              -7.9626043
    ## 6                               5.7940154
    ##   r_picture[images/sunflower.png,Intercept]
    ## 1                                -1.6586852
    ## 2                                 0.2407703
    ## 3                                -8.1317200
    ## 4                                 2.0170240
    ## 5                               -16.3248213
    ## 6                                -0.7524110
    ##   r_picture[images/swan.png,Intercept]
    ## 1                            4.4495710
    ## 2                            0.4067717
    ## 3                           16.9883892
    ## 4                            8.1702509
    ## 5                           20.4084979
    ## 6                           -6.1882465
    ##   r_picture[images/swordfish.png,Intercept]
    ## 1                               -0.06151202
    ## 2                                0.47441244
    ## 3                              -14.36022561
    ## 4                               -3.35613350
    ## 5                              -19.42041136
    ## 6                               16.71058196
    ##   r_picture[images/tuna.png,Intercept]
    ## 1                            -1.200788
    ## 2                            -1.690842
    ## 3                             3.286249
    ## 4                            -4.511522
    ## 5                            -2.571088
    ## 6                             1.333052
    ##   r_picture[images/bonsai.png,syntax1]
    ## 1                           -1.6264265
    ## 2                            3.2513545
    ## 3                           -5.4999985
    ## 4                            2.6360748
    ## 5                           -0.1491587
    ## 6                           -4.3737711
    ##   r_picture[images/canary.png,syntax1]
    ## 1                            10.510066
    ## 2                             5.252614
    ## 3                             9.367708
    ## 4                            -4.474872
    ## 5                            -3.842275
    ## 6                             2.988632
    ##   r_picture[images/chihuahua.png,syntax1]
    ## 1                              -6.0706675
    ## 2                              -3.0648722
    ## 3                              -2.0074333
    ## 4                              -1.1450678
    ## 5                               1.0220533
    ## 6                              -0.3210786
    ##   r_picture[images/clownfish.png,syntax1]
    ## 1                              -0.3658858
    ## 2                               3.7030687
    ## 3                               7.5794098
    ## 4                              11.2227899
    ## 5                              -1.2001080
    ## 6                               1.4986642
    ##   r_picture[images/daisy.png,syntax1]
    ## 1                           2.8728624
    ## 2                           2.4819016
    ## 3                           2.0362406
    ## 4                           4.8276370
    ## 5                           2.9201154
    ## 6                           0.3281048
    ##   r_picture[images/dandelion.png,syntax1]
    ## 1                              -3.0939054
    ## 2                              -2.9635052
    ## 3                              -9.3437587
    ## 4                               2.5903902
    ## 5                               5.1469882
    ## 6                              -0.5592876
    ##   r_picture[images/doberman.png,syntax1]
    ## 1                              2.8557225
    ## 2                              0.2088405
    ## 3                              0.2520903
    ## 4                             -0.6196696
    ## 5                             -1.9088992
    ## 6                              0.3000992
    ##   r_picture[images/eagle.png,syntax1]
    ## 1                          -0.2539466
    ## 2                          -2.8659439
    ## 3                          -3.9328614
    ## 4                           1.2527273
    ## 5                           8.8376935
    ## 6                          -3.3617167
    ##   r_picture[images/goldfish.png,syntax1]
    ## 1                            -12.2615126
    ## 2                             -2.1981129
    ## 3                              5.1418185
    ## 4                              0.7414851
    ## 5                              6.1186034
    ## 6                             -1.3252783
    ##   r_picture[images/goose.png,syntax1]
    ## 1                          -9.0798542
    ## 2                          -4.1547992
    ## 3                          -0.3557205
    ## 4                          -4.0260793
    ## 5                           8.1246446
    ## 6                          -0.6162746
    ##   r_picture[images/great-dane.png,syntax1]
    ## 1                               10.3147668
    ## 2                                0.8219998
    ## 3                                2.4712442
    ## 4                                0.7323159
    ## 5                               -3.1146627
    ## 6                                1.2779997
    ##   r_picture[images/hummingbird.png,syntax1]
    ## 1                                 -1.480840
    ## 2                                 -3.516502
    ## 3                                 -9.126130
    ## 4                                  5.758732
    ## 5                                 -3.877681
    ## 6                                  1.241390
    ##   r_picture[images/peony.png,syntax1] r_picture[images/pug.png,syntax1]
    ## 1                         10.08033871                        -8.0143186
    ## 2                          8.61360625                        -2.5997757
    ## 3                          6.36997029                        -0.8550166
    ## 4                          0.97338045                        -1.6981759
    ## 5                          0.20373421                        -7.0670974
    ## 6                         -0.08897872                         3.1678137
    ##   r_picture[images/sequoia.png,syntax1]
    ## 1                              7.899106
    ## 2                              4.464354
    ## 3                             10.476006
    ## 4                              3.631761
    ## 5                              6.474478
    ## 6                             -3.469678
    ##   r_picture[images/sparrow.png,syntax1]
    ## 1                            -0.6262766
    ## 2                            -1.2043390
    ## 3                             3.6223340
    ## 4                            -0.7145636
    ## 5                             3.0678479
    ## 6                            -1.7929006
    ##   r_picture[images/sunflower.png,syntax1]
    ## 1                              -1.6107471
    ## 2                              -2.2612974
    ## 3                              -2.8363965
    ## 4                              -0.4249306
    ## 5                               0.3176237
    ## 6                               2.4856358
    ##   r_picture[images/swan.png,syntax1]
    ## 1                         -7.7402124
    ## 2                         -0.4829586
    ## 3                          1.8911741
    ## 4                         -4.0717936
    ## 5                         -1.6582597
    ## 6                          0.4562395
    ##   r_picture[images/swordfish.png,syntax1]
    ## 1                               -9.803170
    ## 2                                1.862277
    ## 3                               -2.639933
    ## 4                               -9.985529
    ## 5                               -1.378052
    ## 6                                3.588174
    ##   r_picture[images/tuna.png,syntax1] r_submission_id[285,Intercept]
    ## 1                       -1.555926235                       31.19075
    ## 2                        0.007618729                       16.41611
    ## 3                        6.791298639                       34.87301
    ## 4                       -3.853367125                       51.06936
    ## 5                       -0.760317604                       82.31264
    ## 6                        0.562579495                       33.66463
    ##   r_submission_id[286,Intercept] r_submission_id[287,Intercept]
    ## 1                     -58.907610                     -15.134756
    ## 2                     -66.755691                      -4.221241
    ## 3                    -108.079141                     -56.055147
    ## 4                      -3.487140                     -70.092572
    ## 5                      -4.658373                     -36.093574
    ## 6                     -26.342620                     -26.926565
    ##   r_submission_id[288,Intercept] r_submission_id[289,Intercept]
    ## 1                      -9.127402                      101.37123
    ## 2                      13.819946                       42.78846
    ## 3                      -0.527336                       65.31321
    ## 4                      14.033803                      111.27386
    ## 5                      17.993982                       89.22398
    ## 6                      51.336401                      -13.12763
    ##   r_submission_id[290,Intercept] r_submission_id[291,Intercept]
    ## 1                     -24.818083                     -43.133872
    ## 2                     -18.308582                     -27.630588
    ## 3                     -66.231296                     -28.278629
    ## 4                     -10.091792                     -29.344234
    ## 5                      -5.840631                     -67.916902
    ## 6                      -5.228040                      -8.796655
    ##   r_submission_id[292,Intercept] r_submission_id[293,Intercept]
    ## 1                     -37.196022                     -70.656977
    ## 2                      -2.598294                     -49.324684
    ## 3                     -21.085076                     -82.028779
    ## 4                     -39.447292                     -38.243380
    ## 5                     -57.416999                     -24.986046
    ## 6                     -24.742726                      -6.570541
    ##   r_submission_id[294,Intercept] r_submission_id[295,Intercept]
    ## 1                     -22.235339                      -30.91075
    ## 2                     -35.073595                      -18.53450
    ## 3                     -52.152479                       -2.54357
    ## 4                     -35.816864                      -40.81776
    ## 5                     -74.619936                      -73.87653
    ## 6                      -6.940379                      -20.74927
    ##   r_submission_id[296,Intercept] r_submission_id[297,Intercept]
    ## 1                      32.928332                     100.185837
    ## 2                      17.285062                      27.339382
    ## 3                       6.834468                       2.548021
    ## 4                      37.271973                      41.131448
    ## 5                      48.573697                      39.186789
    ## 6                      12.585305                       9.101915
    ##   r_submission_id[298,Intercept] r_submission_id[299,Intercept]
    ## 1                     44.0600254                       11.27548
    ## 2                     -5.0559707                      -13.22215
    ## 3                      9.9394383                      -19.58602
    ## 4                     10.8980951                      -28.24225
    ## 5                     73.0016935                      -33.74224
    ## 6                      0.1512329                      -15.65893
    ##   r_submission_id[300,Intercept] r_submission_id[301,Intercept]
    ## 1                     -46.347348                       46.01071
    ## 2                     -17.737214                       17.06851
    ## 3                     -83.451577                       91.76040
    ## 4                      12.923068                       21.75782
    ## 5                       5.298439                       60.04298
    ## 6                     -11.972500                       12.96532
    ##   r_submission_id[302,Intercept] r_submission_id[303,Intercept]
    ## 1                    -132.315449                      -37.62392
    ## 2                     -12.071075                      -28.32085
    ## 3                     -14.825192                      -35.56460
    ## 4                     -85.103209                      -22.49557
    ## 5                     -33.719711                      -12.40597
    ## 6                      -7.153158                      -10.02615
    ##   r_submission_id[304,Intercept] r_submission_id[305,Intercept]
    ## 1                      16.349206                     -19.944581
    ## 2                      12.864497                     -11.057797
    ## 3                      26.746153                     -84.668279
    ## 4                      25.059764                     -53.656781
    ## 5                      11.381061                      -5.247925
    ## 6                       8.204837                      -7.285734
    ##   r_submission_id[306,Intercept] r_submission_id[307,Intercept]
    ## 1                      -97.27877                     -52.756969
    ## 2                      -52.66367                      -6.601474
    ## 3                     -133.91801                     -38.500902
    ## 4                     -115.06761                      -8.493258
    ## 5                      -78.55422                     -41.014099
    ## 6                      -31.72285                      -6.303202
    ##   r_submission_id[308,Intercept] r_submission_id[309,Intercept]
    ## 1                    34.44501818                      -49.77890
    ## 2                     0.41856644                      -35.87333
    ## 3                    -0.08228452                      -65.99275
    ## 4                    44.02440844                      -51.49723
    ## 5                    11.44658265                      -64.05547
    ## 6                    15.37533540                      -27.60559
    ##   r_submission_id[310,Intercept] r_submission_id[311,Intercept]
    ## 1                       65.55280                      47.546480
    ## 2                       31.50998                      28.031002
    ## 3                       61.58305                      -1.783088
    ## 4                       68.08781                      19.212119
    ## 5                       37.48191                      24.845231
    ## 6                       11.80427                      12.533199
    ##   r_submission_id[312,Intercept] r_submission_id[313,Intercept]
    ## 1                      -3.454955                    -75.4123375
    ## 2                      14.495425                    -20.7251955
    ## 3                      65.354714                    -47.0807086
    ## 4                      77.922581                    -23.3258465
    ## 5                      74.614194                    -84.4178667
    ## 6                      26.827308                     -0.3012577
    ##   r_submission_id[314,Intercept] r_submission_id[315,Intercept]
    ## 1                     -12.042737                      29.243784
    ## 2                     -35.269659                      18.667131
    ## 3                     -55.347745                      47.471577
    ## 4                     -30.508481                      35.306461
    ## 5                      -4.795698                       8.644004
    ## 6                     -15.203368                       6.707190
    ##   r_submission_id[316,Intercept] r_submission_id[317,Intercept]
    ## 1                    161.9291360                      0.7769867
    ## 2                    109.9404406                     13.6745836
    ## 3                    141.8670914                     14.2388359
    ## 4                     -0.6784551                     58.5691369
    ## 5                     61.7617901                     63.0890513
    ## 6                      9.9090048                      0.4560032
    ##   r_submission_id[318,Intercept] r_submission_id[319,Intercept]
    ## 1                      88.331500                     17.0166306
    ## 2                      68.789837                      8.5238386
    ## 3                     100.908326                     72.2339777
    ## 4                      12.168680                     14.7993512
    ## 5                      44.397085                     31.0899276
    ## 6                      -2.449947                     -0.4234439
    ##   r_submission_id[320,Intercept] r_submission_id[321,Intercept]
    ## 1                       41.71327                     -31.689165
    ## 2                       15.49143                     -10.827176
    ## 3                       18.34090                       1.438730
    ## 4                       10.69993                     -62.069257
    ## 5                       69.96469                     -12.726799
    ## 6                       25.76963                      -5.164859
    ##   r_submission_id[322,Intercept] r_submission_id[323,Intercept]
    ## 1                      -0.421116                      26.457198
    ## 2                      14.093660                       7.472644
    ## 3                      33.459074                      62.284577
    ## 4                     100.859021                      23.519186
    ## 5                     116.640971                      37.831883
    ## 6                      -7.531770                      20.677344
    ##   r_submission_id[324,Intercept] r_submission_id[325,Intercept]
    ## 1                      86.259081                       9.751129
    ## 2                      64.626688                       5.333795
    ## 3                     116.437592                      58.617383
    ## 4                      53.342805                      22.427953
    ## 5                      75.074667                      24.998483
    ## 6                       3.297421                      22.794361
    ##   r_submission_id[326,Intercept] r_submission_id[327,Intercept]
    ## 1                    -115.545134                      31.411699
    ## 2                     -71.879963                      20.282224
    ## 3                     -62.417049                     -18.047103
    ## 4                       2.613727                      31.419531
    ## 5                     -18.536626                      37.503881
    ## 6                     -12.188360                       8.635053
    ##   r_submission_id[328,Intercept] r_submission_id[329,Intercept]
    ## 1                      29.243915                       12.04349
    ## 2                      17.501287                       26.52044
    ## 3                       6.844834                       39.51117
    ## 4                      46.208995                       55.13987
    ## 5                      40.307968                       96.60994
    ## 6                      16.922478                       17.36381
    ##   r_submission_id[330,Intercept] r_submission_id[331,Intercept]
    ## 1                     -15.645255                      -26.08760
    ## 2                     -15.830887                      -27.02400
    ## 3                       2.103699                      -40.42322
    ## 4                     -16.173087                      -21.55210
    ## 5                     -40.460147                      -63.79216
    ## 6                     -11.593933                      -16.08367
    ##   r_submission_id[332,Intercept] r_submission_id[333,Intercept]
    ## 1                     -11.095719                      34.642870
    ## 2                      -6.318679                       9.814541
    ## 3                     -41.700116                      38.284896
    ## 4                       5.282131                     110.259392
    ## 5                     -40.629015                      75.278082
    ## 6                     -11.874003                      29.128622
    ##   r_submission_id[334,Intercept] r_submission_id[335,Intercept]
    ## 1                      79.375448                      -25.14476
    ## 2                      25.595265                      -43.05934
    ## 3                      48.163140                      -37.54590
    ## 4                       7.188251                      -45.18488
    ## 5                      15.017010                      -14.38926
    ## 6                      24.675895                      -22.88697
    ##   r_submission_id[336,Intercept] r_submission_id[337,Intercept]
    ## 1                       2.756873                    -28.1676456
    ## 2                      17.038627                    -11.2245290
    ## 3                       0.227965                      0.7460559
    ## 4                      24.826105                    -51.7521572
    ## 5                      27.469932                    -26.1227181
    ## 6                      17.174533                     -6.7765638
    ##   r_submission_id[338,Intercept] r_submission_id[339,Intercept]
    ## 1                     -110.23111                     -16.803692
    ## 2                      -94.04740                     -23.457269
    ## 3                     -142.86415                     -33.388566
    ## 4                      -48.12685                     -56.110214
    ## 5                      -22.10520                     -78.523017
    ## 6                      -13.10799                       8.122027
    ##   r_submission_id[340,Intercept] r_submission_id[341,Intercept]
    ## 1                      -15.19683                       2.597167
    ## 2                      -34.48723                      -5.920517
    ## 3                      -86.79651                      -2.346760
    ## 4                      -39.75545                     -64.967432
    ## 5                      -48.98966                     -75.163900
    ## 6                      -36.06357                     -27.075557
    ##   r_submission_id[342,Intercept] r_submission_id[343,Intercept]
    ## 1                      33.793783                      -23.65861
    ## 2                      -7.251706                      -23.65527
    ## 3                      10.920656                      -62.73897
    ## 4                      52.621322                      -22.73039
    ## 5                      38.225014                      -27.62297
    ## 6                       9.867985                      -25.54262
    ##   r_submission_id[344,Intercept]      lp__
    ## 1                       28.00785 -142.4495
    ## 2                       33.37888 -156.2325
    ## 3                       31.69518 -161.6696
    ## 4                       18.02076 -155.3405
    ## 5                       29.06995 -147.0304
    ## 6                       24.95472 -148.8174

``` r
# test hypothesis: is the sub response proportion credibly greater in the predicative condition?
mean(pos_samples$b_syntax1 > 0)
```

    ## [1] 0.33725

## Proportion of sub reponses by item category

``` r
#d_main_responseCat %>% group_by(item, syntax, response_label) %>% summarise(nr_sub_resp# sum(resp_cat)) %>% ungroup() %>% ggplot(., aes(x = nr_sub_resp)) + geom_bar() + facet_grid(syntax~item)

# porportions of sub reponses instead of counts
d_main_responseCat %>% group_by(item, syntax, response_label) %>% tidyboot_mean(column = resp_cat) %>% ungroup() %>% ggplot(., aes(x = item, y = mean,  ymin = ci_lower, ymax = ci_upper)) + geom_col(position = position_dodge(0.8)) + geom_linerange(position = position_dodge(0.8)) + facet_wrap(~syntax)
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

    ## Warning: Column `syntax` has different attributes on LHS and RHS of join

![](prereg-comp-class-prod_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Proportion of sub reponses by single item (picture)

``` r
#d_main_responseCat %>% group_by(item, syntax, response_label, size) %>% summarise(nr_sub_resp = sum(resp_cat)) %>% ungroup() %>% ggplot(., aes(x = nr_sub_resp)) + geom_bar() + facet_grid(syntax + size ~item)

# proportion of sub reponses by picture
d_main_responseCat %>% group_by(picture, syntax, response_label) %>% tidyboot_mean(column = resp_cat) %>% ungroup() %>% ggplot(., aes(x = picture, y = mean,  ymin = ci_lower, ymax = ci_upper)) + geom_col(position = position_dodge(0.8)) + geom_linerange(position = position_dodge(0.8)) + facet_wrap(~syntax) + coord_flip()
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

    ## Warning: Column `syntax` has different attributes on LHS and RHS of join

![](prereg-comp-class-prod_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
