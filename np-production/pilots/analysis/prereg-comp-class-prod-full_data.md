Comparison Class Production Full Data Analysis
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
d_main <- d_filt %>% filter((trial_name =="main1") | (trial_name== "main2")  ) %>% 
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
                            labels= c("That's a big X", "That X is big"))) %>% ggplot(aes(x=response_cat, fill=response_cat)) +geom_bar(position=position_dodge()) + facet_wrap(~syntax)
```

![](prereg-comp-class-prod-full_data_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

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

![](prereg-comp-class-prod-full_data_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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
    ## Chain 1: Gradient evaluation took 0.001 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
    ## Chain 1:  Elapsed Time: 12.012 seconds (Warm-up)
    ## Chain 1:                8.06 seconds (Sampling)
    ## Chain 1:                20.072 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '1b1423e832ba07da4efffd5a83eb433e' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.001 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
    ## Chain 2:  Elapsed Time: 13.144 seconds (Warm-up)
    ## Chain 2:                9.475 seconds (Sampling)
    ## Chain 2:                22.619 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '1b1423e832ba07da4efffd5a83eb433e' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.001 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
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
    ## Chain 3:  Elapsed Time: 12.66 seconds (Warm-up)
    ## Chain 3:                7.83 seconds (Sampling)
    ## Chain 3:                20.49 seconds (Total)
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
    ## Chain 4:  Elapsed Time: 11.162 seconds (Warm-up)
    ## Chain 4:                8.913 seconds (Sampling)
    ## Chain 4:                20.075 seconds (Total)
    ## Chain 4:

``` r
summary(lm.bayes)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: resp_cat ~ syntax + (1 | submission_id) + (1 + syntax | picture) 
    ##    Data: d_main_responseCat (Number of observations: 358) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~picture (Number of levels: 20) 
    ##                        Estimate Est.Error l-95% CI u-95% CI Eff.Sample
    ## sd(Intercept)              2.02      0.68     0.95     3.56       1205
    ## sd(syntax1)                0.92      0.59     0.05     2.31        965
    ## cor(Intercept,syntax1)    -0.56      0.41    -0.99     0.58       1798
    ##                        Rhat
    ## sd(Intercept)          1.00
    ## sd(syntax1)            1.00
    ## cor(Intercept,syntax1) 1.00
    ## 
    ## ~submission_id (Number of levels: 60) 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sd(Intercept)     7.87      2.09     4.76    12.62       1137 1.00
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## Intercept     2.27      1.34    -0.15     5.21        633 1.00
    ## syntax1      -1.23      1.29    -4.07     1.11        630 1.01
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
#extract posterior samples
pos_samples <- posterior_samples(lm.bayes)
#head(pos_samples)

# test hypothesis: is the sub response proportion credibly greater in the predicative condition?
mean(pos_samples$b_syntax1 > 0)
```

    ## [1] 0.1545

## Proportion of sub reponses by item category

``` r
#d_main_responseCat %>% group_by(item, syntax, response_label) %>% summarise(nr_sub_resp# sum(resp_cat)) %>% ungroup() %>% ggplot(., aes(x = nr_sub_resp)) + geom_bar() + facet_grid(syntax~item)

# porportions of sub reponses instead of counts
d_main_responseCat %>% group_by(item, syntax, response_label) %>% tidyboot_mean(column = resp_cat) %>% ungroup() %>% ggplot(., aes(x = item, y = mean,  ymin = ci_lower, ymax = ci_upper)) + geom_col(position = position_dodge(0.8)) + geom_linerange(position = position_dodge(0.8)) + facet_wrap(~syntax)
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

    ## Warning: Column `syntax` has different attributes on LHS and RHS of join

![](prereg-comp-class-prod-full_data_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Proportion of sub reponses by single item (picture)

``` r
#d_main_responseCat %>% group_by(item, syntax, response_label, size) %>% summarise(nr_sub_resp = sum(resp_cat)) %>% ungroup() %>% ggplot(., aes(x = nr_sub_resp)) + geom_bar() + facet_grid(syntax + size ~item)

# proportion of sub reponses by picture
d_main_responseCat %>% group_by(picture, syntax, response_label) %>% tidyboot_mean(column = resp_cat) %>% ungroup() %>% ggplot(., aes(x = reorder(picture, mean), y = mean, fill=syntax, ymin = ci_lower, ymax = ci_upper)) + geom_col(position = position_dodge(0.8)) + geom_linerange(position = position_dodge(0.8)) + coord_flip() 
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

    ## Warning: Column `syntax` has different attributes on LHS and RHS of join

![](prereg-comp-class-prod-full_data_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# + facet_wrap(~syntax)
```

## Number of sub responses per participant

``` r
d_main_responseCat %>%
  group_by(submission_id, syntax, response_label) %>%
  summarize(n_sub_responses = sum(resp_cat)) %>%
  ungroup() %>%
  ggplot(., aes( x = n_sub_responses))+
  geom_bar(position=position_dodge())+
  facet_wrap(~syntax) + ggtitle("Number of subordinate responses uttered per participant in the 6 trials")
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

![](prereg-comp-class-prod-full_data_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
## Analysis by target size

d_main_responseCat %>% group_by(size, response_label, syntax) %>% tidyboot_mean(column=resp_cat) %>% ungroup() %>% ggplot(., aes(x=size, fill=syntax, y = mean, ymin = ci_lower, ymax = ci_upper)) + geom_col(position = position_dodge(0.8)) + geom_linerange(position = position_dodge(0.8))
```

    ## Warning: Grouping rowwise data frame strips rowwise nature

    ## Warning: Column `syntax` has different attributes on LHS and RHS of join

![](prereg-comp-class-prod-full_data_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
