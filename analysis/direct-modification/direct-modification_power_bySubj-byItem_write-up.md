Direct Modification Power Analysis Write-Up
================
Polina Tsvilodub
9/01/2020

This write-up summarizes results of the Bayesian power analysis for the
Direct Modification refpred experiment including an iteration both over
different numbers of subjects and over different numbers of experimental
items. For more details in the experiment and the power analysis
procedure, see
<https://github.com/polina-tsvilodub/refpred/blob/master/analysis/direct-modification/direct-modification_power_write-up.md>.

Crucially, we are interested in a credible *effect of syntax in the
critical condition*. We decided to build the power analysis assuming a
maximal model including a main effects of syntax (subject vs
predicate-N), trial type (critical vs. filler) and their *interaction*
since this model is most appropriate given our experimental design, but
we’ll remain agnostic about the direction of the interaction estimate.

The power analysis proceeds as follows:

1.  The desired model to be used in final analyses is fit on pilot data
    (n = 180 subjects):

response-category = syntax \* trial-type + (1 + syntax \* trial-type ||
subjectID) + (1 + syntax \* trial-type || item)

``` r
summary(pilot_model)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: response_num ~ syntax_dev * trial_dev + (1 + syntax_dev * trial_dev || workerid) + (1 + syntax_dev * trial_dev || target) 
    ##    Data: pilot_data (Number of observations: 918) 
    ## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
    ##          total post-warmup samples = 8000
    ## 
    ## Group-Level Effects: 
    ## ~target (Number of levels: 10) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                  0.26      0.20     0.01     0.75 1.00     3942
    ## sd(syntax_dev1)                0.21      0.17     0.01     0.63 1.00     3884
    ## sd(trial_dev1)                 0.47      0.26     0.05     1.06 1.00     2890
    ## sd(syntax_dev1:trial_dev1)     0.25      0.20     0.01     0.73 1.00     3717
    ##                            Tail_ESS
    ## sd(Intercept)                  5024
    ## sd(syntax_dev1)                4855
    ## sd(trial_dev1)                 2692
    ## sd(syntax_dev1:trial_dev1)     4967
    ## 
    ## ~workerid (Number of levels: 180) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                  3.10      0.42     2.37     3.98 1.00     2993
    ## sd(syntax_dev1)                0.41      0.25     0.02     0.94 1.00     1981
    ## sd(trial_dev1)                 0.74      0.31     0.11     1.35 1.00     1235
    ## sd(syntax_dev1:trial_dev1)     0.28      0.20     0.01     0.75 1.00     3132
    ##                            Tail_ESS
    ## sd(Intercept)                  4713
    ## sd(syntax_dev1)                3421
    ## sd(trial_dev1)                 1686
    ## sd(syntax_dev1:trial_dev1)     4516
    ## 
    ## Population-Level Effects: 
    ##                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                  2.91      0.42     2.14     3.81 1.00     4079
    ## syntax_dev1                0.66      0.19     0.29     1.06 1.00     9093
    ## trial_dev1                -0.12      0.27    -0.68     0.42 1.00     7818
    ## syntax_dev1:trial_dev1     0.08      0.19    -0.29     0.45 1.00     9760
    ##                        Tail_ESS
    ## Intercept                  5258
    ## syntax_dev1                6206
    ## trial_dev1                 5343
    ## syntax_dev1:trial_dev1     5870
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

2.  Then, posterior predictive samples are drawn from this fitted model,
    simulating a given number of subjects (increased iteratively from
    150 to 600) and a given number of experimental items (iterating over
    12, 16 or 20). New potential by-subject and by-item effects for the
    respective number are sampled from a gaussian distribution specified
    by the estimated group-level standard deviations and correlations of
    pilot data.
3.  The model is re-computed on these posterior samples and the
    parameter of interest (i.e., the syntax coefficient in the critical
    condition) is extracted. The models were fit using 4 chains and 4000
    iterations each.
4.  This process is repeated *200 times* for each simulated
    subjects-number and item-number.
5.  The power for the given number of participants/items is calculated
    as the proportion of critical coefficients that were estimated in
    the predicted direction (i.e., the credible interval excludes 0)
    over all the simulations.

The power analysis script can be found under:
<https://github.com/polina-tsvilodub/refpred/blob/master/analysis/direct-modification/power_analysis.R>

Here is the credible interval over the estimate for the effect of syntax
in the critical condition over the progressing simulations, faceted by
number of simulated subjects:

![](direct-modification_power_bySubj-byItem_write-up_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The results of the simulations reveal the following power for
subject-numbers between 150 and 600, increasing by 50 subjects and over
12, 16, 20 items can be found under
<https://github.com/polina-tsvilodub/refpred/blob/master/analysis/direct-modification/results/direct_mod_power_analysis_bySubj-byItem_4000iter_200sim_summary.csv>.

The power plotted as a function of number of simulations reveals
oscillations of the power for less than 200 simulations:
![](direct-modification_power_bySubj-byItem_write-up_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Overall, we see that simulations of around 250 subjects already achieve
a power of 0.8 and 300 subjects around 0.85, and the number of items
does not seem to have a large effect for any number of subjects but 350.

#### Attrition rates in the pilots

Over the course of all 6 pilots, we recruited 207 participants and
excluded 27 (13%), mostly due to failing the warm-up trials (i.e.,
taking more than 4 attempts to provide correct picture labels upon
correction on labeling warm-up trials; 17 participants, 8%) or reporting
a native language other than English. The rate of invalid responses
post-exclusion (i.e., unclassifiable free-production responses) is
around 1-3%. The participants were paid $1.00/participant.
