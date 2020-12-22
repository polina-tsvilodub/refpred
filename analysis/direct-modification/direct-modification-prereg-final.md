Direct Modification Results - Prereg Final
================
Polina Tsvilodub
12/13/2020

This write-up presents results of the final direct-modification
experiment for the refpred project. In this experiment we manipulate the
syntactic position of the noun directly modified by the adjective (“big
Great Dane” appearing in the subject or in the predicate) in order to
disentangle effects of reasoning about informational goals on comparison
class inference from effects of syntactic modification.

Our [Bayesian power
analysis](https://github.com/polina-tsvilodub/refpred/blob/master/analysis/direct-modification/direct-modification_power_bySubj-byItem_write-up.md)
based on pilot data revealed that for a power above 0.8 we need at least
300 subjects, so for this final [preregistered](https://osf.io/vt3f7)
study we recruited N=330 on Prolific, aiming for N=300 passing the
exclusion criteria.

# Experiment design

The experiment has a 2x2 within-subjects design, manipulating the
syntactic position of subordinate nouns (subject vs predicate N) and the
trial-type (critical vs filer, where fillers are trials from our CogSci
Experiment 3), all appearing in basic-level context.

Participants see a context picture and read a sentence about a referent
which they have to paraphrase, completing a total of eight main trials,
presented in two blocks of four main trials each. The eight main trials
consist of four critical and four filler trials, where each trial is a
condition resulting from a unique combination of the noun position
condition (subject N vs. predicate N) crossed with the size of the
referent within its basic-level category (e.g., large vs. small
subordinate dog category). Ten contexts created from six different
basic-level categories are used: dogs, flowers, birds, fish, buildings
and trees. For each basic-level context, there are two possible targets
representing a large-subordinate and a small-subordinate category,
respectively. Four contexts are sampled for each participant.
Information about the items can be found
[here](https://docs.google.com/document/d/1yxF9ACALa6MQB70nYydGStvLiY0JjO8mmkASI049lT4/edit?usp=sharing).

# Analysis

## Data Preprocessing

We collected data from N = 323 participants. 4 participants are excluded
for not reporting their native language, or being non-native English
speakers. 3 subjects failed the comparison class paraphrase warm-up
trial (i.e., took more than 4 attempts to enter the right response upon
correction); 14 subjects failed the labeling warm-up trials (also taking
more than 4 attempts upon correction). Overall, 15 subjects were
excluded for failing the warm-ups. This leaves N = 304. Last 4 received
submissions are excluded for an N = 300.

``` r
# exclude participants who report glitches
data %>% select(submission_id, comments, problems) %>% distinct() %>% View()
d_modRef_woGlitches <- data 

# exclude non-native English speakers
d_modRef_woGlitches %>% distinct(languages) %>% View()

# 323 participants received
d_modRef_Native <- d_modRef_woGlitches %>% 
  filter(grepl("en", languages, ignore.case = T)) 
# excluded 4 as non-native English speakers

# cleaning warm-up trials
# comparison class paraphrase trial

# excludes 3
d_failed_cc_warmup <- d_modRef_Native %>% 
  filter( trial_name == "comp_class_warmup") %>%
  group_by(submission_id) %>% count() %>%
  filter( n > 4 )
# excludes 14
d_failed_label_warmup <- d_modRef_Native %>%
  filter( (trial_name == "warmup1") | (trial_name == "warmup2")) %>%
  group_by(submission_id) %>%
  filter(attempts > 4)
d_label_warmup_more1 <- d_modRef_Native %>%
  filter( (trial_name == "warmup1") | (trial_name == "warmup2")) %>%
  group_by(submission_id) %>%
  filter(attempts > 1) %>% ungroup() %>% 
  select(submission_id, picture1, response1, picture2, response2, attempts)

# 304 left 
d_modRef_filter <- anti_join(d_modRef_Native, d_failed_cc_warmup, by = c("submission_id"))
d_modRef_filter <- anti_join(d_modRef_filter, d_failed_label_warmup, by = c("submission_id"))

# exclude last 4 submissions to get 300 subjects
d_modRef_filter300 <- d_modRef_filter %>% filter(!(submission_id %in% c(2644, 2645, 2646, 2647)))
```

Check the balancing of conditions:

``` r
d_modRef_filter300 %>% count(trial_type, syntax, target_size)
```

    ## # A tibble: 9 x 4
    ##   trial_type syntax target_size     n
    ##   <chr>      <chr>  <chr>       <int>
    ## 1 critical   pred   big           300
    ## 2 critical   pred   small         300
    ## 3 critical   subj   big           300
    ## 4 critical   subj   small         300
    ## 5 filler     pred   big           300
    ## 6 filler     pred   small         300
    ## 7 filler     subj   big           300
    ## 8 filler     subj   small         300
    ## 9 <NA>       <NA>   <NA>         1785

### Response Classification

#### Minimal exclusions

First, invalid responses are excluded. This is a minimal pre-processing
step: Excluded responses consist of disjunctions/conjunctions, massive
misspellings, compare the referent to a size, or to an invalid
subordinate category (reference failure). This excludes 21 responses
(0.9%).

``` r
d_modRef_main <- d_modRef_filter300 %>% filter((trial_name == "custom_main_text1") |
                                (trial_name == "custom_main_text2")) %>%
  select(submission_id, trial_number, context_picture, response, target_size, adj, syntax, target, item, adj_cond, trial_type, ref_np )

d_modRef_main %>% distinct(response) %>% View()

# exclude 21 answers with minimal exclusion criteria
d_modRef_valid <- d_modRef_main %>% 
  subset(., !(tolower(response) %in% c("deandal", "compared to the other fish", "compared to the fish", "the size of the flower", "pigeon or other common birds", "a rose", "pigeon", "trees or himself", "dogs or the bow", "landmark or trees", "bow/gift", "sunflowers or bigger flowers", "child", "himself and the other trees", "the size of other birds", "his own size", "human", "flowers and landmark", "a person", "his award")))
```

Then, the minimally cleaned responses are classified as matching the
critical noun (= subordinate) vs. non-matching (i.e., basic-level,
matchin N2, superordinate).

``` r
# classify 2379 responses 
d_modRef_main_responseCat <- d_modRef_valid %>% 
  mutate(response_cat = ifelse(
    tolower(response) %in% 
      c("building", "buildings", "flower", "flowers", "potted plants", "plants", "fish",
        "fishes", "dog", "dogs", "tree", "trees", "bird", "birds",  
        "flowers in the distance", "other flowers", "the other flowers", "the other dogs",
        "big dogs", "birds in the distance", "flowers that are already sold", "flowers.",
        "the other buildings around it", "the dogs around it", "the buildings around it",
        "flowers around it", "dogs around it", "the other flowers around it", "other plants",
        "other trees", "other rescue dogs\nother rescue dogs", "other floers", 
        "fish in the tank", "flowers with red bows", "dogs with red bows", 
        "dogs with leashes", "birda", "breeds", "pets", "trees\ntrees", "dogs that are gifts",
        "rescued fish","dogs.", "fish.", "dogs with medals", "smaller dogs", "dogs with bows", 
        "building with landmarks", "guide dogs", "landmark buildings", "other birds", 
        "flowers along the path", "flowers in the pots", "gift flowers", "dogd", 
        "the other prize winner dogs", "the other smaller birds", "birds you have seen",
        "smaller trees like the bonsai", "dogs that you have seen so far", "trees in general",
        "the other plants they are seeing", "the other trees you have seen", "fish\nfish",
        "the other flowers at the shop", "trees other than the other red wood", 
        "the other trees", "training dogs", "contestants", "gods", "small dogs",
        "landmark flowers", "other buildings", "dogs in the contest", "the other service dogs",
        "the other rescue birds", "the smaller dogs", "the smaller flowers", 
        "birds in the group", "fowers", "other landmarked flowers.", "other fish",
        "flowers at the garden store","flowers in the group", "present", "winner", 
        "rescued dogs", 
        
        "landmark", "landmarks", "service-animal", "service-animals", "service animals",
        "rescue", "rescues", "prize-winner", "prize-winners", "prize winner", "prizewinners",
        "prize winners", "dog gifts", "land mark trees", "smaller service animals",
        "prize-winner dogs", "rescue fish", "prize winning dogs", "prize-winning dogs",
        "other prize-winner dogs", "otherprize winning dog", "landmark trees", 
        "flower gifts", "the other prize winner", "the other service animal", "show winner",
        "prize dogs", "rescue birds", "service dogs", "gift", "gifts", "gifts.", "prize dog",
        "big trees with a landmark", "prize-winners."), 
    "nonmatch", "match"
  ),
  response_num = ifelse(response_cat == "nonmatch", 1, 0)
  )
```

We also consider a more fine-grained 3-way response classification:
basic-level responses (also containing superordinate responses), N2
responses (e.g., “prize-winners”), subordinate responses.

``` r
# detailed analysis of non-matching responses, distinguishing between basic, N2 and 
# subordinate comparison classes
d_modRef_main_responseCat_3way <- d_modRef_main_responseCat %>%
  mutate(
    response_cat = ifelse(
      tolower(response) %in% 
        c("building", "buildings", "flower", "flowers", "potted plants", "plants", "fish",
        "fishes", "dog", "dogs", "tree", "trees", "bird", "birds", "other dogs", 
        "flowers in the distance", "other flowers", "the other flowers", "the other dogs",
        "big dogs", "birds in the distance", "flowers that are already sold", "flowers.",
        "the other buildings around it", "the dogs around it", "the buildings around it",
        "flowers around it", "dogs around it", "the other flowers around it", "other plants",
        "other trees", "other rescue dogs\nother rescue dogs", "other floers", 
        "fish in the tank", "flowers with red bows", "dogs with red bows", 
        "dogs with leashes", "birda", "breeds", "pets", "trees\ntrees", "dogs that are gifts",
        "rescued fish","dogs.", "fish.", "dogs with medals", "smaller dogs", "dogs with bows", 
        "building with landmarks", "guide dogs", "landmark buildings", "other birds", 
        "flowers along the path", "flowers in the pots", "gift flowers", "dogd", 
        "the other prize winner dogs", "the other smaller birds", "birds you have seen",
        "smaller trees like the bonsai", "dogs that you have seen so far", "trees in general",
        "the other plants they are seeing", "the other trees you have seen", "fish\nfish",
        "the other flowers at the shop", "trees other than the other red wood", 
        "the other trees", "training dogs",  "gods", "small dogs",
        "landmark flowers", "other buildings", "dogs in the contest", "the other service dogs",
        "the other rescue birds", "the smaller dogs", "the smaller flowers", 
        "birds in the group", "fowers", "other landmarked flowers.", "other fish",
        "flowers at the garden store","flowers in the group", "prize-winner dogs",
        "rescued dogs", "rescue fish", "prize winning dogs", "prize-winning dogs",
        "other prize-winner dogs", "otherprize winning dog", "landmark trees",
        "prize dogs", "rescue birds", "service dogs", "prize dog", "big trees with a landmark"
        ), "basic",
      ifelse( tolower(response) %in% c("landmark", "landmarks", "service-animal", "service-animals",
                                       "service animals", "rescue", "rescues", "prize-winner",
                                       "prize-winners", "prize winner", "prizewinners",
        "prize winners", "dog gifts", "land mark trees", "smaller service animals",
        "flower gifts", "the other prize winner", "the other service animal", "show winner",
         "gift", "gifts", "gifts.", "prize-winners.", "present", "winner", "contestants"
                                       ), "N2", "subordinate")
    )
  )
```

##### Plots

Here the proportion of non-matching responses by-syntax and by-trial
type is plotted. Error bars represent bootstrapped 95%-CIs.

We see a small effect in the critical condition, and we see a pronounced
effect in the filler conditions (replicating crucial results from CogSci
Exp. 3).

    ## Warning: `as_data_frame()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

![](direct-modification-prereg-final_files/figure-gfm/plot-1.png)<!-- -->

Here, counts of the three response categories (basic, N2, subordinate)
in the critical direct-modification trials are plotted by-syntax.
![](direct-modification-prereg-final_files/figure-gfm/plot2-1.png)<!-- -->

#### More possible exclusions

Yet since the responses are quite noisy, we might consider excluding
further responses. The paraphrase template is "It is {big, small}
relative to other \_\_\_“. Therefore, responses starting with”to the…"
or “to other…” etc are technically ungrammatical. So the following
preprocessing excludes the aforementioned responses additionally to the
exclusions made before. This excludes 77 responses in total (3.2%).

If we are being really strict, responses starting with “the..” could
also be excluded (tbd).

``` r
# apply more strict exclusion criteria

d_modRef_valid_strict <- d_modRef_main %>%
  subset(., !(tolower(response) %in% c(
"deandal", "compared to the other fish", "compared to the fish", "the size of the flower",
"pigeon or other common birds", "a rose", "pigeon", "trees or himself", "dogs or the bow",
"landmark or trees", "bow/gift", "sunflowers or bigger flowers", "child", 
"himself and the other trees", "the size of other birds", "his own size", "human", 
"flowers and landmark", "a person", "his award", "other flowers", "the other flowers", 
"the other dogs", "the other buildings around it", "the other flowers around it", "other plants",
"other trees", "other rescue dogs\nother rescue dogs", "other floers", "other birds",
"the other prize winner dogs", "the other smaller birds", "the other plants they are seeing",
"the other trees you have seen", "the other flowers at the shop", "the other trees",
"other buildings", "the other service dogs", "the other rescue birds", 
"other landmarked flowers.", "other fish", "other prize-winner dogs", "otherprize winning dog",
"the other prize winner", "the other service animal",
# also some subordinate responses start with "other"
"other doberman", "other bonsai trees", "other eagles", "other hummingbirds", 
"other sunflowers", "other clownfish rescues", "the other dandelion", "the other chihuahua", 
"the other doberman", "other dandelions"
  )))
```

Furthermore, there are some responses where the classification might not
be clear.

For instance, some responses refer to perceptual categories. as e.g.,
“birds you have seen”. Since the perceptual context presents the
basic-level category, so far such responses are classified as
basic-level. There are modified basic-level responses (“smaller dogs”,
“the other smaller birds”, “smaller trees like the bonsai”, “small
dogs”, “the smaller dogs”, “the smaller flowers”, “smaller service
animals”,) or responses with a PP which are semantically very similar to
the N2 (“flowers that are already sold”,“flowers with red bows”, “dogs
with red bows”, “dogs with leashes”, “dogs that are gifts”, “dogs with
medals”, “dogs with bows”, “building with landmarks”, “big trees with a
landmark”).

There are also compound responses just difficult to classify, some of
them containing both a subordinate noun and a basic-level noun or N2
(“trees other than the other red wood”, “to the doberman
service-animal”, “prize winning pugs”, “other bonsai trees”, “tuna
fish”, “bonsai trees”, “the prize-winner pug”, “other clownfish
rescues”, “hummingbirds that have been rescued”, “great dane dogs”,
“redwood trees”). So far, all these responses containing a subordinate
N are classified as subordinate (= matching).

These responses are classified in non-matching vs matching:

``` r
d_modRef_main_responseCat_strict <- d_modRef_valid_strict %>% 
  mutate(response_cat = ifelse(
    tolower(response) %in% 
      c("building", "buildings", "flower", "flowers", "potted plants", "plants", "fish",
        "fishes", "dog", "dogs", "tree", "trees", "bird", "birds",  
        "flowers in the distance", 
        "big dogs", "birds in the distance", "flowers that are already sold", "flowers.",
        "the dogs around it", "the buildings around it",
        "flowers around it", "dogs around it",  
        "fish in the tank", "flowers with red bows", "dogs with red bows", 
        "dogs with leashes", "birda", "breeds", "pets", "trees\ntrees", "dogs that are gifts",
        "rescued fish","dogs.", "fish.", "dogs with medals", "smaller dogs", "dogs with bows", 
        "building with landmarks", "guide dogs", "landmark buildings", 
        "flowers along the path", "flowers in the pots", "gift flowers", "dogd", 
        "birds you have seen",
        "smaller trees like the bonsai", "dogs that you have seen so far", "trees in general",
        "fish\nfish", "trees other than the other red wood", 
        "training dogs", "contestants", "gods", "small dogs",
        "landmark flowers", "dogs in the contest", "the smaller dogs", "the smaller flowers", 
        "birds in the group", "fowers", "flowers at the garden store","flowers in the group",
        "present", "winner", "rescued dogs", 
        
        "landmark", "landmarks", "service-animal", "service-animals", "service animals",
        "rescue", "rescues", "prize-winner", "prize-winners", "prize winner", "prizewinners",
        "prize winners", "dog gifts", "land mark trees", "smaller service animals",
        "prize-winner dogs", "rescue fish", "prize winning dogs", "prize-winning dogs",
        "landmark trees", "flower gifts", "show winner",
        "prize dogs", "rescue birds", "service dogs", "gift", "gifts", "gifts.", "prize dog",
        "big trees with a landmark", "prize-winners."), 
    "nonmatch", "match"
  ),
  response_num = ifelse(response_cat == "nonmatch", 1, 0)
  )
```

3-way classification of strictly preproccesed responses:

``` r
d_modRef_main_responseCat_strict_3way <- d_modRef_main_responseCat_strict %>%
  mutate(
    response_cat = ifelse(
      tolower(response) %in% 
        c("building", "buildings", "flower", "flowers", "potted plants", "plants", "fish",
        "fishes", "dog", "dogs", "tree", "trees", "bird", "birds", "other dogs", 
        "flowers in the distance", "other flowers", "the other flowers", "the other dogs",
        "big dogs", "birds in the distance", "flowers that are already sold", "flowers.",
        "the other buildings around it", "the dogs around it", "the buildings around it",
        "flowers around it", "dogs around it",  
        "fish in the tank", "flowers with red bows", "dogs with red bows", 
        "dogs with leashes", "birda", "breeds", "pets", "trees\ntrees", "dogs that are gifts",
        "rescued fish","dogs.", "fish.", "dogs with medals", "smaller dogs", "dogs with bows", 
        "building with landmarks", "guide dogs", "landmark buildings", 
        "flowers along the path", "flowers in the pots", "gift flowers", "dogd", 
        "birds you have seen",
        "smaller trees like the bonsai", "dogs that you have seen so far", "trees in general",
        "fish\nfish", "trees other than the other red wood", 
        "training dogs",  "gods", "small dogs",
        "landmark flowers",  "dogs in the contest", 
         "the smaller dogs", "the smaller flowers", 
        "birds in the group", "fowers", 
        "flowers at the garden store","flowers in the group", "prize-winner dogs",
        "rescued dogs", "rescue fish", "prize winning dogs", "prize-winning dogs",
         "landmark trees",
        "prize dogs", "rescue birds", "service dogs", "prize dog", "big trees with a landmark"
        ), "basic",
      ifelse( tolower(response) %in% c("landmark", "landmarks", "service-animal", "service-animals",
                                       "service animals", "rescue", "rescues", "prize-winner",
                                       "prize-winners", "prize winner", "prizewinners",
        "prize winners", "dog gifts", "land mark trees", "smaller service animals",
        "flower gifts", "show winner",
         "gift", "gifts", "gifts.", "prize-winners.", "present", "winner", "contestants"
                                       ), "N2", "subordinate")
    )
  )
```

##### Plots

The proportion of non-matching responses in the strictly preprocessed
data is plotted by-syntax and by-trial type. No qualitative differences
appear under the more strict preprocessing.

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

![](direct-modification-prereg-final_files/figure-gfm/plot-strict-1.png)<!-- -->

3-way response category counts in the strictly preprocessed data:
![](direct-modification-prereg-final_files/figure-gfm/plot2-strict-1.png)<!-- -->

## Stats

In the following, the dataset where less strict exclusions were applied
is used for analysis. The predictors are deviation-coded.

First, the preregistered Bayesian logistic regression is run. Maximal RE
structure is included:

``` r
model <- brm(
  response_num ~ syntax_dev * trial_type_dev + (1 + syntax_dev * trial_type_dev | submission_id) + 
    (1 + syntax_dev * trial_type_dev | target),
  data = d_modRef_main_responseCat,
  family = "bernoulli",
  cores = 4,
  iter = 3000,
  chains = 4, 
  control = list(adapt_delta = 0.9)
) 
```

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

``` r
summary(model)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: response_num ~ syntax_dev * trial_type_dev + (1 + syntax_dev * trial_type_dev | submission_id) + (1 + syntax_dev * trial_type_dev | target) 
    ##    Data: d_modRef_main_responseCat (Number of observations: 2379) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 300) 
    ##                                                  Estimate Est.Error l-95% CI
    ## sd(Intercept)                                        2.58      0.21     2.20
    ## sd(syntax_dev1)                                      0.29      0.16     0.02
    ## sd(trial_type_dev1)                                  0.31      0.16     0.02
    ## sd(syntax_dev1:trial_type_dev1)                      0.38      0.17     0.04
    ## cor(Intercept,syntax_dev1)                          -0.20      0.37    -0.82
    ## cor(Intercept,trial_type_dev1)                      -0.23      0.35    -0.82
    ## cor(syntax_dev1,trial_type_dev1)                     0.08      0.42    -0.75
    ## cor(Intercept,syntax_dev1:trial_type_dev1)          -0.33      0.31    -0.84
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         0.07      0.40    -0.72
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.36      0.39    -0.56
    ##                                                  u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                                        3.03 1.00     1358
    ## sd(syntax_dev1)                                      0.61 1.00      849
    ## sd(trial_type_dev1)                                  0.62 1.00      879
    ## sd(syntax_dev1:trial_type_dev1)                      0.68 1.00      834
    ## cor(Intercept,syntax_dev1)                           0.57 1.00     3299
    ## cor(Intercept,trial_type_dev1)                       0.52 1.00     3748
    ## cor(syntax_dev1,trial_type_dev1)                     0.81 1.00     1336
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           0.36 1.00     3452
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         0.80 1.00     1220
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.90 1.00      895
    ##                                                  Tail_ESS
    ## sd(Intercept)                                        2391
    ## sd(syntax_dev1)                                      1634
    ## sd(trial_type_dev1)                                  1312
    ## sd(syntax_dev1:trial_type_dev1)                      1077
    ## cor(Intercept,syntax_dev1)                           3213
    ## cor(Intercept,trial_type_dev1)                       3221
    ## cor(syntax_dev1,trial_type_dev1)                     2597
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           3562
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         2430
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     1726
    ## 
    ## ~target (Number of levels: 14) 
    ##                                                  Estimate Est.Error l-95% CI
    ## sd(Intercept)                                        0.72      0.20     0.41
    ## sd(syntax_dev1)                                      0.14      0.10     0.01
    ## sd(trial_type_dev1)                                  0.33      0.16     0.05
    ## sd(syntax_dev1:trial_type_dev1)                      0.11      0.09     0.00
    ## cor(Intercept,syntax_dev1)                           0.15      0.42    -0.71
    ## cor(Intercept,trial_type_dev1)                       0.20      0.35    -0.51
    ## cor(syntax_dev1,trial_type_dev1)                     0.05      0.44    -0.80
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           0.07      0.43    -0.75
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         0.00      0.45    -0.81
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.18      0.44    -0.70
    ##                                                  u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                                        1.18 1.00     2056
    ## sd(syntax_dev1)                                      0.38 1.00     2506
    ## sd(trial_type_dev1)                                  0.68 1.00     1361
    ## sd(syntax_dev1:trial_type_dev1)                      0.33 1.00     2478
    ## cor(Intercept,syntax_dev1)                           0.85 1.00     6303
    ## cor(Intercept,trial_type_dev1)                       0.79 1.00     4251
    ## cor(syntax_dev1,trial_type_dev1)                     0.80 1.00     1733
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           0.83 1.00     6834
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         0.80 1.00     4367
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.89 1.00     4320
    ##                                                  Tail_ESS
    ## sd(Intercept)                                        2853
    ## sd(syntax_dev1)                                      3231
    ## sd(trial_type_dev1)                                  1437
    ## sd(syntax_dev1:trial_type_dev1)                      2337
    ## cor(Intercept,syntax_dev1)                           3951
    ## cor(Intercept,trial_type_dev1)                       3678
    ## cor(syntax_dev1,trial_type_dev1)                     3235
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           4608
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         4655
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     4825
    ## 
    ## Population-Level Effects: 
    ##                             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                       1.89      0.28     1.36     2.45 1.00     1074
    ## syntax_dev1                     0.55      0.11     0.34     0.76 1.00     2973
    ## trial_type_dev1                 0.12      0.14    -0.16     0.40 1.00     3068
    ## syntax_dev1:trial_type_dev1     0.33      0.11     0.11     0.54 1.00     3079
    ##                             Tail_ESS
    ## Intercept                       1854
    ## syntax_dev1                     4137
    ## trial_type_dev1                 3784
    ## syntax_dev1:trial_type_dev1     3646
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

The contrast of interest is the effect of syntax in the critical
condition.

``` r
# get effect of syntax by trial type 
model_draws <- model %>%
  spread_draws(b_Intercept, b_syntax_dev1, b_trial_type_dev1, `b_syntax_dev1:trial_type_dev1`) %>%
  mutate(critical_subj = b_Intercept + b_syntax_dev1 - b_trial_type_dev1 - `b_syntax_dev1:trial_type_dev1`,
         critical_pred = b_Intercept - b_syntax_dev1 - b_trial_type_dev1 + `b_syntax_dev1:trial_type_dev1`,
         syntax_critical = critical_subj - critical_pred, # subject vs predicate 
         filler_subj = b_Intercept + b_syntax_dev1 + b_trial_type_dev1 + `b_syntax_dev1:trial_type_dev1`,
         filler_pred = b_Intercept - b_syntax_dev1 + b_trial_type_dev1 - `b_syntax_dev1:trial_type_dev1`,
         syntax_filler = filler_subj - filler_pred) %>% # subject vs predicate
  select(b_Intercept, b_syntax_dev1, b_trial_type_dev1, `b_syntax_dev1:trial_type_dev1`, critical_subj, critical_pred, syntax_critical, filler_subj, filler_pred, syntax_filler) %>%
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
model_draws  
```

    ## # A tibble: 10 x 4
    ##    key                            mean  lower upper
    ##    <chr>                         <dbl>  <dbl> <dbl>
    ##  1 b_Intercept                   1.89   1.36  2.45 
    ##  2 b_syntax_dev1                 0.551  0.337 0.756
    ##  3 b_syntax_dev1:trial_type_dev1 0.330  0.114 0.541
    ##  4 b_trial_type_dev1             0.117 -0.157 0.397
    ##  5 critical_pred                 1.55   0.951 2.21 
    ##  6 critical_subj                 2.00   1.35  2.72 
    ##  7 filler_pred                   1.13   0.505 1.80 
    ##  8 filler_subj                   2.89   2.15  3.71 
    ##  9 syntax_critical               0.442 -0.116 1.02 
    ## 10 syntax_filler                 1.76   1.16  2.38

Compute the probability of the effect of syntax in the critical
condition being credible:

``` r
posterior_samples %>% filter(key == "syntax_critical") %>% summarize(prob = mean(val > 0))
```

    ## # A tibble: 1 x 1
    ##    prob
    ##   <dbl>
    ## 1 0.941

Exploratory model on data without N2 responses

``` r
d_modRef_main_responseCat_noN2 <- d_modRef_main_responseCat_3way %>% filter (response_cat != "N2")

logistic_model_noN2 <- brm(
  response_num ~ syntax_dev * trial_type_dev + (1 + syntax_dev * trial_type_dev | submission_id) + 
    (1 + syntax_dev * trial_type_dev | target),
  data = d_modRef_main_responseCat_noN2,
  family = "bernoulli",
  cores = 4,
  iter = 3000,
  chains = 4 #, 
#  control = list(adapt_delta = 0.9)
) 
```

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

``` r
summary(logistic_model_noN2)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: response_num ~ syntax_dev * trial_type_dev + (1 + syntax_dev * trial_type_dev | submission_id) + (1 + syntax_dev * trial_type_dev | target) 
    ##    Data: d_modRef_main_responseCat_noN2 (Number of observations: 2194) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 300) 
    ##                                                  Estimate Est.Error l-95% CI
    ## sd(Intercept)                                        2.74      0.24     2.30
    ## sd(syntax_dev1)                                      0.35      0.18     0.03
    ## sd(trial_type_dev1)                                  0.40      0.19     0.04
    ## sd(syntax_dev1:trial_type_dev1)                      0.37      0.18     0.02
    ## cor(Intercept,syntax_dev1)                          -0.30      0.35    -0.85
    ## cor(Intercept,trial_type_dev1)                      -0.36      0.33    -0.85
    ## cor(syntax_dev1,trial_type_dev1)                     0.16      0.40    -0.67
    ## cor(Intercept,syntax_dev1:trial_type_dev1)          -0.15      0.34    -0.76
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         0.04      0.40    -0.74
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.24      0.40    -0.64
    ##                                                  u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                                        3.26 1.00     1025
    ## sd(syntax_dev1)                                      0.70 1.00      886
    ## sd(trial_type_dev1)                                  0.75 1.01      599
    ## sd(syntax_dev1:trial_type_dev1)                      0.71 1.00      609
    ## cor(Intercept,syntax_dev1)                           0.50 1.00     3072
    ## cor(Intercept,trial_type_dev1)                       0.41 1.00     2804
    ## cor(syntax_dev1,trial_type_dev1)                     0.81 1.00     1230
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           0.58 1.00     3524
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         0.78 1.00     1121
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.87 1.00     1237
    ##                                                  Tail_ESS
    ## sd(Intercept)                                        2695
    ## sd(syntax_dev1)                                      1436
    ## sd(trial_type_dev1)                                   883
    ## sd(syntax_dev1:trial_type_dev1)                      1237
    ## cor(Intercept,syntax_dev1)                           3686
    ## cor(Intercept,trial_type_dev1)                       2745
    ## cor(syntax_dev1,trial_type_dev1)                     2276
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           2864
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         2155
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     2078
    ## 
    ## ~target (Number of levels: 14) 
    ##                                                  Estimate Est.Error l-95% CI
    ## sd(Intercept)                                        0.89      0.24     0.52
    ## sd(syntax_dev1)                                      0.16      0.11     0.01
    ## sd(trial_type_dev1)                                  0.37      0.16     0.09
    ## sd(syntax_dev1:trial_type_dev1)                      0.14      0.10     0.01
    ## cor(Intercept,syntax_dev1)                           0.19      0.42    -0.68
    ## cor(Intercept,trial_type_dev1)                      -0.00      0.34    -0.65
    ## cor(syntax_dev1,trial_type_dev1)                    -0.03      0.43    -0.80
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           0.05      0.42    -0.76
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)        -0.05      0.44    -0.82
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.28      0.42    -0.63
    ##                                                  u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                                        1.46 1.00     1919
    ## sd(syntax_dev1)                                      0.43 1.00     2090
    ## sd(trial_type_dev1)                                  0.73 1.00     1738
    ## sd(syntax_dev1:trial_type_dev1)                      0.38 1.00     2263
    ## cor(Intercept,syntax_dev1)                           0.88 1.00     5545
    ## cor(Intercept,trial_type_dev1)                       0.66 1.00     3987
    ## cor(syntax_dev1,trial_type_dev1)                     0.77 1.00     1387
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           0.80 1.00     6407
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         0.77 1.00     3154
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     0.90 1.00     4029
    ##                                                  Tail_ESS
    ## sd(Intercept)                                        3261
    ## sd(syntax_dev1)                                      2536
    ## sd(trial_type_dev1)                                  1917
    ## sd(syntax_dev1:trial_type_dev1)                      2727
    ## cor(Intercept,syntax_dev1)                           4338
    ## cor(Intercept,trial_type_dev1)                       4239
    ## cor(syntax_dev1,trial_type_dev1)                     2455
    ## cor(Intercept,syntax_dev1:trial_type_dev1)           3846
    ## cor(syntax_dev1,syntax_dev1:trial_type_dev1)         3737
    ## cor(trial_type_dev1,syntax_dev1:trial_type_dev1)     4114
    ## 
    ## Population-Level Effects: 
    ##                             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                       1.76      0.33     1.13     2.43 1.00     1006
    ## syntax_dev1                     0.61      0.12     0.38     0.83 1.00     2534
    ## trial_type_dev1                 0.29      0.15    -0.02     0.58 1.00     2443
    ## syntax_dev1:trial_type_dev1     0.32      0.11     0.10     0.54 1.00     2879
    ##                             Tail_ESS
    ## Intercept                       1977
    ## syntax_dev1                     3294
    ## trial_type_dev1                 2884
    ## syntax_dev1:trial_type_dev1     3567
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 10 x 4
    ##    key                            mean   lower upper
    ##    <chr>                         <dbl>   <dbl> <dbl>
    ##  1 b_Intercept                   1.76   1.13   2.43 
    ##  2 b_syntax_dev1                 0.605  0.377  0.833
    ##  3 b_syntax_dev1:trial_type_dev1 0.319  0.100  0.538
    ##  4 b_trial_type_dev1             0.288 -0.0173 0.584
    ##  5 critical_pred                 1.18   0.464  2.00 
    ##  6 critical_subj                 1.75   0.986  2.60 
    ##  7 filler_pred                   1.12   0.451  1.84 
    ##  8 filler_subj                   2.97   2.18   3.84 
    ##  9 syntax_critical               0.573 -0.0384 1.19 
    ## 10 syntax_filler                 1.85   1.21   2.52

Compute the probability of the effect of syntax in the critical
condition being credible given the data without N2 responses:

    ## # A tibble: 1 x 1
    ##    prob
    ##   <dbl>
    ## 1 0.968

### Critical trials only

Fit the preregistered logistic model to critical trials only:

``` r
d_modRef_main_critical <- d_modRef_main_responseCat %>% filter(trial_type == "critical")

logistic_model_critical <- brm(
  response_num ~ syntax_dev + (1 + syntax_dev | submission_id) + 
    (1 + syntax_dev | target),
  data = d_modRef_main_critical,
  family = "bernoulli",
  cores = 4,
  iter = 3000,
  chains = 4 )
```

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

``` r
summary(logistic_model_critical)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: response_num ~ syntax_dev + (1 + syntax_dev | submission_id) + (1 + syntax_dev | target) 
    ##    Data: d_modRef_main_critical (Number of observations: 1192) 
    ## Samples: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
    ##          total post-warmup samples = 6000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 300) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                  2.59      0.28     2.10     3.21 1.00     1340
    ## sd(syntax_dev1)                0.46      0.25     0.03     0.97 1.00      890
    ## cor(Intercept,syntax_dev1)     0.26      0.44    -0.77     0.93 1.00     3536
    ##                            Tail_ESS
    ## sd(Intercept)                  2549
    ## sd(syntax_dev1)                1734
    ## cor(Intercept,syntax_dev1)     2885
    ## 
    ## ~target (Number of levels: 14) 
    ##                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)                  0.63      0.24     0.22     1.17 1.00     1260
    ## sd(syntax_dev1)                0.18      0.14     0.01     0.50 1.00     2594
    ## cor(Intercept,syntax_dev1)     0.27      0.54    -0.90     0.98 1.00     4776
    ##                            Tail_ESS
    ## sd(Intercept)                  1393
    ## sd(syntax_dev1)                3237
    ## cor(Intercept,syntax_dev1)     3577
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       1.68      0.28     1.15     2.25 1.00     2096     3169
    ## syntax_dev1     0.24      0.15    -0.04     0.57 1.00     3197     3627
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Compute the probability of the effect of syntax being greater than 0:

    ## # A tibble: 1 x 1
    ##    prob
    ##   <dbl>
    ## 1 0.958

### Multinomial regression

We might run an exploratory multinomial regression in run on the 3-way
classified responses on the whole dataset, but it takes quite long and
not sure how much sense that makes.

``` r
# make the response category a factor
d_modRef_main_responseCat_3way <- d_modRef_main_responseCat_3way %>%
  mutate(response_cat = as.factor(response_cat))

model_multinomial <- brm(
  response_cat ~ syntax_dev * trial_type_dev + (1 + syntax_dev * trial_type_dev || submission_id) + 
    (1 + syntax_dev * trial_type_dev || target),
  data = d_modRef_main_responseCat_3way,
  family = "categorical",
  cores = 3,
  iter = 2000,
  chains = 3 #, 
  #control = list(adapt_delta = 0.9)
)

summary(model_multinomial)
```

For computational tractability reasons, the exploratory multinomial
regression is run on critical trials only:

``` r
d_modRef_main_3way_critical <- d_modRef_main_responseCat_3way %>%
  mutate(syntax_dev = factor(syntax, levels = c("subj", "pred")),
         trial_type_dev = factor(trial_type, levels = c( "filler", "critical"))) %>%
  filter(trial_type == "critical")

contrasts(d_modRef_main_3way_critical$syntax_dev) <- contr.sum(2)

model_multinomial_critical <- brm(
  response_cat ~ syntax_dev + (1 + syntax_dev || submission_id) + 
    (1 + syntax_dev || target),
  data = d_modRef_main_3way_critical,
  family = "categorical",
  cores = 3,
  iter = 2000,
  chains = 3 
)
```

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

``` r
summary(model_multinomial_critical)
```

    ##  Family: categorical 
    ##   Links: muN2 = logit; musubordinate = logit 
    ## Formula: response_cat ~ syntax_dev + (1 + syntax_dev || submission_id) + (1 + syntax_dev || target) 
    ##    Data: d_modRef_main_3way_critical (Number of observations: 1192) 
    ## Samples: 3 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 3000
    ## 
    ## Group-Level Effects: 
    ## ~submission_id (Number of levels: 300) 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(muN2_Intercept)                2.18      0.30     1.67     2.85 1.00
    ## sd(muN2_syntax_dev1)              0.60      0.30     0.04     1.16 1.00
    ## sd(musubordinate_Intercept)       2.71      0.30     2.16     3.33 1.01
    ## sd(musubordinate_syntax_dev1)     0.50      0.24     0.05     0.95 1.01
    ##                               Bulk_ESS Tail_ESS
    ## sd(muN2_Intercept)                 596     1182
    ## sd(muN2_syntax_dev1)               337      425
    ## sd(musubordinate_Intercept)        697     1122
    ## sd(musubordinate_syntax_dev1)      390      417
    ## 
    ## ~target (Number of levels: 14) 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(muN2_Intercept)                0.81      0.26     0.41     1.41 1.00
    ## sd(muN2_syntax_dev1)              0.38      0.22     0.02     0.87 1.01
    ## sd(musubordinate_Intercept)       0.80      0.26     0.39     1.40 1.01
    ## sd(musubordinate_syntax_dev1)     0.17      0.13     0.01     0.49 1.00
    ##                               Bulk_ESS Tail_ESS
    ## sd(muN2_Intercept)                 781     1660
    ## sd(muN2_syntax_dev1)               736      795
    ## sd(musubordinate_Intercept)        865     1696
    ## sd(musubordinate_syntax_dev1)     1243     1665
    ## 
    ## Population-Level Effects: 
    ##                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## muN2_Intercept               -2.36      0.38    -3.24    -1.69 1.00      681
    ## musubordinate_Intercept      -1.42      0.33    -2.07    -0.76 1.00      773
    ## muN2_syntax_dev1             -0.30      0.18    -0.69     0.03 1.00     1858
    ## musubordinate_syntax_dev1    -0.23      0.12    -0.47     0.00 1.00     2423
    ##                           Tail_ESS
    ## muN2_Intercept                 804
    ## musubordinate_Intercept       1259
    ## muN2_syntax_dev1              1910
    ## musubordinate_syntax_dev1     2108
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Extract contrasts of interest from the multinomial model on critical
condition :

``` r
# extract the probabilities of the different response types in different syntactic positions in the critical condition
# from Kruschke, Doing BDA

# not sure if the contrasts are right yet
posteriors_multinomial_critical <- model_multinomial_critical %>%
  spread_draws(b_muN2_Intercept, b_musubordinate_Intercept, b_muN2_syntax_dev1, b_musubordinate_syntax_dev1) %>%
  mutate(basic_subj = exp(0)/(exp(b_muN2_Intercept + b_muN2_syntax_dev1) + exp(b_musubordinate_Intercept + b_musubordinate_syntax_dev1) + exp(0)),
         basic_pred = exp(0) / (exp(b_muN2_Intercept - b_muN2_syntax_dev1) + exp(b_musubordinate_Intercept - b_musubordinate_syntax_dev1) + exp(0)),
         N2_subj = exp(b_muN2_Intercept + b_muN2_syntax_dev1) / (exp(b_muN2_Intercept + b_muN2_syntax_dev1) + exp(b_musubordinate_Intercept + b_musubordinate_syntax_dev1) + exp(0)),
         N2_pred = exp(b_muN2_Intercept - b_muN2_syntax_dev1) / (exp(b_muN2_Intercept - b_muN2_syntax_dev1) + exp(b_musubordinate_Intercept - b_musubordinate_syntax_dev1) + exp(0)),
         sub_subj = exp(b_musubordinate_Intercept + b_musubordinate_syntax_dev1) / (exp(b_muN2_Intercept + b_muN2_syntax_dev1) + exp(b_musubordinate_Intercept + b_musubordinate_syntax_dev1) + exp(0)),
         sub_pred = exp(b_musubordinate_Intercept - b_musubordinate_syntax_dev1) / (exp(b_muN2_Intercept - b_muN2_syntax_dev1) + exp(b_musubordinate_Intercept - b_musubordinate_syntax_dev1) + exp(0)),
         basic_syntax = basic_subj - basic_pred,
         N2_syntax = N2_subj - N2_pred,
         sub_syntax = sub_subj - sub_pred
                         ) %>%
  select(basic_subj, basic_pred, N2_subj, N2_pred, sub_subj, sub_pred, basic_syntax, N2_syntax, sub_syntax) %>%
  gather(key, val) %>%
  group_by(key) %>%
  summarise(
    mean = mean(val),
    lower = HDInterval::hdi(val, credMass = 0.95)[1],
    upper = HDInterval::hdi(val, credMass = 0.95)[2]
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
posteriors_multinomial_critical
```

    ## # A tibble: 9 x 4
    ##   key             mean   lower  upper
    ##   <chr>          <dbl>   <dbl>  <dbl>
    ## 1 basic_pred    0.690   0.579  0.802 
    ## 2 basic_subj    0.784   0.678  0.865 
    ## 3 basic_syntax  0.0935  0.0155 0.176 
    ## 4 N2_pred       0.0936  0.0315 0.157 
    ## 5 N2_subj       0.0593  0.0200 0.107 
    ## 6 N2_syntax    -0.0343 -0.0879 0.0133
    ## 7 sub_pred      0.216   0.110  0.335 
    ## 8 sub_subj      0.157   0.0777 0.256 
    ## 9 sub_syntax   -0.0592 -0.137  0.0110

Compute likelihood of a credible effect of syntax for the subordinate
response category:

    ## # A tibble: 1 x 1
    ##    prob
    ##   <dbl>
    ## 1 0.954

## Detailed plots

Here the proportion of non-matching responses by-syntax in the critical
condition is plotted by-item:

``` r
d_modRef_main_responseCat %>%  
  group_by(syntax, trial_type, target) %>%
  tidyboot_mean(column = response_num) -> d_modRef_main_responseCat.targets
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

``` r
d_modRef_main_responseCat.targets %>%
  ungroup() %>%
  filter(trial_type == "critical") %>%
  mutate(syntax = factor(syntax, levels = c("subj", "pred"), 
                         labels = c("Subject NP", "Predicate NP"))) %>%
  ggplot(., aes(x=syntax, y = mean, ymin = ci_lower, ymax = ci_upper, fill=syntax)) +
  geom_col(position = position_dodge(bar.width), width = bar.width,
           alpha = 0.5, color="black", size = 0.5) +
  geom_linerange(position = position_dodge(bar.width), size = 0.5) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.5, 1))+
  ylab("Proportion of non-matching responses") +
  #theme_bw() +
  facet_wrap(~target, ncol=2) 
```

![](direct-modification-prereg-final_files/figure-gfm/plot-byItem-1.png)<!-- -->

Here, the proportion of non-matching responses is plotted by-N2. The
landmark items (buildings, trees and flowers) seem to be less sensitive
to syntactic manipulations.

``` r
d_modRef_main_responseCat %>%  
  group_by(syntax, trial_type, ref_np) %>%
  tidyboot_mean(column = response_num) -> d_modRef_main_responseCat.N2
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(strap)`

``` r
d_modRef_main_responseCat.N2 %>%
  ungroup() %>%
  filter(trial_type == "critical") %>%
  mutate(syntax = factor(syntax, levels = c("subj", "pred"), 
                         labels = c("Subject NP", "Predicate NP"))) %>%
  ggplot(., aes(x=syntax, y = mean, ymin = ci_lower, ymax = ci_upper, fill=syntax)) +
  geom_col(position = position_dodge(bar.width), width = bar.width,
           alpha = 0.5, color="black", size = 0.5) +
  geom_linerange(position = position_dodge(bar.width), size = 0.5) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.5, 1))+
  ylab("Proportion of non-matching responses") +
  #theme_bw() +
  facet_wrap(~ref_np, ncol = 1)
```

![](direct-modification-prereg-final_files/figure-gfm/plot-byN2-1.png)<!-- -->

The counts of different response types by-N2 for checking if there are
any inconsistencies:

``` r
d_modRef_main_responseCat_3way %>%
  filter(trial_type == "critical") %>%
  ggplot(., aes(x = response_cat, fill = response_cat)) +
  geom_bar(alpha = 0.8) +
  facet_wrap(ref_np~syntax, ncol=2) 
```

![](direct-modification-prereg-final_files/figure-gfm/counts-byN2-1.png)<!-- -->

## Exploratory descriptive stats

Look at the number of non-switchers (participants sticking to one type
of response throughout):

``` r
# number of participants not switching between matching & non-matching responses 
d_modRef_main_responseCat %>% group_by(submission_id, response_cat) %>% count() %>% spread(response_cat, n) %>% filter((is.na(match) | is.na(nonmatch))) %>% nrow() 
```

    ## [1] 125

``` r
# proportion of non-switching participants under two-way response classification
125/300
```

    ## [1] 0.4166667

``` r
# number of participants not switching the response category under 3-way response categorization
d_modRef_main_responseCat_3way %>% group_by(submission_id, response_cat) %>% count() %>% 
  spread(response_cat, n) %>% mutate(basic = ifelse(is.na(basic), 0, basic),
                                     N2 = ifelse(is.na(N2), 0, N2),
                                     subordinate = ifelse(is.na(subordinate), 0, subordinate),
                                     sum = basic + N2 + subordinate) %>%
  filter((basic == sum) | (N2 == sum) | (subordinate == sum)) %>% nrow()
```

    ## [1] 76

``` r
# proportion of non-switching participants under 3-way response classification 
76/300
```

    ## [1] 0.2533333
