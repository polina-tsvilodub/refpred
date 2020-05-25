## Direct modification experiments

In this repository contains pilot experiments disentangling the effect of NP position from the syntactic modification of the noun on the comparison class inference.
Specifically, we test the effect of syntactically modified subordinate nouns (i.e. 'big Great Dane') appearing either in the subject ('That big Great Dane is a prize-winner') or in the predicate ('That prize-winner is a big Great Dane') of the sentence. The second noun denotes a visually distinctive feature of the referent.
* `modification_manipulation`: contains the baseline experiment with 5 trials, where the context contains 3 other subordinate members, the warm-ups provide click-through and labeling trials of the context members, and the target is relatively small compared to the context.
* `modificationXcongruence`: a version of the baseline experiment, where 2 trials are congruent (i.e. the Great Dane is described as 'big'), 2 trials are incongruent (i.e. the Great Dane is described as 'small') and one is assigned one of those two condition randomly. It is to improve the comparison class flexibility of the listeners.
* `modificationXrefUt`: in this version, the context also contains 2 prize-winners, to balance the referential utility of the critical and the additional noun. 
