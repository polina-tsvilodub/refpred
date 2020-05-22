# Sentence Rating Experiment

This repository contains the sentence rating experiment. It was created from the  [magpie departure point template](https://github.com/magpie-ea/magpie-departure-point).
The experiment can be viewed [here](https://syntax-rating.netlify.com/), the OSF preregistration can be found [here](https://osf.io/7jqm5).

In this experiment, the ratings of sentences describing objects in context which differ syntactically are gathered. Specifically, the position and the type of the noun phrase (the label of the object) is manipulated. For example, the to-be-rated sentences could be "That dog is big." and "That's a big dog.". The noun in the sentences can be basic-level or a subordinate category label: for example, if the referent is a great dane, the noun can be 'great dane' (subordinate) or 'dog' (basic-level).
The experiment has a two-by-two within-subject design and takes about five minutes; there are two comprehension check trials followed by six main trials. For a balanced number of conditions, the number of trials should be even.

## File Structure

Usually, you might just want to manipulate the following files:
- `01_custom_styles.css` :  contains custom styles: the styling of the rating sliders are defined here. The color, the size etc of the slider handle can be adjusted.
- `02_custom_functions.js` : can contain custom functions, variables and hooks
- `03_custom_views_templates.js` : contains custom view templates: a botcaptcha, the introduction, the post-experiment, the main and warm-up trials views. The html-code rendering the views are defined.
    - botcaptcha: number of errors allowed before the participant is block from proceeding is defined here;
    - custom_slider: main trials view; the critical sentences are inserted in ll. 230 and 247; which syntactic condition is rated at which slider is randomized between-subjects in 04_trials.js (see below)
    - post-test: custom worker information questions can be added;
    - intro-view: includes a _unique turker ID check_ (to prevent Turkers from multiple accepting the HIT multiple times) and an IP address check (US-only allowed). The script javascript-file must be included in the `index.html` file (see index.html, line 21).
- `04_trials.js` : contains the data of different trials and randomization of the different conditions.
     - `slider_warmup` contain the picture paths, the text and the slider labels for the task comprehension warm-up trials. For one trial, one sentence is clearly true and the other clearly false; for the other, one sentence is clearly false and the other may receive an intermediate rating.
     - `possible syntax` (l.24) array holds the possible syntactic conditions for the non-numeric data recording
     - `syntax` array randomizes the order in which the two syntactic conditions are presented (order is set once per subject): 0 - predicate conditions is rated at the corresponding slider; 1 - subject condition is rated at the corresponding slider (see above)  
     - `np_spec` randomizes the NP that is used in the critical sentence (the integer is the index of the corresponding NP in the `np` for every target ): 0 - subordinate NP ('great dane'); 1 - basic-level NP ('dog'). Here, three basic level and three subordinate are specified and their order is randomized.
     - `adj_congr` array helps to create congruent (adjective matches the a priori expectations about the size of the object within its basic-level category, e.g. a great dane is described as 'big') and incongruent (the adjective mismatches the a priori expectations, e.g. a great dane is described as 'small') adjective conditions. The integer is the index of the respective adjective in `adj` for every target (see below): 0 - congruent, 1 - incongruent adjective. The order is randomized. Here, only congruent conditions are defined.
     - `size` randomizes the size of the target for every context (each participant only sees one of the possible targets per context). The integer is the index of the target to be chosen within the single item objects (below). 0 - big target (great dane for dogs); 1 - small target (pug for dogs). Here three big and three small targets are defined, the order is randomized.
     - `items` object holds the context and target picture paths, the utterance parts, the NPs (sub, basic) and the adjectives (congruent, incongruent) for every target per item (= context, the keys, e.g. 'dogs1'). The first dictionary in the array holds information for the big target, the second one for the small target. There are six different items, with two possible targets each. _Information for new targets can be inserted in this object_
     - `main_trials` holds item-wise dictionaries containing information called when the view is rendered. The keys are the single items (the 6 different contexts, e.g. 'dogs1'). The correct target is accessed via indexing the items.X variable (X = name of the item) at the position corresponding to the value in `size` array at the item index;the question always includes the subordinate target label; `sentences` holds both critical utterances, the first one is always in predicate syntax, the sond one in subject syntax; the correct NP condition is accessed via indexing the items.X.np at the position corresponding to the value in `np_spec` array at the item index. All the information is recorded. _Information for new items and targets can be inserted here._
     - The main trials are shuffled.
- `05_views.js` : calls all the different kinds of views (both the custom and the magpie-template views). The instructions text, the botcaptcha text and names are defined here. The names of the views should be the same as the variable names.
- `06_main.js` : contains the experiment structure and information about deployment. `view_seq` defines the order of the trials. The `UniqueTurkerID` is defined here.
- `index.html` renders the experiment. The images are preloaded to speed up the loading; if additional scripts are used, they must be called here.

The numbering of the files is important, you can use the functions defined in earlier files in later ones, but not the other way around; that means you can use functions defined in `01` in `04`, but you can't use some variable from `05` in `02`.
