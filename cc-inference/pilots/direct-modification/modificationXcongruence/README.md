# Comparison Class Inference Experiment

This repository contains the comparison class inference experiment, created from the  [magpie departure point template](https://github.com/magpie-ea/magpie-departure-point).
The experiment can be viewed [here](https://prereg-cc-inf.netlify.com/), the OSF preregistration can be found [here](https://osf.io/3rhg2).

In this experiment, participants indicate the comparison class they inferred by paraphrasing a sentence about an object in context (it is a free production Experiment).

The experiment has a two-by-three-by-two within-subject design and takes about ten minutes.
We manipulate the context of the object, the type of the NP and the syntax of the critical sentence (12 condiitons within-subject).
The context provides either the basic-level or the subordinate category of the object; the noun phrase of the critical sentence is either the basic-level category label, the subordinate category label of the object or the underspecified 'one'; the noun phrase can be in the subject or the predicate position. For example, the sentences could be "That's a big dog.", "That's a big great dane." or "That's a big one." (predicate condition); or "that dog is big" etc. (subject condition). The number of trials for a balanced number of every condition should be a multiple of 12.

There is a comprehension check trial followed by two blocks of three warm-up and six main trials each, for a total of 12 main trials per participant.


## File Structure
The important files are the following:

- `01_custom_styles.css` : (optional) can contain custom styles
- `02_custom_functions.js` : can contain custom functions, variables and hooks
- `03_custom_views_templates.js` : contains custom view templates: a botcaptcha, the introduction, the post-experiment, the main and warm-up trials views. The html-code rendering the views, the response correctness check functions for the warm-up trials are defined.
    - botcaptcha: number of errors allowed before the participant is block from proceeding is defined here;
    - main & warm-up trials: the number of characters to be entered before the button to proceed to the next view appears can be adjusted (`minChars`);
    - post-test: custom worker information questions can be added;
    - intro-view: includes a _unique turker ID check_ (to prevent Turkers from multiple accepting the HIT multiple times) and an IP address check (US-only allowed). The script javascript-file must be included in the `index.html` file (see index.html, line 21).
- `04_trials.js` : contains the data of different trials and randomization of the different conditions. The number of trials should be a multiple of 6 for equal numbers of different conditions.
    - `referent` (l.6) array randomizes the NP that is used in the critical sentence (the integer is the index of the corresponding utterance in the `utterances` object defining utterances for every target ): 0 - basic NP (e.g. 'That dog is big'); 1 - subordinate NP (e.g. 'That great dane is big'); 2 - underspecified 'one' (e.g. 'That one is big'). Here, four basic level, four subordinate and four 'one' trials are specified and their order is randomized.
    - `syntactic_cond` (l.9) array randomizes the syntactic condition of the critical utterance (the integer is the index of the array holding the possible critical utterance with the corresponding syntax in the `utterances` object): 0 - predicate condition utterances ('That's a bg dog'); 1 - subject condition utterances ('That dog is big'). Here, six predicate and six subject condition trials are defined.      
    - `context`-function (l.12) flips a coin for the context type: 0 - basic-level context picture; 1 - subordinate context picture (the integer is the index of the dictionary with the corresponding trial information of every item, see below). The context is flipped for all the six items, determining if the basic-level or the subordinate context is the one to be assigned to the big target within the item. The other context is assigned to the small target.
    - `items` (l.23) object holds the picture path, the possible utterances and the trial information for recording for all the targets (the keys, e.g. 'dogs1_b'). The first dictionary in the value array holds information for the basic-level context case, the second dictionary holds information for the subordinate context case. _Information for new targets can be inserted in this object_
    - `main_trials` (l.201) holds item-wise dictionaries containing information called when the view is rendered. The keys are the single items (here: 6 different types, e.g. 'dogs1'); the first dictionary in every item renders the big targets, the second the small target. The correct context picture is accessed via indexing the cont_orderX variable (X = index of the item) at the position corresponding to the target index; the utterance is accessed via getting the integer in `syntactic_cond`-array at the index corresponding to the overall target index (first index for the `utterances` object) and getting the integer in the `referent`-array at the index corresponding to overall target index (second index for the `utterances` object). All the information is recorded. _Information for new items and targets can be inserted here._
    - `warmup_trials` holds the paths to the warm-up trial pictures and the correct response options.
    - `trials` (l.475) matches the warm-up trials and the corresponding main trials. `trial_info` separates the randomized-order trials into two blocks of main and warm-up trials; it is accessed when the views are called in the next file.   
- `05_views.js` : calls all the different kinds of views (both the custom and the magpie-template views). The instructions text, the botcaptcha text and names are defined here. The names of the views should be the same as the variable names.
- `06_main.js` : contains the experiment structure and information about deployment. `view_seq` defines the order of the trials. The `UniqueTurkerID` is defined here.
-  `index.html` renders the experiment. The images are preloaded to speed up the loading; if additional scripts are used, they must be called here.

The numbering of the files is important, you can use the functions defined in earlier files in later ones, but not the other way around; that means you can use functions defined in `01` in `04`, but you can't use some variable from `05` in `02`.
