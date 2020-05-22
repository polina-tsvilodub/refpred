# Speaker Production Experiment

This repository contains the speaker noun free production experiment based on the [magpie departure point template](https://github.com/magpie-ea/magpie-departure-point). The goal of the experiment is to test if speakers adjust the nouns they produce given different syntactic frames. The syntactic frames are tested between-subjects: The **subject-NP** experiment can be view [here](https://exp1-pred.netlify.com/); the **predicate-NP** experiment can be viewed [here](https://exp1-prenom.netlify.com/).

The syntactic frame (2 conditions: subject NP vs. predicate NP) is manipulated between-subjects. Participants see a picture of an object in context and are asked to judge the size of this object by completing one of the two sentential frames:
1. That is a big ____ . (predicate NP condition)
2. The ____ is big. (subject NP condition)
This repo contains the subject condition.
The experiment consists of two blocks with three warm-up and three main trials each and takes about 7-9 minutes.   

## File Structure

The file structure is identical for both experimental conditions.
The important files are the following:
- `01_custom_styles.css` :: (optional) can contain custom styles
- `02_custom_functions.js` :: (optional) can contain custom functions, variables and hooks
- `03_custom_views_templates.js` :: contains custom view templates: a botcaptcha, the introduction, the post-experiment, the main and warm-up trials views. The html-code rendering the views, the response correctness check functions for the warm-up trials are defined.
  - botcaptcha: number of errors allowed before the participant is block from proceeding is defined here;
  - main & warm-up trials: the number of characters to be entered before the button to proceed to the next view appears can be adjusted (`minChars`);
  - post-test: custom worker information questions can be added;
  - intro-view: includes a _unique turker ID check_ (to prevent Turkers from multiple accepting the HIT multiple times) and an IP address check (US-only allowed). The script javascript-file must be included in the `index.html` file (see index.html).
- `04_trials.js` :: contains the data of different trials and randomization of the different conditions.
- `05_views.js` : calls all the different kinds of views (both the custom and the magpie-template views). The instructions text, the botcaptcha text and names are defined here. The names of the views should be the same as the variable names.
- `06_main.js` : contains the experiment structure and information about deployment. `view_seq` defines the order of the trials. The `UniqueTurkerID` is defined here.
-  `index.html` renders the experiment. The images are preloaded to speed up the loading; if additional scripts are used, they must be called here.

The numbering of the files is important, you can use the functions defined in `01` in `04`, but you can't use some variable from `05` in `02`
