# Comparison Class Inference Experiment

This repository contains the comparison class inference experiment. It was created from the  [magpie departure point template](https://github.com/magpie-ea/magpie-departure-point).
The experiment can be viewed [here](), the OSF preregistration can be found [here]().

In this experiment, participants report the comparison class they inferred by paraphrasing a sentence about an object in context.
The experiment has a two-by-three-by-two within-subject design and takes about ten minutes.
The context provides either the basic-level or the subordinate category of the object; the noun phrase of the critical sentence is either the basic-level category label, the subordinate category label of the object or the underspecified 'one'; the noun phrase can be in the subject or the predicate position. For example, the sentences could be "That dog is big.", "That's a big great dane." or "That's a big one.".
There is a comprehension check trial followed by two blocks of three warm-up and six main trials each, for a total of 12 main trials per participant.

Randomization - context sentence
Big and small
To adjust the number of trials, essentially just the arrays at the top of 04 have to be adjusted. The number of trials for a balanced number of every condition should be a multiple of 6.

## File Structure

- Usually, you might just want to manipulate the following files:
  - `01_custom_styles.css` : (optional) contains custom styles
	- `02_custom_functions.js` : contains custom functions, variables and hooks (e.g. a global coin flip)
	- `03_custom_views_templates.js` : contains custom view templates
	- `04_trials.js` : contains the data of different trials of a task
	- `05_views.js` : defines the different kinds of views, or, more generally, anything users will engage with on the screen
	- `06_main.js` : contains the experiment structure and information about deployment
- The numbering of the files is important, you can use the functions defined in earlier files in later ones, but not the other way around; that means you can use functions defined in `01` in `04`, but you can't use some variable from `05` in `02`.
