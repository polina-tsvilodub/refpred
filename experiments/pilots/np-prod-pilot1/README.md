# Speaker Production Experiment

This repository contains the script for a speaker production experiment based on the [_babe environment](https://github.com/babe-project/departure-point), to be run in on-line on English native speakers. The goal of the experiment is to verify whether speakers tend to establish subordinate comparison classes in a certain syntactic frame.  

## Structure

Participants see a picture of e. g. an animal and they are told that they see this object as they walk by. They are asked to judge the size of this object by completing one of the two sentential frames:
1. That is a big ____ .
2. The ____ is big.

Here, given a picture of a dog, specifically a Great Dane, animal would be superordinate, dog would be the basic level and Great Dane the subordinate category label. We hypothesize that ... .
The assignment of pictures to sentential frames is randomized, the ratio of the two frames throughout the whole experiment for each participant is 1:1.   

## Warm-up trials proposal
The first part of the experiment consists of warm-up trials to make the participants use subordinate category labels. The participants are supposed to describe a picture of a situation involving members of the same basic level category belonging to two different subordinate categories, such that subordinate label is necessary to describe the situation. For example, a picture could depict a Great Dane chasing a Yorkshire Terrier, whereas the answer template looks like this:
*The \___ is chasing the ___ .*
Depending on whether the participants are able to recognize and reproduce the subordinate categories, they will be included in the main experiment.
## Main trials
In the main trials, we test whether the comparison class established in a given situation depends on the syntactic frame. Participants see pictures of e. g. an animal they are supposed to be walking by. The are asked to judge the size of this object by referring to a friend and uttering one of the two possible sentences. The comparison class label has to be inserted.


### Obtaining the `departure point`

1. install npm by following these [instructions](https://www.npmjs.com/get-npm)
2. download or clone this github repository: https://github.com/babe-project/departure-point
   - e.g. type `git clone https://github.com/babe-project/departure-point.git`
3. change the folder name `departure-point` to whatever you like
   - let's say you call is `my-exp`, e.g. by typing `mv departure-point my-exp`
4. go to your folder `my-exp`, e.g., by typing `cd my-exp`
5. now type `npm install`; this will download the Javascript packages with the most current version of _babe
6. you can have a look at the example experiment by opening the file `index.html` now
7. you can now start editing to create your own experiment

### Changing the `departure point` to your own experiment

- Usually, you might just want to manipulate the following files:
    - `01_custom_styles.css` :: (optional) contains custom styles
	- `02_custom_functions.js` :: (optional) contains custom functions, variables and hooks (e.g. a global coin flip)
	- `03_custom_views_templates.js` :: (optional) contains user-defined special-purpose view templates (only needed, if the provided view templates are not enough for your experiment)
	- `04_trials.js` :: (optional) contains the data of different trials of a task (e.g., names of pictures, test sentences, questions, etc.)
	- `05_views.js` :: defines the different kinds of tasks, or, more generally, anything users will engage with on the screen
	- `06_main.js` :: contains the experiment structure and general information about deployment
- The numbering of the files is important, you can use the functions defined in `01` in `04`, but you can't use some variable from `05` in `02`
