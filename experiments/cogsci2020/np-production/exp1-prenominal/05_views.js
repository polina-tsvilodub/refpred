// In this file you can instantiate your views
// We here first instantiate wrapping views, then the trial views


/** Wrapping views below

* Obligatory properties

    * trials: int - the number of trials this view will appear
    * name: string

*Optional properties
    * buttonText: string - the text on the button (default: 'next')
    * text: string - the text to be displayed in this view
    * title: string - the title of this view

    * More about the properties and functions of the wrapping views - https://magpie-project.github.io/magpie-docs/01_designing_experiments/01_template_views/#wrapping-views

*/

// Every experiment should start with an intro view. Here you can welcome your participants and tell them what the experiment is about

var speaker = _.sample(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"]);
var listener = _.sample(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"]);


const botcaptcha = custom_botcaptcha({
  name: 'botcaptcha',
  trials: 1,
  story: speaker + ' says to ' + listener + ': "It\'s a beautiful day, isn\'t it?"',
  question: "Who is " + speaker + " talking to?",
  speaker: speaker,
  listener: listener

});


// For most tasks, you need instructions views
const instructions = magpieViews.view_generator("instructions", {
    trials: 1,
    name: 'instrucions',
    title: 'Instructions',
    text:  `Please read the instructions carefully before starting the experiment and
            make sure you can concentrate on the task without disturbance.

            <br />
            <br />
            The experiment consists of two rounds, each having a few warm-up trials before the main trials start.
            <br />
            <br />
            During the warm-up trials, you will see pictures of objects.
            <br />
            <br />
            Please label the objects. You will be given feedback on your labels.
            <br />
            <br />
            By pressing the button 'Go to Trials' you begin the first round.

            <br/>
            <br/>


            `,
    buttonText: 'go to trials'
});


// In the post test questionnaire you can ask your participants addtional questions
const custom_post_test = custom_post_test_view({
  name: 'post_test',
  title: 'Additional information',
  text: `Please enter your native languages.
  <br />
    Answering the other questions is optional, but your answers will help us analyze our results.`,
  trials: 1
});
const custom_intro = custom_intro_view({
  name: 'Intro',
  title: 'Welcome!',
  picture1: 'images/cpl.png',
  trials: 1

});
// The 'thanks' view is crucial; never delete it; it submits the results!
const thanks = magpieViews.view_generator("thanks", {
    trials: 1,
    name: 'thanks',
    title: 'Thank you for taking part in this experiment!',
    prolificConfirmText: 'Press the button'
});


const warmup_trials1 = custom_textfield_warmup({
  name: 'warmup1',
  title: 'Labeling task',
  trials: 3,
  data: trial_info.text_insertion_warmup1,
//  hook: {
//    after_response_enabled: check_response
//  }
});
const warmup_trials2 = custom_textfield_warmup({
  name: 'warmup2',
  title: 'Labeling task',
  trials: 3,
  data: trial_info.text_insertion_warmup2,
//  hook: {
//    after_response_enabled: check_response
//  }
});

const warmup_trials3 = custom_textfield_warmup({
  name: 'warmup3',
  title: 'Labeling task',
  trials: 3,
  data: trial_info.text_insertion_warmup3,
//  hook: {
//    after_response_enabled: check_response
//  }
});

const context1 = magpieViews.view_generator("instructions",{
    trials: 1,
    name: 'instructions',
    title: 'Instructions',
    text:  `
    Next, you will complete the main trials.
    <br/>
    <br/>
    You and your friend watch groups of objects and talk about them.
    <br />
    <br />

    Press the button 'Go to trials' to begin the main trials.
            `,
    buttonText: 'go to trials'
});
const context2 = magpieViews.view_generator("instructions",{
    trials: 1,
    name: 'instructions',
    title: 'Instructions',
    text:  `
    Now the second round of the experiment starts. You will complete similar trials.
    <br />
    </br>
    Press the button 'Go to trials' to begin the second round.
            `,
    buttonText: 'go to trials'
});

const context3 = magpieViews.view_generator("instructions",{
    trials: 1,
    name: 'instructions',
    title: 'Instructions',
    text:  `
    Now the last round of the experiment starts. You will complete similar trials.
    <br />
    </br>
    Press the button 'Go to trials' to begin the last round.
            `,
    buttonText: 'go to trials'
});

const main_trials1 = custom_textfield_main({
  name: 'main1',
//  title: 'Parades',
  trials: 3,
  data: _.shuffle(trial_info.text_insertion_main1)


});
const main_trials2 = custom_textfield_main({
  name: 'main2',
//  title: 'Parades',
  trials: 3,
  data: _.shuffle(trial_info.text_insertion_main2)


});
const main_trials3 = custom_textfield_main({
  name: 'main3',
//  title: 'Parades',
  trials: 3,
  data: _.shuffle(trial_info.text_insertion_main3)


});

// There are many more templates available:
// forced_choice, slider_rating, dropdown_choice, testbox_input, rating_scale, image_selection, sentence_choice,
// key_press, self_paced_reading and self_paced_reading_rating_scale
