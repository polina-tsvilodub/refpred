
/** Wrapping views below

* Obligatory properties

    * trials: int - the number of trials this view will appear
    * name: string

*Optional properties
    * buttonText: string - the text on the button (default: 'next')
    * text: string - the text to be displayed in this view
    * title: string - the title of this view

    * More about the properties and functions of the wrapping views - https://magpie-ea.github.io/magpie-docs/01_designing_experiments/01_template_views/#wrapping-views

*/


//  instructions views
const instructions = magpieViews.view_generator("instructions", {
  trials: 1,
  name: 'instructions',
  title: 'Instructions',
  text: `Please read the instructions carefully before starting the experiment and
            make sure you can concentrate on the task without disturbance.
            <br />
            <br />
            The experiment consists of two warm-up trials and six main trials.
            <br />
            <br />
            First, you will complete the warm-up trials.
            <br />
            <br />
            You will see pictures of objects. Imagine someone says something about these objects. <br />
            Please rate how well the uttered sentences describe the objects.
            <br />
            <br />
            By pressing the button 'Go to Trials' you begin the first warm-up trial.

            `,
  buttonText: 'go to trials'
});

// slider rating task comprehension warm-up
const custom_warmup = custom_warmup_slider({
  trials: slider_warmup.warmup1.length,
  name: 'custom_warmup',
  data: _.shuffle(slider_warmup.warmup1)
})

// The 'thanks' view is crucial; never delete it; it submits the results!
const thanks = magpieViews.view_generator("thanks", {
  trials: 1,
  name: 'thanks',
  title: 'Thank you for taking part in this experiment!',
  prolificConfirmText: 'Press the button'
});

// main trials
const main_slider = custom_slider({
  // This will use all trials specified in `data`, you can user a smaller value (for testing), but not a larger value
  trials: trial_info.main.length,
  // name should be identical to the variable name
  name: 'main_slider',
  data: _.shuffle(trial_info.main),

});


// speaker and listener names that can be used in the btcaptcha
var speaker = _.sample(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"]);
var listener = _.sample(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"]);
// botcaptcha
const botcaptcha = custom_botcaptcha({
  name: 'botcaptcha',
  trials: 1,
  story: speaker + ' says to ' + listener + ': "It\'s a beautiful day, isn\'t it?"',
  question: "Who is " + speaker + " talking to?",
  speaker: speaker,
  listener: listener

});

// post-experiment view
const custom_post_test = custom_post_test_view({
  name: 'post_test',
  title: 'Additional information',
  text: `Please enter your native languages.
  <br />
    Answering the other questions is optional, but your answers will help us analyze our results.`,
  trials: 1
});
// introduction view
const custom_intro = custom_intro_view({
  name: 'Intro',
  title: 'Welcome!',
  picture1: 'images/cpl.png',
  trials: 1

});

// instructions after the warm-ups
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
