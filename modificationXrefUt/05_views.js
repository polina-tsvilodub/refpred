// here, the different views are called and the number of trials is initialized

// instruction view
// adjust description for a different exp. structure
const instructions = magpieViews.view_generator("instructions", {
  trials: 1,
  name: 'instructions',
  title: 'Instructions',
  text: `Please read the instructions carefully before starting the experiment and
            make sure you can concentrate on the task without disturbance.
            <br />
            <br />
            The experiment consists of two rounds, each having a few warm-up trials before the main trials start.
            <br />
            <br />
            In the following warm-up trials, you will see pictures of objects.
            First, you will see example trials where the objects are labeled.
            Then you will be asked to complete similar trials and provide object labels yourself. You will be given feedback on your labels.
            <br />
            <br />
            In the last warm-up trial, you will have to rephrase a sentence someone says.
            <br />
            <br />
            By pressing the button 'Go to Trials' you begin the first round.
            `,
  buttonText: 'go to trials'
});


// thanks view
const thanks = magpieViews.view_generator("thanks", {
  trials: 1,
  name: 'thanks',
  title: 'Thank you for taking part in this experiment!',
  prolificConfirmText: 'Press the button'
});

// comparison class inference task comprehension trial
const comp_class_warmup = custom_comp_class_warmup({
  name: 'comp_class_warmup',
  trials: 1,
  context: "In the main trials you will be asked to rephrase something a person said. The utterance will contain a word that is <i>relative</i> and your task is to figure out what it is relative to. <br /> <br /> For example: <br />",
  sentence: "<b> Speaker A: \"The Empire State Building is tall.\"</b>",
  question: "<i>What do you think Speaker A meant? </i>",
  paraphrase: "The Empire State Building is tall relative to other ",
  correct: "buildings, skyscrapers, houses, constructions (choose one)"
});

function reorder_trials(trials_list) {

  while(trials_list[0].trial_type != "filler"){
    trials_list = _.shuffle(trials_list)
  }
  return trials_list
}

// first main trials block
const custom_main_text1 = custom_free_paraphrase({
  trials: trial_info.text_insertion_main1.length, // all trials are called
  // name should be identical to the variable name
  name: 'custom_main_text1',
  data: reorder_trials(trial_info.text_insertion_main1),

});

// second main trials block
const custom_main_text2 = custom_free_paraphrase({
  trials: trial_info.text_insertion_main2.length, // all trials are called
  name: 'custom_main_text2',
  data: reorder_trials(trial_info.text_insertion_main2),
});


// first warmup block
const warmup1 = custom_textfield_warmup({
  name: 'warmup1',
  title: 'Labeling task',
  trials: 2,
  data: trial_info.text_insertion_warmup1,
});

// secnd warm-up block
const warmup2 = custom_textfield_warmup({
  name: 'warmup2',
  title: 'Labeling task',
  trials: 2,
  data: trial_info.text_insertion_warmup2,
});

// referential noun warm-ups
const ref1 = custom_ref_warmup({
  name: "ref1",
  title: "Warm-up trial",
  trials: 2,
  data: _.shuffle(trial_info.ref_warmup1)
})

const ref2 = custom_ref_warmup({
  name: "ref2",
  title: "Warm-up trial",
  trials: 2,
  data: _.shuffle(trial_info.ref_warmup2)
})

// speaker and listeneers names to be sampled from for the botcaptcha
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

// custom post-test questions
const custom_post_test = custom_post_test_view({
  name: 'post_test',
  title: 'Additional information',
  text: `Please enter your native languages.
  <br />
    Answering the other questions is optional, but your answers will help us analyze our results.`,
  trials: 1
});

// custom introduction view
const custom_intro = custom_intro_view({
  name: 'Intro',
  title: 'Welcome!',
  picture1: 'images/cpl.png',
  trials: 1

});

// task view after the first warm-up trials block
const context1 = magpieViews.view_generator("instructions",{
    trials: 1,
    name: 'instructions',
    title: 'Instructions',
    text:  `
    Next, you will complete the main trials.
    <br/>
    <br/>
    Please rephrase the sentence a person said.
    <br />
    <br />
    Press the button 'Go to trials' to begin the main trials.
            `,
    buttonText: 'go to trials'
});

// task view after the first main trials block
const context2 = magpieViews.view_generator("instructions",{
    trials: 1,
    name: 'instructions',
    title: 'Instructions',
    text:  `
    Now the second round of the experiment starts. You will complete similar trials.
    <br />
    <br />
    Press the button 'Go to trials' to begin the second round.
            `,
    buttonText: 'go to trials'
});
