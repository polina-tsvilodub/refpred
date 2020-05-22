// In this file you can specify the trial data for your experiment
const utt_prenominal = {question1b: "That's a big ", question2b: "", question1s: "That's a small ", question2s: "", condition: "prenominal"}
const utt_predicative = {question1b: "That ", question2b: " is big.", question1s: "That ", question2s: " is small.", condition:"predicative"}
// const utt_prenominal_small = [{question1: "That's a small ", question2: "" }]
// const utt_predicative_bla = [ {question1: "That ", question2: " is small."}]

const utterance = _.sample([utt_prenominal, utt_predicative])
const size = function() {
   return _.sample([0,1])
 }

const mainTrials = { dogs: [{
   context_picture: "images/dog_parade.png",
   item: "dogs",
    picture: "images/great-dane.jpg",
    context: "You see a group of animals.",
    text: "Another animal belongs to the group.",
    sentence: "You say to your friend:",
    question1: utterance.question1b,
    question2: utterance.question2b,
    condition: utterance.condition,
    size: "big"
  },
  {
   context_picture: "images/dog_parade_small.png",
   item: "dogs",
    picture: "images/chihuahua.jpg",
    context: "You see a group of animals.",
    text: "Another animal belongs to the group.",
    sentence: "You say to your friend:",
    question1: utterance.question1s,
    question2: utterance.question2s,
    condition: utterance.condition,
    size: "small"
  }],
  birds: [{
    context_picture: "images/bird-parade.png",
    item: "birds",
     picture: "images/swan.jpg",
     context: "You see a group of animals.",
     text: "Another animal belongs to the group.",
     sentence: "You say to your friend:",
     question1: utterance.question1b,
     question2: utterance.question2b,
     condition: utterance.condition,
     size: "big"
   },
   {
     context_picture: "images/bird-parade-small.png",
     item: "birds",
      picture: "images/colibri.jpg",
      context: "You see a group of animals.",
      text: "Another animal bleongs to the group.",
      sentence: "You say to your friend:",
      question1: utterance.question1s,
      question2: utterance.question2s,
      condition: utterance.condition,
      size: "small"
    }],
   monkeys: [{
     context_picture: "images/monkey-parade.png",
     item: "primates",
      picture: "images/gorilla.jpg",
      context: "You see a group of animals.",
      text: "Another animal belongs to the group.",
      sentence: "You say to your friend:",
      question1: utterance.question1b,
      question2: utterance.question2b,
      condition: utterance.condition,
      size: "big"
    },
    {
      context_picture: "images/monkey-parade-small.png",
      item: "primates",
       picture: "images/spidermonkey.jpg",
       context: "You see a group of animals.",
       text: "Another animal belongs to the group.",
       sentence: "You say to your friend:",
       question1: utterance.question1s,
       question2: utterance.question2s,
       condition: utterance.condition,
       size: "small"
     }],
    flowers: [{
      context_picture: "images/flower-parade.png",
      item: "flowers",
       picture: "images/sunflower.png",
       context: "You see a group of plants.",
       text: "Another plant belongs to the group.",
       sentence: "You say to your friend:",
       question1: utterance.question1b,
       question2: utterance.question2b,
       condition: utterance.condition,
       size: "big"
     },
     {
       context_picture: "images/flower-parade-small.png",
       item: "flowers",
        picture: "images/daisy.png",
        context: "You see a group of plants.",
        text: "Another plant belongs to the group.",
        sentence: "You say to your friend:",
        question1: utterance.question1s,
        question2: utterance.question2s,
        condition: utterance.condition,
        size: "small"
      }],
     fish: [{
       context_picture: "images/fish-parade.png",
       item: "fish",
        picture: "images/swordfish.jpg",
        context: "You see a group of animals.",
        text: "Another animal belongs to the group.",
        sentence: "You say to your friend:",
        question1: utterance.question1b,
        question2: utterance.question2b,
        condition: utterance.condition,
        size: "big"
      },
      {
        context_picture: "images/fish-parade.png",
        item: "fish",
         picture: "images/goldfish.png",
         context: "You see a group of animals.",
         text: "Another animal belongs to the group.",
         sentence: "You say to your friend:",
         question1: utterance.question1s,
         question2: utterance.question2s,
         condition: utterance.condition,
         size: "small"
       }],
    trees: [{
        context_picture: "images/tree-parade.png",
        item: "trees",
         picture: "images/sequoia.jpg",
         context: "You see a group of plants.",
         text: "Another plant belongs to the group.",
         sentence: "You say to your friend:",
         question1: utterance.question1b,
         question2: utterance.question2b,
         condition: utterance.condition,
         size: "big"
       },
       {
         context_picture: "images/tree-parade.png",
         item: "trees",
          picture: "images/bonsai.jpg",
          context: "You see a group of plants.",
          text: "Another plant belongs to the group.",
          sentence: "You say to your friend:",
          question1: utterance.question1s,
          question2: utterance.question2s,
          condition: utterance.condition,
          size: "small"
        }]};
const warmupTrials = {dogs: {
  item: "dogs",
  picture1: "images/chihuahua.jpg",
  picture2: "images/great-dane.jpg",
  correct1: ["chihuahua"],
  correct2: ["great dane"],
  correct3: ["dogs"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question2: "These are both"
},

birds: {
  item: "birds",
  picture1: "images/swan.jpg",
  picture2: "images/colibri.jpg",
  correct1: ["swan"],
  correct2: ["hummingbird"],
  correct3: ["birds"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question2: "These are both"
},
monkeys: {
  item: "primates",
  picture1: "images/spidermonkey.jpg",
  picture2: "images/gorilla.jpg",
  correct1: ["spider monkey"],
  correct2: ["gorilla"],
  correct3: ["monkeys", "apes", "primates"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question2: "These are both"
},
flowers: {
  item: "flowers",
  picture1: "images/daisy.png",
  picture2: "images/sunflower.png",
  correct1: ["daisy"],
  correct2: ["sunflower"],
  correct3: ["flowers"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question2: "These are both"
},
fish: {
  item: "fish",
  picture1: "images/swordfish.jpg",
  picture2: "images/goldfish.png",
  correct1: ["swordfish"],
  correct2: ["goldfish"],
  correct3: ["fish"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question2: "These are both"
},
trees: {
  item: "trees",
  picture1: "images/sequoia.jpg",
  picture2: "images/bonsai.jpg",
  correct1: ["redwood", "sequoia"],
  correct2: ["bonsai"],
  correct3: ["trees"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question2: "These are both"
}
}

const trials = _.shuffle([ {x: warmupTrials.dogs, y:mainTrials.dogs[size()]},
  {x:warmupTrials.birds, y:mainTrials.birds[size()]}, {x:warmupTrials.monkeys,y:mainTrials.monkeys[size()]},
  {x:warmupTrials.flowers, y:mainTrials.flowers[size()]}, {x:warmupTrials.fish,y:mainTrials.fish[size()]},
  {x:warmupTrials.trees, y: mainTrials.trees[size()]}])

const trial_info = {

     text_insertion_main1: [
       // mainTrials.dogs[size()],
       // mainTrials.birds[size()],
       // mainTrials.monkeys[size()],
       // mainTrials.flowers[size()],
       // mainTrials.fish[size()],
       // mainTrials.trees[size()]
       trials[0].y,
       trials[1].y,
       trials[2].y

  ],
  text_insertion_main2 :[
    trials[3].y,
    trials[4].y,
    trials[5].y
  ],
   text_insertion_warmup1: [
     trials[0].x,
     trials[1].x,
     trials[2].x

    // warmupTrials.trees[0]
  ],
  text_insertion_warmup2: [
    trials[3].x,
    trials[4].x,
    trials[5].x
  ]
};
