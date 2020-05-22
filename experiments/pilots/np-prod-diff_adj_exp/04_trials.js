// In this file you can specify the trial data for your experiment
const utt_prenominal = {question1b: "That's a big ", question2b: "", question1s: "That's a small ", question2s: "", condition: "prenominal"}
const utt_predicative = {question1b: "That ", question2b: " is big.", question1s: "That ", question2s: " is small.", condition:"predicative"}
// const utt_prenominal_small = [{question1: "That's a small ", question2: "" }]
// const utt_predicative_bla = [ {question1: "That ", question2: " is small."}]

const utterance = _.sample([utt_prenominal, utt_predicative])
const adj = [[utterance.question1b, utterance.question2b, "big"], [utterance.question1s, utterance.question2s, "small"]];
// const size = function() {
//    return _.sample([0,1])
//  }

const mainTrials = { dogs: [ [{

   context_picture: "images/dog_parade.png",
   item: "dogs",
    picture: "images/great-dane.jpg",
    context: "You see a group of animals.",
    text: "Another animal belongs to the group.",
    sentence: "You say to your friend:",
    question1: adj[0][0],
    question2: adj[0][1],
    size_adj: adj[0][2],
    condition: utterance.condition,
    adj_cond: "congruent",
    size_target: "big"
  },
  {
    context_picture: "images/dog_parade.png",
    item: "dogs",
     picture: "images/great-dane-s.jpg",
     context: "You see a group of animals.",
     text: "Another animal belongs to the group.",
     sentence: "You say to your friend:",
     question1: adj[1][0],
     question2: adj[1][1],
     size_adj: adj[1][2],
     condition: utterance.condition,
     adj_cond: "incongruent",
     size_target: "big"
  }
],
  [
  {
   context_picture: "images/dog_parade_small.png",
   item: "dogs",
    picture: "images/chihuahua.jpg",
    context: "You see a group of animals.",
    text: "Another animal belongs to the group.",
    sentence: "You say to your friend:",
    question1: adj[1][0],
    question2: adj[1][1],
    size_adj: adj[1][2],
    condition: utterance.condition,
    adj_cond: "congruent",
    size_target: "small"
  },
  {
   context_picture: "images/dog_parade_small.png",
   item: "dogs",
    picture: "images/chihuahua-b.jpg",
    context: "You see a group of animals.",
    text: "Another animal belongs to the group.",
    sentence: "You say to your friend:",
    question1: adj[0][0],
    question2: adj[0][1],
    size_adj: adj[0][2],
    condition: utterance.condition,
    adj_cond: "incongruent",
    size_target: "small"
  }
]],
  birds: [ [{
    context_picture: "images/bird-parade.png",
    item: "birds",
     picture: "images/swan.jpg",
     context: "You see a group of animals.",
     text: "Another animal belongs to the group.",
     sentence: "You say to your friend:",
     question1: adj[0][0],
     question2: adj[0][1],
     size_adj: adj[0][2],
     condition: utterance.condition,
     adj_cond: "congruent",
     size_target: "big"
   },
   {
     context_picture: "images/bird-parade.png",
     item: "birds",
      picture: "images/swan-s.jpg",
      context: "You see a group of animals.",
      text: "Another animal belongs to the group.",
      sentence: "You say to your friend:",
      question1: adj[1][0],
      question2: adj[1][1],
      size_adj: adj[1][2],
      condition: utterance.condition,
      adj_cond: "incongruent",
      size_target: "big"
    }
  ],
   [
    {
    context_picture: "images/bird-parade-small.png",
    item: "birds",
     picture: "images/colibri.jpg",
     context: "You see a group of animals.",
     text: "Another animal bleongs to the group.",
     sentence: "You say to your friend:",
     question1: adj[1][0],
     question2: adj[1][1],
     size_adj: adj[1][2],
     condition: utterance.condition,
     adj_cond: "congruent",
     size_target: "small"
   },
   {
     context_picture: "images/bird-parade-small.png",
     item: "birds",
      picture: "images/colibri-b.jpg",
      context: "You see a group of animals.",
      text: "Another animal bleongs to the group.",
      sentence: "You say to your friend:",
      question1: adj[0][0],
      question2: adj[0][1],
      size_adj: adj[0][2],
      condition: utterance.condition,
      adj_cond: "incongruent",
      size_target: "small"
    }
  ]],
   monkeys: [ [{
     context_picture: "images/monkey-parade.png",
     item: "primates",
      picture: "images/gorilla.jpg",
      context: "You see a group of animals.",
      text: "Another animal belongs to the group.",
      sentence: "You say to your friend:",
      question1: adj[0][0],
      question2: adj[0][1],
      size_adj: adj[0][2],
      condition: utterance.condition,
      adj_cond: "congruent",
      size_target: "big"
    },
    {
      context_picture: "images/monkey-parade.png",
      item: "primates",
       picture: "images/gorilla_s.jpg",
       context: "You see a group of animals.",
       text: "Another animal belongs to the group.",
       sentence: "You say to your friend:",
       question1: adj[1][0],
       question2: adj[1][1],
       size_adj: adj[1][2],
       condition: utterance.condition,
       adj_cond: "incongruent",
       size_target: "big"
     }
  ],
    [
     {
       context_picture: "images/monkey-parade-small.png",
       item: "primates",
        picture: "images/spidermonkey.jpg",
        context: "You see a group of animals.",
        text: "Another animal belongs to the group.",
        sentence: "You say to your friend:",
        question1: adj[1][0],
        question2: adj[1][1],
        size_adj: adj[1][2],
        condition: utterance.condition,
        adj_cond: "congruent",
        size_target: "small"
      },
      {
        context_picture: "images/monkey-parade-small.png",
        item: "primates",
         picture: "images/spidermonkey-b.jpg",
         context: "You see a group of animals.",
         text: "Another animal belongs to the group.",
         sentence: "You say to your friend:",
         question1: adj[0][0],
         question2: adj[0][1],
         size_adj: adj[0][2],
         condition: utterance.condition,
         adj_cond: "incongruent",
         size_target: "small"
       }
   ]],
    flowers: [ [{
      context_picture: "images/flower-parade.png",
      item: "flowers",
       picture: "images/sunflower.png",
       context: "You see a group of plants.",
       text: "Another plant belongs to the group.",
       sentence: "You say to your friend:",
       question1: adj[0][0],
       question2: adj[0][1],
       size_adj: adj[0][2],
       condition: utterance.condition,
       adj_cond: "congruent",
       size_target: "big"
     },
     {
       context_picture: "images/flower-parade.png",
       item: "flowers",
        picture: "images/sunflower-s.png",
        context: "You see a group of plants.",
        text: "Another plant belongs to the group.",
        sentence: "You say to your friend:",
        question1: adj[1][0],
        question2: adj[1][1],
        size_adj: adj[1][2],
        condition: utterance.condition,
        adj_cond: "incongruent",
        size_target: "big"
      },
   ],
     [
      {
        context_picture: "images/flower-parade-small.png",
        item: "flowers",
         picture: "images/daisy.png",
         context: "You see a group of plants.",
         text: "Another plant belongs to the group.",
         sentence: "You say to your friend:",
         question1: adj[1][0],
         question2: adj[1][1],
         size_adj: adj[1][2],
         condition: utterance.condition,
         adj_cond: "congruent",
         size_target: "small"
       },
       {
         context_picture: "images/flower-parade-small.png",
         item: "flowers",
          picture: "images/daisy-b.png",
          context: "You see a group of plants.",
          text: "Another plant belongs to the group.",
          sentence: "You say to your friend:",
          question1: adj[0][0],
          question2: adj[0][1],
          size_adj: adj[0][2],
          condition: utterance.condition,
          adj_cond: "incongruent",
          size_target: "small"
        }
    ]],
     fish: [[{
       context_picture: "images/fish-parade.png",
       item: "fish",
        picture: "images/swordfish.jpg",
        context: "You see a group of animals.",
        text: "Another animal belongs to the group.",
        sentence: "You say to your friend:",
        question1: adj[0][0],
        question2: adj[0][1],
        size_adj: adj[0][2],
        condition: utterance.condition,
        adj_cond: "congruent",
        size_target: "big"
      },
      {
        context_picture: "images/fish-parade.png",
        item: "fish",
         picture: "images/swordfish-s.jpg",
         context: "You see a group of animals.",
         text: "Another animal belongs to the group.",
         sentence: "You say to your friend:",
         question1: adj[1][0],
         question2: adj[1][1],
         size_adj: adj[1][2],
         condition: utterance.condition,
         adj_cond: "incongruent",
         size_target: "big"
       },
    ],
      [
       {
         context_picture: "images/fish-parade.png",
         item: "fish",
          picture: "images/goldfish.png",
          context: "You see a group of animals.",
          text: "Another animal belongs to the group.",
          sentence: "You say to your friend:",
          question1: adj[1][0],
          question2: adj[1][1],
          size_adj: adj[1][2],
          condition: utterance.condition,
          adj_cond: "congruent",
          size_target: "small"
        },
        {
          context_picture: "images/fish-parade.png",
          item: "fish",
           picture: "images/goldfish-b.png",
           context: "You see a group of animals.",
           text: "Another animal belongs to the group.",
           sentence: "You say to your friend:",
           question1: adj[0][0],
           question2: adj[0][1],
           size_adj: adj[0][2],
           condition: utterance.condition,
           adj_cond: "incongruent",
           size_target: "small"
         }
     ]],
    trees: [[{
        context_picture: "images/tree-parade.png",
        item: "trees",
         picture: "images/sequoia.jpg",
         context: "You see a group of plants.",
         text: "Another plant belongs to the group.",
         sentence: "You say to your friend:",
         question1: adj[0][0],
         question2: adj[0][1],
         size_adj: adj[0][2],
         condition: utterance.condition,
         adj_cond: "congruent",
         size_target: "big"
       },
       {
           context_picture: "images/tree-parade.png",
           item: "trees",
            picture: "images/sequoia-s.jpg",
            context: "You see a group of plants.",
            text: "Another plant belongs to the group.",
            sentence: "You say to your friend:",
            question1: adj[1][0],
            question2: adj[1][1],
            size_adj: adj[1][2],
            condition: utterance.condition,
            adj_cond: "incongruent",
            size_target: "big"
          }
     ],
       [
        {
          context_picture: "images/tree-parade.png",
          item: "trees",
           picture: "images/bonsai.jpg",
           context: "You see a group of plants.",
           text: "Another plant belongs to the group.",
           sentence: "You say to your friend:",
           question1: adj[1][0],
           question2: adj[1][1],
           size_adj: adj[1][2],
           condition: utterance.condition,
           adj_cond: "congruent",
           size_target: "small"
         },
         {
           context_picture: "images/tree-parade.png",
           item: "trees",
            picture: "images/bonsai-b.png",
            context: "You see a group of plants.",
            text: "Another plant belongs to the group.",
            sentence: "You say to your friend:",
            question1: adj[0][0],
            question2: adj[0][1],
            size_adj: adj[0][2],
            condition: utterance.condition,
            adj_cond: "incongruent",
            size_target: "small"
          }
      ]]};
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

const consistency = _.shuffle([0, 0, 0, 1, 1, 1])
const size = _.shuffle([0, 0, 0, 1, 1, 1])
const trials = _.shuffle([ {x: warmupTrials.dogs, y:mainTrials.dogs[size[0]][consistency[0]]},
  {x:warmupTrials.birds, y:mainTrials.birds[size[1]][consistency[1]]}, {x:warmupTrials.monkeys,y:mainTrials.monkeys[size[2]][consistency[2]]},
  {x:warmupTrials.flowers, y:mainTrials.flowers[size[3]][consistency[3]]}, {x:warmupTrials.fish,y:mainTrials.fish[size[4]][consistency[4]]},
  {x:warmupTrials.trees, y: mainTrials.trees[size[5]][consistency[5]]}])

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
