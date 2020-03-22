// In this file trial data and some functions for trial data creation are specified

// half of the trials with big (0), half of trials with small (1) targets

// 4 trials with basic-level np (0), 4 trials with subordinate np (1), 4 trials with 'one' (2) (position in referent-list)
// const referent = _.shuffle([0,0,0,0, 1,1,1,1, 2,2,2,2])

// 6 trials with predicate syntax (0), six trials with subject syntax (1)
const syntactic_cond = _.shuffle([0,0,0,1,1,1])

const target_size = _.shuffle([0,0,0,1,1,1])

const syntax = ["subject", "predicate"]

const contexts = _.shuffle(["dogs1", "dogs2", "birds", "flowers", "trees", "fish"])
console.log(contexts)
// flips the coin if the big or the small referent gets the corresponding basic-level context (the other one gets respective subordinate context)
// const context = function() {
//   return _.shuffle([0,1])
// }
// flip the coin for each context
// const cont_order1 = context()
// const cont_order2 = context()
// const cont_order3 = context()
// const cont_order4 = context()
// const cont_order5 = context()
// const cont_order6 = context()

///////////////////////////
//   adjust if needed
const num_trials = 6
//////////////////////////

// creating views with all the necessary information
function create_view(items, syntactic_cond, target_size, syntax, contexts, num_trials) {
  const expt_views = []
  for ( i = 0; i < num_trials; i ++) { // the iterator iterates over all the contexts and takes one target per context (either big or small)
    const view = {
      context: items[contexts[i]][target_size[i]].context_sent +  "and you see the folowing:", // target_size indicates if the target is big or small within the given context
      context_picture: items[contexts[i]][target_size[i]].context_picture, // context picture is chose (it is the same for both big and small targets)
      text: "Your friend goes ahead of you. You see your friend in the distance:", // text appearing above the target picture
      target_picture: items[contexts[i]][target_size[i]].target, // target picture, differs for bis and small target
      utterance: "Your friend says: <br/><b>" + items[contexts[i]][target_size[i]].utterances[syntactic_cond[i]] + ".</b>", // syntactic_cond indicates whether the critical NP is subject or predicate (randomly assugned per trial)
      item: contexts[i],
      target: items[contexts[i]][target_size[i]].item,
      syntax: syntax[syntactic_cond[i]], // record the synactic condition  as string
      adj: items[contexts[i]][target_size[i]].adj, // record target size
      question: "What do you think is it " + items[contexts[i]][target_size[i]].adj + " relative to?", // task question
      paraphrase: "It is " + items[contexts[i]][target_size[i]].adj + " relative to other " // paraphrase template
    }
    expt_views.push(view);
  }
  return expt_views;
}

const items = {
  // "_b" are the big referents
  dogs1: [
  // first set contains the basic-level context
    {
     item: "doberman",
     context_sent: "You and your friend are at an animal training ground ",
     context_picture: "images/dog-parade-basic.png",
     adj: "big",
     target: "images/doberman.png",
     // first array is subject  , second is predicate
     utterances: ["That big doberman is a service animal", "That service animal is a big doberman."],
     reference: "service animal" // critical utterances
   },
   // second set contains the subordinate context
   {
    item: "chihuahua",
    context_sent: "You and your friend are at an animal training ground ",
    context_picture: "images/dog-parade-basic.png",
    adj: "small",
    target: "images/chihuahua.png",
    // first array are utterances in predicate, second is subject utterances
    utterances: ["That small chihuahua is a service animal", "That service animal is a small chihuahua" ],
    reference: "service animal"
    }
  ],
  // "_s" are the small referents

  dogs2: [
    {
     item: "great dane",
     context_sent: "You and your friend are at a pet show ",
     context_picture: "images/dog-parade-basic2.png",
     adj: "big ",
     target: "images/great-dane.png",
     utterances: ["That big Great Dane is a prize-winner", "That prize-winner is a big Great Dane"],
     reference: "prize-winner"
   },
   {
    item: "pug",
    context_sent: "You and your friend are at a pet show ",
    context_picture: "images/dog-parade-basic2.png",
    adj: "small ",
    target: "images/pug.png",
    utterances: ["That small pug is a prize-winner", "That prize-winner is a small pug"],
    reference: "prize-winner"
   }
  ],
  birds: [
    {
     item: "eagle",
     context_sent: "You visit your friend who works at a zoo ",
     context_picture: "images/bird-parade-basic.png",
     adj: "big ",
     target: "images/eagle.png",
     utterances: ["That big eagle is a new exemplar", "That new exemplar is a big eagle"],
     reference: "new exemplar"
   },
   {
    item: "hummingbird",
    context_sent: "You visit your friend who works at a zoo ",
    context_picture: "images/bird-parade-basic.png",
    adj: "small ",
    target: "images/hummingbird.png",
    utterances: ["That small hummingbird is a new exemplar", "That new exemplar is a small hummingbird"],
    reference: "new exemplar"
   }
  ],
  fish: [
    {
     item: "tuna",
     context_sent: "You and your friend are at a fish market ",
     context_picture: "images/fish-parade-basic.png",
     adj: "big ",
     target: "images/tuna_net.png",
     utterances: ["That big tuna is a gem", "That gem is a big tuna"],
     reference: "gem"
   },
   {
    item: "clownfish",
    context_sent: "You and your friend are at a fish market ",
    context_picture: "images/fish-parade-basic.png",
    adj: "small ",
    target: "images/clownfish_net.png",
    utterances: ["That small clownfish is a gem", "That gem is a small clownfish"],
    reference: "gem"
   }
  ],
  flowers: [
  {
   item: "sunflower",
   context_sent: "You and your friend are at their garden ",
   context_picture: "images/flower-parade-basic.png",
   adj: "big ",
   target: "images/sunflower.png",
   utterances: ["That big sunflower is a gift", "That gift is a big sunflower"],
   reference: "gift"
 },
  {
   item: "dandelion",
   context_sent: "You and your friend are at their garden ",
   context_picture: "images/flower-parade-basic.png",
   adj: "small ",
   target: "images/dandelion.png",
   utterances: ["That small dandelion is a gift", "That gift is a small dandelion"],
   reference: "gift"
  }
 ],
  trees: [
    {
     item: "redwood",
     context_sent: "You and your friend walk to their cabin in a park for the first time. You want to memorize the path ",
     context_picture: "images/tree-parade-basic.png",
     adj: "big ",
     target: "images/redwood.png",
     utterances: ["That big redwood is a sign", "That sign is a big redwood"],
     reference: "sign"
   },
  {
   item: "bonsai",
   context_sent:  "You and your friend walk to their cabin in a park for the first time. You want to memorize the path ",
   context_picture: "images/tree-parade-basic.png",
   adj: "small ",
   target: "images/bonsai.png",
   utterances: ["That small bonsai is a sign", "That sign is a small bonsai"],
   reference: "sign"
 }
 ]
}

// warm-up trial information
// a warm-up block contains the same targets that the following main block contains
const warmup_trials = {dogs1: {
  item: "dogs",
  picture1: "warmup/chihuahua.jpg",
  picture2: "warmup/doberman.png",
  correct1: ["chihuahua"], // correct labels for the feedback
  correct2: ["doberman"],
  correct3: ["dogs"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a ",
  question2: "These are both"
},
dogs2: {
  item: "dogs",
  picture1: "warmup/pug.jpg",
  picture2: "warmup/great-dane.jpg",
  correct1: ["pug"],
  correct2: ["great dane"],
  correct3: ["dogs"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a ",
  question2: "These are both"
},
birds: {
  item: "birds",
  picture1: "warmup/colibri.jpg",
  picture2: "warmup/eagle.jpg",
  correct1: ["hummingbird"],
  correct2: ["eagle"],
  correct3: ["birds"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question3: "This is an ",
  question2: "These are both"
},
flowers: {
  item: "flowers",
  picture1: "warmup/dandelion.jpg",
  picture2: "warmup/sunflower.png",
  correct1: ["dandelion"],
  correct2: ["sunflower"],
  correct3: ["flowers"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a ",
  question2: "These are both"
},
fish: {
  item: "fish",
  picture1: "warmup/tuna.jpg",
  picture2: "warmup/clownfish.jpg",
  correct1: ["tuna"],
  correct2: ["clownfish"],
  correct3: ["fish"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a ",
  question2: "These are both"
},
trees: {
  item: "trees",
  picture1: "warmup/sequoia.png",
  picture2: "warmup/bonsai.jpg",
  correct1: "redwood or sequoia (choose one)",
  correct2: ["bonsai"],
  correct3: ["trees"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a ",
  question2: "These are both"
}
}

const main_trials = create_view(items, syntactic_cond, target_size, syntax, contexts, num_trials)

// shuffle sets of warmup trials and the corresponding big and small targets
 // x: warmup trial
 // y: big target
 // z: small target
 // the trials are already shuffled in the context list
const trials = [
  {x:warmup_trials[contexts[0]], y:main_trials[0]},
  {x:warmup_trials[contexts[1]], y:main_trials[1]},
  {x:warmup_trials[contexts[2]], y:main_trials[2]},
  {x:warmup_trials[contexts[3]], y:main_trials[3]},
  {x:warmup_trials[contexts[4]], y:main_trials[4]},
  {x:warmup_trials[contexts[5]], y:main_trials[5]}
]

  const trial_info = {

     text_insertion_main1: [
// get three items for the first main trial block

       trials[0].y,
       trials[1].y,
       trials[2].y
    ],
    // get items for the second block
    text_insertion_main2 :[

      trials[3].y,
      trials[4].y,
      trials[5].y
    ],
     text_insertion_warmup1: [
       // get the warmup trials corresponding to the main trials in the first main block
       trials[0].x,
       trials[1].x,
       trials[2].x
    ],
    text_insertion_warmup2: [
      // warm-up trials for second block
      trials[3].x,
      trials[4].x,
      trials[5].x
    ]
  };
