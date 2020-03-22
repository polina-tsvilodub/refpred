// In this file trial data and some functions for trial data creation are specified

// half of the trials with big (0), half of trials with small (1) targets

// 4 trials with basic-level np (0), 4 trials with subordinate np (1), 4 trials with 'one' (2) (position in referent-list)
const referent = _.shuffle([0,0,0,0, 1,1,1,1, 2,2,2,2])

// 6 trials with predicate syntax (0), six trials with subject syntax (1)
const syntactic_cond = _.shuffle([0,0,0,0,0,0, 1,1,1,1,1,1])

// flips the coin if the big or the small referent gets the corresponding basic-level context (the other one gets respective subordinate context)
const context = function() {
  return _.shuffle([0,1])
}
// flip the coin for each context
const cont_order1 = context()
const cont_order2 = context()
const cont_order3 = context()
const cont_order4 = context()
const cont_order5 = context()
const cont_order6 = context()

const items = {
  // "_b" are the big referents
  dogs1_b: [
  // first set contains the basic-level context
    {np_type: ["basic", "sub", "one"], // NP type for recording
     syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     picture: {pic: "images/dog-parade-basic.png", adj: "big ", condition: "basic"}, // context picture and trial info
     target: "images/doberman.png", // referent picture
     // first array is predicate utterances , second is subject utterances
     utterances: [["That's a big dog.", "That's a big doberman.", "That's a big one."], ["That dog is big.", "That doberman is big.", "That one is big."]] // critical utterances
   },
   // second set contains the subordinate context
   {np_type: ["basic", "sub", "one"],
    syntax_condition: ["predicate", "subject"],
    picture: {pic: "images/dog-parade-doberman.png", adj: "big ", condition: "sub"},
    target: "images/doberman.png",
    // first array are utterances in predicate, second is subject utterances
    utterances: [["That's a big dog.", "That's a big doberman.", "That's a big one."], ["That dog is big.", "That doberman is big.", "That one is big."]]
    }
  ],
  // "_s" are the small referents
  dogs1_s : [
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: {pic: "images/dog-parade-basic.png", adj: "small ", condition: "basic"},
     target: "images/chihuahua.png",
     utterances: [["That's a small dog.", "That's a small chihuahua.", "That's a small one."], ["That dog is small.", "That chihuahua is small.", "That one is small."]]
   },
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture:  {pic: "images/dog-parade-chihuahua.png", adj: "small ", condition: "sub"} ,
    target: "images/chihuahua.png",
    utterances: [["That's a small dog.", "That's a small chihuahua.", "That's a small one."], ["That dog is small.", "That chihuahua is small.", "That one is small."]]
   }
  ],
  dogs2_b: [
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: {pic: "images/dog-parade-basic2.png", adj: "big ", condition: "basic"},
     target: "images/great-dane.png",
     utterances: [["That's a big dog.", "That's a big great dane.", "That's a big one."], ["That dog is big.", "That great dane is big.", "That one is big."]]
   },
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/dog-parade-great-dane.png", adj: "big ", condition: "sub"},
    target: "images/great-dane.png",
    utterances: [["That's a big dog.", "That's a big great dane.", "That's a big one."], ["That dog is big.", "That great dane is big.", "That one is big."]]
  }],
  dogs2_s:[
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: {pic: "images/dog-parade-basic2.png", adj: "small ", condition: "basic"},
     target: "images/pug.png",
     utterances: [["That's a small dog.", "That's a small pug.", "That's a small one."], ["That dog is small.", "That pug is small.", "That one is small."]]
   },
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/dog-parade-pug.png", adj: "small ", condition: "sub"},
    target: "images/pug.png",
    utterances: [["That's a small dog.", "That's a small pug.", "That's a small one."], ["That dog is small.", "That pug is small.", "That one is small."]]
   }
  ],
  birds_b: [
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: {pic: "images/bird-parade-basic.png", adj: "big ", condition: "basic"},
     target: "images/eagle.png",
     utterances: [["That's a big bird.", "That's a big eagle.", "That's a big one."], ["That bird is big.", "That eagle is big.", "That one is big."]]
   },
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/bird-parade-eagle.png", adj: "big ", condition: "sub"},
    target: "images/eagle.png",
    utterances: [["That's a big bird.", "That's a big eagle.", "That's a big one."], ["That bird is big.", "That eagle is big.", "That one is big."]]
  }
  ],
  birds_s: [
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: {pic: "images/bird-parade-basic.png", adj: "small ", condition: "basic"},
     target: "images/hummingbird.png",
     utterances: [["That's a small bird.", "That's a small hummingbird.", "That's a small one."], ["That bird is small.", "That hummingbird is small.", "That one is small."]]
   },
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/bird-parade-hummingbird.png", adj: "small ", condition: "sub"},
    target: "images/hummingbird.png",
    utterances: [["That's a small bird.", "That's a small hummingbird.", "That's a small one."], ["That bird is small.", "That hummingbird is small.", "That one is small."]]
   }
  ],
  fish_b: [
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: {pic: "images/fish-parade-basic.png", adj: "big ", condition: "basic"},
     target: "images/swordfish.png",
     utterances: [["That's a big fish.", "That's a big swordfish.", "That's a big one."], ["That fish is big.", "That swordfish is big.", "That one is big."]]
   },
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture:  {pic: "images/fish-parade-swordfish.png", adj: "big ", condition: "sub"},
    target: "images/swordfish.png",
    utterances: [["That's a big fish.", "That's a big swordfish.", "That's a big one."], ["That fish is big.", "That swordfish is big.", "That one is big."]]
  }
  ],
  fish_s: [
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: [{pic: "images/fish-parade-basic.png", adj: "small ", condition: "basic"}],
     target: "images/goldfish.png",
     utterances: [["That's a small fish.", "That's a small goldfish.", "That's a small one."], ["That fish is small.", "That goldfish is small.", "That one is small."]]
   },
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/fish-parade-goldfish.png", adj: "small ", condition: "sub"},
    target: "images/goldfish.png",
    utterances: [["That's a small fish.", "That's a small goldfish.", "That's a small one."], ["That fish is small.", "That goldfish is small.", "That one is small."]]
   }
  ],
  flowers_b: [
  {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
   np_type: ["basic", "sub", "one"],
   picture: {pic: "images/flower-parade-basic.png", adj: "big ", condition: "basic"},
   target: "images/sunflower.png",
   utterances: [["That's a big flower.", "That's a big sunflower.", "That's a big one."], ["That flower is big.", "That sunflower is big.", "That one is big."]]
 },
 {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
  np_type: ["basic", "sub", "one"],
  picture: {pic: "images/flower-parade-sunflower.png", adj: "big ", condition: "sub"},
  target: "images/sunflower.png",
  utterances: [["That's a big flower.", "That's a big sunflower.", "That's a big one."], ["That flower is big.", "That sunflower is big.", "That one is big."]]
}
 ],
 flowers_s: [
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/flower-parade-basic.png", adj: "small ", condition: "basic"},
    target: "images/dandelion.png",
    utterances: [["That's a small flower.", "That's a small dandelion.", "That's a small one."], ["That flower is small.", "That dandelion is small.", "That one is small."]]
  },
  {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
   np_type: ["basic", "sub", "one"],
   picture: {pic: "images/flower-parade-dandelion.png", adj: "small ", condition: "sub"},
   target: "images/dandelion.png",
   utterances: [["That's a small flower.", "That's a small dandelion.", "That's a small one."], ["That flower is small.", "That dandelion is small.", "That one is small."]]
  }
 ],
  trees_b: [
    {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
     np_type: ["basic", "sub", "one"],
     picture: {pic: "images/tree-parade-basic.png", adj: "big ", condition: "basic"},
     target: "images/redwood.png",
     utterances: [["That's a big tree.", "That's a big redwood.", "That's a big one."], ["That tree is big.", "That redwood is big.", "That one is big."]]
   },

   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/tree-parade-redwood.png", adj: "big ", condition: "sub"},
    target: "images/redwood.png",
    utterances: [["That's a big tree.", "That's a big redwood.", "That's a big one."], ["That tree is big.", "That redwood is big.", "That one is big."]]
  },
 ],
 trees_s: [
   {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
    np_type: ["basic", "sub", "one"],
    picture: {pic: "images/tree-parade-basic.png", adj: "small ", condition: "basic"},
    target: "images/bonsai.png",
    utterances: [["That's a small tree.", "That's a small bonsai.", "That's a small one."], ["That tree is small.", "That bonsai is small.", "That one is small."]]
  },
  {syntax_condition: ["predicate", "subject"], // syntactic condition for recording
   np_type: ["basic", "sub", "one"],
   picture:  {pic: "images/tree-parade-bonsai.png", adj: "small ", condition: "sub"},
   target: "images/bonsai.png",
   utterances: [["That's a small tree.", "That's a small bonsai.", "That's a small one."], ["That tree is small.", "That bonsai is small.", "That one is small."]]
 },
 ]
}


const main_trials = {
// each context is called

    dogs1:  [ // big target of each context is first
      {
            context: "You and your friend see the following:",
            context_picture: items.dogs1_b[cont_order1[0]].picture.pic,// context picture for this item is called, cont_order was already sampled and specifies if the big target gets the basic or the sub context
            pic_spec: items.dogs1_b[cont_order1[0]].picture.condition, // record context picture type
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.dogs1_b[cont_order1[0]].target, //  target picture
            utterance: "Your friend says: </br><b>" + items.dogs1_b[cont_order1[0]].utterances[syntactic_cond[0]][referent[0]] + "</b>", // get 'utterance' of a random syntactic condition (syntactic_cond[0]) with a random NP (referent[0])
            np_type: items.dogs1_b[cont_order1[0]].np_type[referent[0]], // record NP type
            syntax_condition: items.dogs1_b[cont_order1[0]].syntax_condition[syntactic_cond[0]], // record syntactic condition of the sentence
            question: "What do you think your friend meant?",
            sentence_left: "It is " + items.dogs1_b[cont_order1[0]].picture.adj + " relative to other ", // insert the corresponding adjective into question (here: big, since the target is big)
            item: "dogs1", // record context ID
            target_size: "big" // record target size

        },
        // small target of the context is second
        {
            context: "You and your friend see the following:",
            context_picture: items.dogs1_s[cont_order1[1]].picture.pic,
            pic_spec: items.dogs1_s[cont_order1[1]].picture.condition,
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.dogs1_s[cont_order1[1]].target,
            utterance: "Your friend says: </br><b>" + items.dogs1_s[cont_order1[1]].utterances[syntactic_cond[1]][referent[1]] + "</b>",
            np_type: items.dogs1_s[cont_order1[1]].np_type[referent[1]],
            syntax_condition: items.dogs1_s[cont_order1[1]].syntax_condition[syntactic_cond[1]],
            question: "What do you think your friend meant?",
            sentence_left: "It is " + items.dogs1_s[cont_order1[1]].picture.adj + " relative to other ",
            item: "dogs1",
            target_size: "small"
        }
      ],
    dogs2:  [  {
            context: "You and your friend see the following:",
            context_picture: items.dogs2_b[cont_order2[0]].picture.pic,
            pic_spec: items.dogs2_b[cont_order2[0]].picture.condition,
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.dogs2_b[cont_order2[0]].target,
            utterance: "Your friend says: </br><b>" + items.dogs2_b[cont_order2[0]].utterances[syntactic_cond[2]][referent[2]] + "</b>",
            np_type: items.dogs2_b[cont_order2[0]].np_type[referent[2]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.dogs2_b[cont_order2[0]].picture.adj + " relative to other ",
            item: "dogs2",
            target_size: "big",
            syntax_condition: items.dogs2_b[cont_order2[0]].syntax_condition[syntactic_cond[2]]


        },
        {
            context: "You and your friend see the following:",
            context_picture: items.dogs2_s[cont_order2[1]].picture.pic,
            pic_spec: items.dogs2_s[cont_order2[1]].picture.condition,
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.dogs2_s[cont_order2[1]].target,
            utterance: "Your friend says: </br><b>" + items.dogs2_s[cont_order2[1]].utterances[syntactic_cond[3]][referent[3]] + "</b>",
            np_type: items.dogs2_s[cont_order2[1]].np_type[referent[3]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.dogs2_s[cont_order2[1]].picture.adj + " relative to other ",
            item: "dogs2",
            target_size: "small",
            syntax_condition: items.dogs2_s[cont_order2[1]].syntax_condition[syntactic_cond[3]],


            }
      ],
    birds:   [ {
            context: "You and your friend see the following:",
            context_picture: items.birds_b[cont_order3[0]].picture.pic,
            pic_spec: items.birds_b[cont_order3[0]].picture.condition,
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.birds_b[cont_order3[0]].target,
            utterance:  "Your friend says: </br><b>" + items.birds_b[cont_order3[0]].utterances[syntactic_cond[4]][referent[4]] + "</b>",
            np_type: items.birds_b[cont_order3[0]].np_type[referent[4]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.birds_b[cont_order3[0]].picture.adj + " relative to other ",
            item: "birds",
            target_size: "big",
            syntax_condition: items.birds_b[cont_order3[0]].syntax_condition[syntactic_cond[4]]

        },
        {
            context: "You and your friend see the following:",
            context_picture: items.birds_s[cont_order3[1]].picture.pic,
            pic_spec: items.birds_s[cont_order3[1]].picture.condition,
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.birds_s[cont_order3[1]].target,
            utterance:"Your friend says: </br><b>" + items.birds_s[cont_order3[1]].utterances[syntactic_cond[5]][referent[5]] + "</b>",
            np_type: items.birds_s[cont_order3[1]].np_type[referent[5]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.birds_s[cont_order3[1]].picture.adj + " relative to other ",
            item: "birds",
            target_size: "small",
            syntax_condition: items.birds_s[cont_order3[1]].syntax_condition[syntactic_cond[5]]

            }
      ],
  fish:   [   {
            context: "You and your friend see the following:",
            context_picture: items.fish_b[cont_order4[0]].picture.pic,
            pic_spec: items.fish_b[cont_order4[0]].picture.condition,
            text:"Your friend runs far ahead of you, and you see him in the distance: " ,
            pic: items.fish_b[cont_order4[0]].target,
            utterance: "Your friend says: </br><b>" + items.fish_b[cont_order4[0]].utterances[syntactic_cond[6]][referent[6]] + "</b>",
            np_type: items.fish_b[cont_order4[0]].np_type[referent[6]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.fish_b[cont_order4[0]].picture.adj + " relative to other ",
            item: "fish",
            target_size: "big",
            syntax_condition: items.fish_b[cont_order4[0]].syntax_condition[syntactic_cond[6]]

        },
        {
            context: "You and your friend see the following:",
            context_picture: items.fish_s[cont_order4[1]].picture.pic,
            pic_spec: items.fish_s[cont_order4[1]].picture.condition,
            text:"Your friend runs far ahead of you, and you see him in the distance: " ,
            pic: items.fish_s[cont_order4[1]].target,
            utterance: "Your friend says: </br><b>" + items.fish_s[cont_order4[1]].utterances[syntactic_cond[7]][referent[7]] + "</b>",
            np_type: items.fish_s[cont_order4[1]].np_type[referent[7]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.fish_s[cont_order4[1]].picture.adj + " relative to other ",
            item: "fish",
            target_size: "small",
            syntax_condition: items.fish_s[cont_order4[1]].syntax_condition[syntactic_cond[7]]

              }
        ],
  flowers:    [  {
            context: "You and your friend see the following:",
            context_picture: items.flowers_b[cont_order5[0]].picture.pic,
            pic_spec: items.flowers_b[cont_order5[0]].picture.condition,
            text:"Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.flowers_b[cont_order5[0]].target,
            utterance: "Your friend says: </br><b>" + items.flowers_b[cont_order5[0]].utterances[syntactic_cond[8]][referent[8]] + "</b>",
            np_type: items.flowers_b[cont_order5[0]].np_type[referent[8]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.flowers_b[cont_order5[0]].picture.adj + " relative to other ",
            item: "flowers",
            target_size: "big",
            syntax_condition: items.flowers_b[cont_order5[0]].syntax_condition[syntactic_cond[8]]

        },
        {
            context: "You and your friend see the following:",
            context_picture: items.flowers_s[cont_order5[1]].picture.pic,
            pic_spec: items.flowers_s[cont_order5[1]].picture.condition,
            text:"Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.flowers_s[cont_order5[1]].target,
            utterance: "Your friend says: </br><b>" + items.flowers_s[cont_order5[1]].utterances[syntactic_cond[9]][referent[9]] + "</b>",
            np_type: items.flowers_s[cont_order5[1]].np_type[referent[9]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.flowers_s[cont_order5[1]].picture.adj + " relative to other ",
            item: "flowers",
            target_size: "small",
            syntax_condition: items.flowers_s[cont_order5[1]].syntax_condition[syntactic_cond[9]]

              }
        ],
    trees:   [ {
            context: "You and your friend see the following:",
            context_picture: items.trees_b[cont_order6[0]].picture.pic,
            pic_spec: items.trees_b[cont_order6[0]].picture.condition,
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.trees_b[cont_order6[0]].target,
            utterance: "Your friend says: </br><b>" + items.trees_b[cont_order6[0]].utterances[syntactic_cond[10]][referent[10]] + "</b>",
            np_type: items.trees_b[cont_order6[0]].np_type[referent[10]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.trees_b[cont_order6[0]].picture.adj + " relative to other ",
            item: "trees",
            target_size: "big",
            syntax_condition: items.trees_b[cont_order6[0]].syntax_condition[syntactic_cond[10]]

        },
        {
            context: "You and your friend see the following:",
            context_picture: items.trees_s[cont_order6[1]].picture.pic,
            pic_spec: items.trees_s[cont_order6[1]].picture.condition,
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.trees_s[cont_order6[1]].target,
            utterance: "Your friend says: </br><b>" + items.trees_s[cont_order6[1]].utterances[syntactic_cond[11]][referent[11]] + "</b>",
            np_type: items.trees_s[cont_order6[1]].np_type[referent[11]],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.trees_s[cont_order6[1]].picture.adj + " relative to other ",
            item: "trees",
            target_size: "small",
            syntax_condition: items.trees_s[cont_order6[1]].syntax_condition[syntactic_cond[11]]

        }
      ]
};

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
  picture1: "warmup/swordfish.jpg",
  picture2: "warmup/goldfish.png",
  correct1: ["swordfish"],
  correct2: ["goldfish"],
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

// shuffle sets of warmup trials and the corresponding big and small targets
 // x: warmup trial
 // y: big target
 // z: small target
const trials = _.shuffle([ {x: warmup_trials.dogs1, y:main_trials.dogs1[0], z:main_trials.dogs1[1]}, {x: warmup_trials.dogs2, y:main_trials.dogs2[0], z:main_trials.dogs2[1]},
  {x:warmup_trials.birds, y:main_trials.birds[0], z:main_trials.birds[1]},
  {x:warmup_trials.flowers, y:main_trials.flowers[0], z:main_trials.flowers[1]},
  {x:warmup_trials.fish, y:main_trials.fish[0], z:main_trials.fish[1]},
  {x:warmup_trials.trees, y: main_trials.trees[0], z:main_trials.trees[1]}])

  const trial_info = {

     text_insertion_main1: [
// get three items for the first main trial block
// bigg targets
       trials[0].y,
       trials[1].y,
       trials[2].y,
       // small targets
       trials[0].z,
       trials[1].z,
       trials[2].z

    ],
    // get items for the second block
    text_insertion_main2 :[
      // big targets
      trials[3].y,
      trials[4].y,
      trials[5].y,
      // small targets
      trials[3].z,
      trials[4].z,
      trials[5].z
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
