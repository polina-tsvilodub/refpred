// In this file you can specify the trial data for your experiment
const utt_prenominal = {utterance1b: "That's a ", utterance2b: "", utterance1s: "That's a ", utterance2s: "", condition: "prenominal"}
const utt_predicative = {utterance1b: "That ", utterance2b: " is ", utterance1s: "That ", utterance2s: " is ", condition:"predicative"}
const size = function () {
  return _.sample([1, 2])
}

const utterance = utt_prenominal;
const target_size = _.shuffle([0,0,0,1,1,1])
const referent = _.shuffle([0,0,0,0, 1,1,1,1, 2,2,2,2])
const picture = _.shuffle([0,0,0,1,1,1])

const syntactic_cond = _.shuffle([0,0,0,0,0,0, 1,1,1,1,1,1])
const context = function() {
  return _.shuffle([0,1])
}
const cont_order1 = context()
const cont_order2 = context()
const cont_order3 = context()
const cont_order4 = context()
const cont_order5 = context()
const cont_order6 = context()

const items = {
  dogs1_b: [
    {referent: ["dog", "doberman", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: [{pic: "images/dog-parade-basic.png", adj: "big ", condition: "basic"}],
     target: "images/doberman.png",
     // first utterance is predicate, second is subject NP
     utterances: [["That's a big dog.", "That's a big doberman.", "That's a big one."], ["That dog is big.", "That doberman is big.", "That one is big."]]
   },
   {referent: ["dog", "doberman", "one"],
    utterance1: utterance.utterance1b,
    utterance2: utterance.utterance2b,
    picture: [{pic: "images/dog-parade-doberman.png", adj: "big ", condition: "congr"}],
    target: "images/doberman.png",
    // first utterance is predicate, second is subject NP
    utterances: [["That's a big dog.", "That's a big doberman.", "That's a big one."], ["That dog is big.", "That doberman is big.", "That one is big."]]
    }
  ],
  dogs1_s : [
    {referent: ["dog","chihuahua", "one"],
     utterance1: utterance.utterance1s,
     utterance2: utterance.utterance2s,
     picture: [{pic: "images/dog-parade-basic.png", adj: "small ", condition: "basic"} ],
     target: "images/chihuahua.png",
     utterances: [["That's a small dog.", "That's a small chihuahua.", "That's a small one."], ["That dog is small.", "That chihuahua is small.", "That one is small."]]
   },
   {referent: ["dog","chihuahua", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: [ {pic: "images/dog-parade-chihuahua.png", adj: "small ", condition: "congr"} ],
    target: "images/chihuahua.png",
    utterances: [["That's a small dog.", "That's a small chihuahua.", "That's a small one."], ["That dog is small.", "That chihuahua is small.", "That one is small."]]
   }
  ],
  dogs2_b: [
    {referent: ["dog", "great dane", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: [{pic: "images/dog-parade-basic2.png", adj: "big ", condition: "basic"}],
     target: "images/great-dane.png",
     utterances: [["That's a big dog.", "That's a big great dane.", "That's a big one."], ["That dog is big.", "That great dane is big.", "That one is big."]]
   },
   {referent: ["dog", "great dane", "one"],
    utterance1: utterance.utterance1b,
    utterance2: utterance.utterance2b,
    picture: [ {pic: "images/dog-parade-great-dane.png", adj: "big ", condition: "congr"}],
    target: "images/great-dane.png",
    utterances: [["That's a big dog.", "That's a big great dane.", "That's a big one."], ["That dog is big.", "That great dane is big.", "That one is big."]]
  }],
  dogs2_s:[
    {referent: ["dog", "pug", "one"],
     utterance1: utterance.utterance1s,
     utterance2: utterance.utterance2s,
     picture: [{pic: "images/dog-parade-basic2.png", adj: "small ", condition: "basic"}],
     target: "images/pug.png",
     utterances: [["That's a small dog.", "That's a small pug.", "That's a small one."], ["That dog is small.", "That pug is small.", "That one is small."]]
   },
   {referent: ["dog", "pug", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: [{pic: "images/dog-parade-pug.png", adj: "small ", condition: "congr"}],
    target: "images/pug.png",
    utterances: [["That's a small dog.", "That's a small pug.", "That's a small one."], ["That dog is small.", "That pug is small.", "That one is small."]]
   }
  ],
  birds_b: [
    {referent: ["bird", "eagle", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: [{pic: "images/bird-parade-basic.png", adj: "big ", condition: "basic"}],
     target: "images/eagle.png",
     utterances: [["That's a big bird.", "That's a big eagle.", "That's a big one."], ["That bird is big.", "That eagle is big.", "That one is big."]]
   },
   {referent: ["bird", "eagle", "one"],
    utterance1: utterance.utterance1b,
    utterance2: utterance.utterance2b,
    picture: [{pic: "images/bird-parade-eagle.png", adj: "big ", condition: "congr"}],
    target: "images/eagle.png",
    utterances: [["That's a big bird.", "That's a big eagle.", "That's a big one."], ["That bird is big.", "That eagle is big.", "That one is big."]]
  }
  ],
  birds_s: [
    {referent: ["bird", "hummingbird", "one"],
     utterance1: utterance.utterance1s,
     utterance2: utterance.utterance2s,
     picture: [{pic: "images/bird-parade-basic.png", adj: "small ", condition: "basic"}],
     target: "images/hummingbird.png",
     utterances: [["That's a small bird.", "That's a small hummingbird.", "That's a small one."], ["That bird is small.", "That hummingbird is small.", "That one is small."]]
   },
   {referent: ["bird", "hummingbird", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: [{pic: "images/bird-parade-hummingbird.png", adj: "small ", condition: "congr"}],
    target: "images/hummingbird.png",
    utterances: [["That's a small bird.", "That's a small hummingbird.", "That's a small one."], ["That bird is small.", "That hummingbird is small.", "That one is small."]]
   }
  ],
  fish_b: [
    {referent: ["fish", "swordfish", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: [{pic: "images/fish-parade-basic.png", adj: "big ", condition: "basic"}],
     target: "images/swordfish.png",
     utterances: [["That's a big fish.", "That's a big swordfish.", "That's a big one."], ["That fish is big.", "That swordfish is big.", "That one is big."]]
   },
   {referent: ["fish", "swordfish", "one"],
    utterance1: utterance.utterance1b,
    utterance2: utterance.utterance2b,
    picture: [ {pic: "images/fish-parade-swordfish.png", adj: "big ", condition: "congr"}],
    target: "images/swordfish.png",
    utterances: [["That's a big fish.", "That's a big swordfish.", "That's a big one."], ["That fish is big.", "That swordfish is big.", "That one is big."]]
  }
  ],
  fish_s: [
    {referent: ["fish", "goldfish", "one"],
     utterance1: utterance.utterance1s,
     utterance2: utterance.utterance2s,
     picture: [{pic: "images/fish-parade-basic.png", adj: "small ", condition: "basic"}],
     target: "images/goldfish.png",
     utterances: [["That's a small fish.", "That's a small goldfish.", "That's a small one."], ["That fish is small.", "That goldfish is small.", "That one is small."]]
   },
   {referent: ["fish", "goldfish", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: [{pic: "images/fish-parade-goldfish.png", adj: "small ", condition: "congr"}],
    target: "images/goldfish.png",
    utterances: [["That's a small fish.", "That's a small goldfish.", "That's a small one."], ["That fish is small.", "That goldfish is small.", "That one is small."]]
   }
  ],
  flowers_b: [
  {referent: ["flower", "sunflower", "one"],
   utterance1: utterance.utterance1b,
   utterance2: utterance.utterance2b,
   picture: [{pic: "images/flower-parade-basic.png", adj: "big ", condition: "basic"}],
   target: "images/sunflower.png",
   utterances: [["That's a big flower.", "That's a big sunflower.", "That's a big one."], ["That flower is big.", "That sunflower is big.", "That one is big."]]
 },
 {referent: ["flower", "sunflower", "one"],
  utterance1: utterance.utterance1b,
  utterance2: utterance.utterance2b,
  picture: [{pic: "images/flower-parade-sunflower.png", adj: "big ", condition: "congr"}],
  target: "images/sunflower.png",
  utterances: [["That's a big flower.", "That's a big sunflower.", "That's a big one."], ["That flower is big.", "That sunflower is big.", "That one is big."]]
}
 ],
 flowers_s: [
   {referent: ["flower", "dandelion", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: [{pic: "images/flower-parade-basic.png", adj: "small ", condition: "basic"}],
    adj: "small",
    target: "images/dandelion.png",
    utterances: [["That's a small flower.", "That's a small dandelion.", "That's a small one."], ["That flower is small.", "That dandelion is small.", "That one is small."]]
  },
  {referent: ["flower", "dandelion", "one"],
   utterance1: utterance.utterance1s,
   utterance2: utterance.utterance2s,
   picture: [{pic: "images/flower-parade-dandelion.png", adj: "small ", condition: "congr"}],
   adj: "small",
   target: "images/dandelion.png",
   utterances: [["That's a small flower.", "That's a small dandelion.", "That's a small one."], ["That flower is small.", "That dandelion is small.", "That one is small."]]
  }
 ],
  trees_b: [
    {referent: ["tree", "redwood", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: [{pic: "images/tree-parade-basic.png", adj: "big ", condition: "basic"}],
     adj: "big",
     target: "images/redwood.png",
     utterances: [["That's a big tree.", "That's a big redwood.", "That's a big one."], ["That tree is big.", "That redwood is big.", "That one is big."]]
   },

   {referent: ["tree", "redwood", "one"],
    utterance1: utterance.utterance1b,
    utterance2: utterance.utterance2b,
    picture: [{pic: "images/tree-parade-redwood.png", adj: "big ", condition: "congr"}],
    adj: "big",
    target: "images/redwood.png",
    utterances: [["That's a big tree.", "That's a big redwood.", "That's a big one."], ["That tree is big.", "That redwood is big.", "That one is big."]]
  },
 ],
 trees_s: [
   {referent: ["tree", "bonsai", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: [{pic: "images/tree-parade-basic.png", adj: "small ", condition: "basic"}],
    adj: "small",
    target: "images/bonsai.png",
    utterances: [["That's a small tree.", "That's a small bonsai.", "That's a small one."], ["That tree is small.", "That bonsai is small.", "That one is small."]]
  },
  {referent: ["tree", "bonsai", "one"],
   utterance1: utterance.utterance1s,
   utterance2: utterance.utterance2s,
   picture: [ {pic: "images/tree-parade-bonsai.png", adj: "small ", condition: "congr"}],
   adj: "small",
   target: "images/bonsai.png",
   utterances: [["That's a small tree.", "That's a small bonsai.", "That's a small one."], ["That tree is small.", "That bonsai is small.", "That one is small."]]
 },
 ]
}


// const utterance1 = utterance.question1
// const utterance2 = utterance.question2

const main_trials = {

    dogs1:  [ {
            context: "You and your friend see the following:",
            context_picture: items.dogs1_b[cont_order1[0]].picture[0].pic,
            pic_spec: cont_order1[0], // if 0, basic, if 1 subordinate context
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.dogs1_b[cont_order1[0]].target,
            utterance: items.dogs1_b[cont_order1[0]].utterances[syntactic_cond[0]][referent[0]],
            ref_spec: referent[0], // if 0 basic, if 1 sub, if 2 one
            question: "What do you think your friend meant?",
            sentence_left: "It is " + items.dogs1_b[cont_order1[0]].picture[0].adj + " relative to other ",
            item: "dogs1",
            // if 0: big, if 1 small
            target_size: "big",
            condition: syntactic_cond[0], // if 0 predicate, if 1 subject NP
        },
        {
                context: "You and your friend see the following:",
                context_picture: items.dogs1_s[cont_order1[1]].picture[0].pic,
                pic_spec: cont_order1[1], // if 0, basic, if 1 subordinate context
                text: "Your friend runs far ahead of you, and you see him in the distance: ",
                pic: items.dogs1_s[cont_order1[1]].target,
                utterance: items.dogs1_s[cont_order1[1]].utterances[syntactic_cond[1]][referent[1]],
                ref_spec: referent[1], // if 0 basic, if 1 sub, if 2 one
                question: "What do you think your friend meant?",
                sentence_left: "It is " + items.dogs1_s[cont_order1[1]].picture[0].adj + " relative to other ",
                item: "dogs1",
                // if 0: big, if 1 small
                target_size: "small",
                condition: syntactic_cond[1], // if 0 predicate, if 1 subject NP
            }
      ],
    dogs2:  [  {
            context: "You and your friend see the following:",
            context_picture: items.dogs2_b[cont_order2[0]].picture[0].pic,
            pic_spec: cont_order2[0],
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.dogs2_b[cont_order2[0]].target,
            utterance: items.dogs2_b[cont_order2[0]].utterances[syntactic_cond[2]][referent[2]],
            ref_spec: referent[2],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.dogs2_b[cont_order2[0]].picture[0].adj + " relative to other ",
            item: "dogs2",
            target_size: "big",
            condition: syntactic_cond[2],
            // adj_cond: items.dogs2[target_size[1]].picture[picture[1]].condition

        },
        {
                context: "You and your friend see the following:",
                context_picture: items.dogs2_s[cont_order2[1]].picture[0].pic,
                pic_spec: cont_order2[1],
                text: "Your friend runs far ahead of you, and you see him in the distance: ",
                pic: items.dogs2_s[cont_order2[1]].target,
                utterance: items.dogs2_s[cont_order2[1]].utterances[syntactic_cond[3]][referent[3]],
                ref_spec: referent[2],
                question:  "What do you think your friend meant?",
                sentence_left: "It is " + items.dogs2_s[cont_order2[1]].picture[0].adj + " relative to other ",
                item: "dogs2",
                target_size: "small",
                condition: syntactic_cond[3],
                // adj_cond: items.dogs2[target_size[1]].picture[picture[1]].condition

            }
      ],
    birds:   [ {
            context: "You and your friend see the following:",
            context_picture: items.birds_b[cont_order3[0]].picture[0].pic,
            pic_spec: cont_order3[0],
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.birds_b[cont_order3[0]].target,
            utterance: items.birds_b[cont_order3[0]].utterances[syntactic_cond[4]][referent[4]],
            ref_spec: referent[4],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.birds_b[cont_order3[0]].picture[0].adj + " relative to other ",
            item: "birds",
            target_size: "big",
            condition: syntactic_cond[4],
            // adj_cond: items.birds[target_size[2]].picture[picture[2]].condition

        },
        {
                context: "You and your friend see the following:",
                context_picture: items.birds_s[cont_order3[1]].picture[0].pic,
                pic_spec: cont_order3[1],
                text: "Your friend runs far ahead of you, and you see him in the distance: ",
                pic: items.birds_s[cont_order3[1]].target,
                utterance: items.birds_s[cont_order3[1]].utterances[syntactic_cond[5]][referent[5]],
                ref_spec: referent[5],
                question:  "What do you think your friend meant?",
                sentence_left: "It is " + items.birds_s[cont_order3[1]].picture[0].adj + " relative to other ",
                item: "birds",
                target_size: "small",
                condition: syntactic_cond[5],
                // adj_cond: items.birds[target_size[2]].picture[picture[2]].condition

            }
      ],
  fish:   [   {
            context: "You and your friend see the following:",
            context_picture: items.fish_b[cont_order4[0]].picture[0].pic,
            pic_spec: cont_order4[0],
            text:"Your friend runs far ahead of you, and you see him in the distance: " ,
            pic: items.fish_b[cont_order4[0]].target,
            utterance: items.fish_b[cont_order4[0]].utterances[syntactic_cond[6]][referent[6]],
            ref_spec: referent[6],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.fish_b[cont_order4[0]].picture[0].adj + " relative to other ",
            item: "fish",
            target_size: "big",
            condition: syntactic_cond[6],
            // adj_cond: items.fish[target_size[3]].picture[picture[3]].condition

        },
        {
                  context: "You and your friend see the following:",
                  context_picture: items.fish_s[cont_order4[1]].picture[0].pic,
                  pic_spec: cont_order4[1],
                  text:"Your friend runs far ahead of you, and you see him in the distance: " ,
                  pic: items.fish_s[cont_order4[1]].target,
                  utterance: items.fish_s[cont_order4[1]].utterances[syntactic_cond[7]][referent[7]],
                  ref_spec: referent[7],
                  question:  "What do you think your friend meant?",
                  sentence_left: "It is " + items.fish_s[cont_order4[1]].picture[0].adj + " relative to other ",
                  item: "fish",
                  target_size: "small",
                  condition: syntactic_cond[7],
                  // adj_cond: items.fish[target_size[3]].picture[picture[3]].condition

              }
        ],
  flowers:    [  {
            context: "You and your friend see the following:",
            context_picture: items.flowers_b[cont_order5[0]].picture[0].pic,
            pic_spec: cont_order5[0],
            text:"Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.flowers_b[cont_order5[0]].target,
            utterance: items.flowers_b[cont_order5[0]].utterances[syntactic_cond[8]][referent[8]],
            ref_spec: referent[8],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.flowers_b[cont_order5[0]].picture[0].adj + " relative to other ",
            item: "flowers",
            target_size: "big",
            condition: syntactic_cond[8],
            // adj_cond: items.flowers[target_size[4]].picture[picture[4]].condition

        },
        {
                  context: "You and your friend see the following:",
                  context_picture: items.flowers_s[cont_order5[1]].picture[0].pic,
                  pic_spec: cont_order5[1],
                  text:"Your friend runs far ahead of you, and you see him in the distance: ",
                  pic: items.flowers_s[cont_order5[1]].target,
                  utterance: items.flowers_s[cont_order5[1]].utterances[syntactic_cond[9]][referent[9]],
                  ref_spec: referent[9],
                  question:  "What do you think your friend meant?",
                  sentence_left: "It is " + items.flowers_s[cont_order5[1]].picture[0].adj + " relative to other ",
                  item: "flowers",
                  target_size: "small",
                  condition: syntactic_cond[9],
                  // adj_cond: items.flowers[target_size[4]].picture[picture[4]].condition

              }
        ],
    trees:   [ {
            context: "You and your friend see the following:",
            context_picture: items.trees_b[cont_order6[0]].picture[0].pic,
            pic_spec: cont_order6[0],
            text: "Your friend runs far ahead of you, and you see him in the distance: ",
            pic: items.trees_b[cont_order6[0]].target,
            utterance: items.trees_b[cont_order6[0]].utterances[syntactic_cond[10]][referent[10]],
            ref_spec: referent[10],
            question:  "What do you think your friend meant?",
            sentence_left: "It is " + items.trees_b[cont_order6[0]].picture[0].adj + " relative to other ",
            item: "trees",
            target_size: "big",
            condition: syntactic_cond[10],
            // adj_cond: items.trees[target_size[5]].picture[picture[5]].condition

        },
        {
                context: "You and your friend see the following:",
                context_picture: items.trees_s[cont_order6[1]].picture[0].pic,
                pic_spec: cont_order6[1],
                text: "Your friend runs far ahead of you, and you see him in the distance: ",
                pic: items.trees_s[cont_order6[1]].target,
                utterance: items.trees_s[cont_order6[1]].utterances[syntactic_cond[11]][referent[11]],
                ref_spec: referent[11],
                question:  "What do you think your friend meant?",
                sentence_left: "It is " + items.trees_s[cont_order6[1]].picture[0].adj + " relative to other ",
                item: "trees",
                target_size: "small",
                condition: syntactic_cond[11],
                // adj_cond: items.trees[target_size[5]].picture[picture[5]].condition

            }
      ]
};


const warmup_trials = {dogs1: {
  item: "dogs",
  picture1: "warmup/chihuahua.jpg",
  picture2: "warmup/doberman.png",
  correct1: ["chihuahua"],
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

const trials = _.shuffle([ {x: warmup_trials.dogs1, y:main_trials.dogs1[0], z:main_trials.dogs1[1]}, {x: warmup_trials.dogs2, y:main_trials.dogs2[0], z:main_trials.dogs2[1]},
  {x:warmup_trials.birds, y:main_trials.birds[0], z:main_trials.birds[1]},
  {x:warmup_trials.flowers, y:main_trials.flowers[0], z:main_trials.flowers[1]},
  {x:warmup_trials.fish, y:main_trials.fish[0], z:main_trials.fish[1]},
  {x:warmup_trials.trees, y: main_trials.trees[0], z:main_trials.trees[1]}])

  const trial_info = {

       text_insertion_main1: [

         trials[0].y,
         trials[1].y,
         trials[2].y,
         trials[0].z,
         trials[1].z,
         trials[2].z

    ],
    text_insertion_main2 :[
      trials[3].y,
      trials[4].y,
      trials[5].y,
      trials[3].z,
      trials[4].z,
      trials[5].z
    ],
     text_insertion_warmup1: [
       trials[0].x,
       trials[1].x,
       trials[2].x
    ],
    text_insertion_warmup2: [
      trials[3].x,
      trials[4].x,
      trials[5].x
    ]
  };
