// In this file you can specify the trial data for your experiment
const utt_prenominal = {utterance1b: "That's a big ", utterance2b: "", utterance1s: "That's a small ", utterance2s: "", condition: "prenominal"}
const utt_predicative = {utterance1b: "That ", utterance2b: " is big", utterance1s: "That ", utterance2s: " is small", condition:"predicative"}
const size = function () {
  return _.sample([0,1])
}

const utterance = utt_prenominal;
const target_size = _.shuffle([0,0,0,1,1,1])
const referent = _.shuffle([0,0,0,1,1,1])
const picture = _.shuffle([0,0,0,1,1,1])

const items = {
  dogs1: [
    {referent: ["doberman", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/dog-parade-basic.png", "images/dog-parade-doberman.png"],
     adj: "big"
   },
   {referent: ["chihuahua", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/dog-parade-basic.png", "images/dog-parade-chihuahua.png"],
    adj: "small"
   }
  ],
  dogs2: [
    {referent: ["great dane", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/dog-parade-basic2.png", "images/dog-parade-great-dane.png"],
     adj: "big"
   },
   {referent: ["pug", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/dog-parade-basic2.png", "images/dog-parade-pug.png"],
    adj: "small"
   }
  ],
  birds: [
    {referent: ["eagle", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/bird-parade-basic.png", "images/bird-parade-eagle.png"],
     adj: "big"
   },
   {referent: ["hummingbird", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/bird-parade-basic.png", "images/bird-parade-hummingbird.png"],
    adj: "small"
   }
  ],
  fish: [
    {referent: ["swordfish", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/fish-parade-basic.png", "images/fish-parade-swordfish.png"],
     adj: "big"
   },
   {referent: ["goldfish", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/fish-parade-basic.png", "images/fish-parade-goldfish.png"],
    adj: "small"
   }
  ],
  flowers: [
  {referent: ["sunflower", "one"],
   utterance1: utterance.utterance1b,
   utterance2: utterance.utterance2b,
   picture: ["images/flower-parade-basic.png", "images/flower-parade-sunflower.png"],
   adj: "big"
 },
 {referent: ["dandelion", "one"],
  utterance1: utterance.utterance1s,
  utterance2: utterance.utterance2s,
  picture: ["images/flower-parade-basic.png", "images/flower-parade-dandelion.png"],
  adj: "small"
 }],
  trees: [
    {referent: ["redwood", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/tree-parade-basic.png", "images/tree-parade-redwood.png"],
     adj: "big"
   },
   {referent: ["bonsai", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/tree-parade-basic.png", "images/tree-parade-bonsai.png"],
    adj: "small"
   }
  ]
}


// const utterance1 = utterance.question1
// const utterance2 = utterance.question2

const main_trials = {
    dogs1:   {
            context: "You and your friend see the following:",
            context_picture: items.dogs1[target_size[0]].picture[picture[0]],
            pic_spec: picture[0],
            text: "Your friend points to another member of the group which you cannot see and says: <br/><b>" + items.dogs1[target_size[0]].utterance1 + items.dogs1[target_size[0]].referent[referent[0]] + items.dogs1[target_size[0]].utterance2 + ".</b>",
            ref_spec: referent[0],
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.dogs1[target_size[0]].adj + " relative to other ",
            item: "dogs1",
            // if 0: big, if 1 small
            target_size: items.dogs1[target_size[0]].adj,
            condition: utterance.condition


        },
    dogs2:    {
            context: "You and your friend see the following:",
            context_picture: items.dogs2[target_size[1]].picture[picture[1]],
            pic_spec: picture[1],
            text: "Your friend points to another member of the group which you cannot see and says: <br/><b>" + items.dogs2[target_size[1]].utterance1 + items.dogs2[target_size[1]].referent[referent[1]] + items.dogs2[target_size[1]].utterance2 + ".</b>",
            ref_spec: referent[1],
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.dogs2[target_size[1]].adj + " relative to other ",
            item: "dogs2",
            target_size: items.dogs2[target_size[1]].adj,
            condition: utterance.condition

        },
    birds:    {
            context: "You and your friend see the following:",
            context_picture: items.birds[target_size[2]].picture[picture[2]],
            pic_spec: picture[2],
            text: "Your friend points to another member of the group which you cannot see and says: <br/><b> " + items.birds[target_size[2]].utterance1 + items.birds[target_size[2]].referent[referent[2]] + items.birds[target_size[2]].utterance2 + ".</b>",
            ref_spec: referent[2],
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.birds[target_size[2]].adj + " relative to other ",
            item: "birds",
            target_size: items.birds[target_size[2]].adj,
            condition: utterance.condition

        },
  fish:      {
            context: "You and your friend see the following:",
            context_picture: items.fish[target_size[3]].picture[picture[3]],
            pic_spec: picture[3],
            text: "Your friend points to another member of the group which you cannot see and says: <br/><b>" + items.fish[target_size[3]].utterance1 + items.fish[target_size[3]].referent[referent[3]] + items.fish[target_size[3]].utterance2 + ".</b>",
            ref_spec: referent[3],
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.fish[target_size[3]].adj + " relative to other ",
            item: "fish",
            target_size: items.fish[target_size[3]].adj,
            condition: utterance.condition

        },
  flowers:      {
            context: "You and your friend see the following:",
            context_picture: items.flowers[target_size[4]].picture[picture[4]],
            pic_spec: picture[4],
            text: "Your friend points to another member of the group which you cannot see and says: <br/><b>" + items.flowers[target_size[4]].utterance1 + items.flowers[target_size[4]].referent[referent[4]] + items.flowers[target_size[4]].utterance2 + ".</b>",
            ref_spec: referent[4],
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.flowers[target_size[4]].adj + " relative to other ",
            item: "flowers",
            target_size: items.flowers[target_size[4]].adj,
            condition: utterance.condition

        },
    trees:    {
            context: "You and your friend see the following:",
            context_picture: items.trees[target_size[5]].picture[picture[5]],
            pic_spec: picture[5],
            text: "Your friend points to another member of the group which you cannot see and says: <br/><b>" + items.trees[target_size[5]].utterance1 + items.trees[target_size[5]].referent[referent[5]] + items.trees[target_size[5]].utterance2 + ".</b>",
            ref_spec: referent[5],
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.trees[target_size[5]].adj + " relative to other ",
            item: "trees",
            target_size: items.trees[target_size[5]].adj,
            condition: utterance.condition

        }
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
  picture1: "warmup/sequoia.jpg",
  picture2: "warmup/bonsai.jpg",
  correct1: ["redwood", "sequoia"],
  correct2: ["bonsai"],
  correct3: ["trees"],
  text: "Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a ",
  question2: "These are both"
}
}

const trials = _.shuffle([ {x: warmup_trials.dogs1, y:main_trials.dogs1}, {x: warmup_trials.dogs2, y:main_trials.dogs2},
  {x:warmup_trials.birds, y:main_trials.birds},
  {x:warmup_trials.flowers, y:main_trials.flowers},
  {x:warmup_trials.fish, y:main_trials.fish},
  {x:warmup_trials.trees, y: main_trials.trees}])

  const trial_info = {

       text_insertion_main1: [

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
    ],
    text_insertion_warmup2: [
      trials[3].x,
      trials[4].x,
      trials[5].x
    ]
  };
