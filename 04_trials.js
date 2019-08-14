// In this file you can specify the trial data for your experiment
const utt_prenominal = {utterance1b: "That's a big ", utterance2b: "", utterance1s: "That's a small ", utterance2s: "", condition: "prenominal"}
const utt_predicative = {utterance1b: "That ", utterance2b: " is big", utterance1s: "That ", utterance2s: " is small", condition:"predicative"}
const size = function () {
  return _.sample([0,1])
}

const utterance = _.sample([utt_prenominal, utt_predicative]);
const target_size = _.shuffle([0,0,0,1,1,1])
const referent = _.shuffle([0,0,0,1,1,1])
const picture = _.shuffle([0,0,0,1,1,1])

const items = {
  dogs1: [
    {referent: ["doberman", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/dog-parade-basic.png", "images/dog-parade-doberman.png"],
     adj: "big",
     option1: "doberman"
   },
   {referent: ["chihuahua", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/dog-parade-basic.png", "images/dog-parade-chihuahua.png"],
    adj: "small",
    option1: "chihuahuas"
   }
  ],
  dogs2: [
    {referent: ["great dane", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/dog-parade-basic2.png", "images/dog-parade-great-dane.png"],
     adj: "big",
     option1: "great danes"
   },
   {referent: ["pug", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/dog-parade-basic2.png", "images/dog-parade-pug.png"],
    adj: "small",
    option1: "pugs"
   }
  ],
  birds: [
    {referent: ["eagle", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/bird-parade-basic.png", "images/bird-parade-eagle.png"],
     adj: "big",
     option1: "eagles"
   },
   {referent: ["hummingbird", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/bird-parade-basic.png", "images/bird-parade-hummingbird.png"],
    adj: "small",
    option1: "hummingbirds"
   }
  ],
  fish: [
    {referent: ["swordfish", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/fish-parade-basic.png", "images/fish-parade-swordfish.png"],
     adj: "big",
     option1: "swordfish"
   },
   {referent: ["goldfish", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/fish-parade-basic.png", "images/fish-parade-goldfish.png"],
    adj: "small",
    option1: "goldfish"
   }
  ],
  flowers: [
  {referent: ["sunflower", "one"],
   utterance1: utterance.utterance1b,
   utterance2: utterance.utterance2b,
   picture: ["images/flower-parade-basic.png", "images/flower-parade-sunflower.png"],
   adj: "big",
   option1: "sunflowers"
 },
 {referent: ["dandelion", "one"],
  utterance1: utterance.utterance1s,
  utterance2: utterance.utterance2s,
  picture: ["images/flower-parade-basic.png", "images/flower-parade-dandelion.png"],
  adj: "small",
  option1: "dandelia"
 }],
  trees: [
    {referent: ["redwood", "one"],
     utterance1: utterance.utterance1b,
     utterance2: utterance.utterance2b,
     picture: ["images/tree-parade-basic.png", "images/tree-parade-redwood.png"],
     adj: "big",
     option1: "redwoods"
   },
   {referent: ["bonsai", "one"],
    utterance1: utterance.utterance1s,
    utterance2: utterance.utterance2s,
    picture: ["images/tree-parade-basic.png", "images/tree-parade-bonsai.png"],
    adj: "small",
    option1: "bonsais"
   }
  ]
}


// const utterance1 = utterance.question1
// const utterance2 = utterance.question2

const trial_info = {
  forced_choice: [
        {
            context: "You and your friend see a parade of animals.",
            context_picture: items.dogs1[target_size[0]].picture[picture[0]],
            text: "Your friend points to another member of the parade which you cannot see and says: " + items.dogs1[target_size[0]].utterance1 + items.dogs1[target_size[0]].referent[referent[0]] + items.dogs1[target_size[0]].utterance2 + ".",
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.dogs1[target_size[0]].adj + " relative to other ",
            option1: items.dogs1[target_size[0]].option1,
            option2: 'dogs',
            item: "dogs1"

        },
        {
            context: "You and your friend see a parade of animals.",
            context_picture: items.dogs2[target_size[1]].picture[picture[1]],
            text: "Your friend points to another member of the parade which you cannot see and says: " + items.dogs2[target_size[1]].utterance1 + items.dogs2[target_size[1]].referent[referent[1]] + items.dogs2[target_size[1]].utterance2 + ".",
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.dogs2[target_size[1]].adj + " relative to other ",
            option1: items.dogs2[target_size[1]].option1,
            option2: 'dogs',
            item: "dogs2"

        },
        {
            context: "You and your friend see a parade of animals.",
            context_picture: items.birds[target_size[2]].picture[picture[2]],
            text: "Your friend points to another member of the parade which you cannot see and says: " + items.birds[target_size[2]].utterance1 + items.birds[target_size[2]].referent[referent[2]] + items.birds[target_size[2]].utterance2 + ".",
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.birds[target_size[2]].adj + " relative to other ",
            option1: items.birds[target_size[2]].option1,
            option2: 'birds',
            item: "birds"

        },
        {
            context: "You and your friend see a parade of animals.",
            context_picture: items.fish[target_size[3]].picture[picture[3]],
            text: "Your friend points to another member of the parade which you cannot see and says: " + items.fish[target_size[3]].utterance1 + items.fish[target_size[3]].referent[referent[3]] + items.fish[target_size[3]].utterance2 + ".",
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.fish[target_size[3]].adj + " relative to other ",
            option1: items.fish[target_size[3]].option1,
            option2: 'fish',
            item: "fish"

        },
        {
            context: "You and your friend see a collection of plants.",
            context_picture: items.flowers[target_size[4]].picture[picture[4]],
            text: "Your friend points to another member of the collection which you cannot see and says: " + items.flowers[target_size[4]].utterance1 + items.flowers[target_size[4]].referent[referent[4]] + items.flowers[target_size[4]].utterance2 + ".",
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.flowers[target_size[4]].adj + " relative to other ",
            option1: items.flowers[target_size[4]].option1,
            option2: 'flowers',
            item: "flowers"

        },
        {
            context: "You and your friend see a collection of plants.",
            context_picture: items.trees[target_size[5]].picture[picture[5]],
            text: "Your friend points to another member of the collection which you cannot see and says: " + items.trees[target_size[5]].utterance1 + items.trees[target_size[5]].referent[referent[5]] + items.trees[target_size[5]].utterance2 + ".",
            question: "Please rephrase your friend's comment.",
            sentence_left: "It is " + items.trees[target_size[5]].adj + " relative to other ",
            option1: items.trees[target_size[5]].option1,
            option2: 'trees',
            item: "trees"

        }

  ]
};
