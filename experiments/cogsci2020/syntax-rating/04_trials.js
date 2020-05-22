// In this file you can specify the trial data for your experiment
const slider_warmup = {
  warmup1: [
  {text: "Imagine you see this basketball.",
  target: "warmup/basketball.png",
  question: "How well does each of the sentences describe it? (Click on the slider to provide a rating)",
  sentence1: "The basketball is orange.",
  option1: "very bad",
  option2: "very well",
  sentence2: "The basketball is green."
  },
  {text: "Imagine you see this chair.",
  target: "warmup/purple-chair.png",
  question: "How well does each of the sentences describe it? (Click on the slider to provide a rating)",
  sentence1: "The chair is yellow.",
  option1: "very bad",
  option2: "very well",
  sentence2: "The chair is blue."}]
 }

const utt_prenominal = {utterance1b: "That's a ", utterance2b: " ", utterance1s: "That's a ", utterance2s: " ", condition: "prenominal"}
const utt_predicative = {utterance1b: "That ", utterance2b: " is ", utterance1s: "That ", utterance2s: " is ", condition:"predicative"}

const possible_syntax = ["predicate", "subject"]
const syntax = _.shuffle([0,1]);
const np_spec = _.shuffle([0, 0, 0, 1, 1, 1]);
const adj_congr = _.shuffle([0, 0, 0, 0, 0, 0]);
const size = _.shuffle([0, 0, 0, 1, 1, 1]);
const items = {
  // all the context items
  dogs1: [
    // big target
          {np: ["doberman", "dog"], // sub and basic NP
           np_cond: ["sub", "basic"],  // NP type for recording
           adj: ["big", "small"], // congruent, incongruent adjectives
           utterance1l: utt_prenominal.utterance1b, // utterance parts
           utterance1r: utt_prenominal.utterance2b,
           utterance2l: utt_predicative.utterance1b,
           utterance2r: utt_predicative.utterance2b,
           picture: "images/dog-parade-basic.png", // context picture
           target: "warmup/doberman.png" // target picture
           },
           // small target
           {
             np: ["chihuahua", "dog"],
             np_cond: ["sub", "basic"],
             adj: ["small", "big"],
             utterance1l: utt_prenominal.utterance1s,
             utterance1r: utt_prenominal.utterance2s,
             utterance2l: utt_predicative.utterance1s,
             utterance2r: utt_predicative.utterance2s,
             picture: "images/dog-parade-basic.png",
             target: "warmup/chihuahua.jpg"
           }
         ],
  dogs2:  [{np: ["great dane", "dog"],
            np_cond: ["sub", "basic"],
            adj: ["big", "small"],
            utterance1l: utt_prenominal.utterance1b,
            utterance1r: utt_prenominal.utterance2b,
            utterance2l: utt_predicative.utterance1b,
            utterance2r: utt_predicative.utterance2b,
            picture: "images/dog-parade-basic2.png",
            target: "warmup/great-dane.jpg"
            },
            {
              np: ["pug", "dog"],
              np_cond: ["sub", "basic"],
              adj: ["small", "big"],
              utterance1l: utt_prenominal.utterance1s,
              utterance1r: utt_prenominal.utterance2s,
              utterance2l: utt_predicative.utterance1s,
              utterance2r: utt_predicative.utterance2s,
              picture: "images/dog-parade-basic2.png",
              target: "warmup/pug.jpg"
            }
        ],

  birds: [  {np: ["eagle", "bird"],
             np_cond: ["sub", "basic"],
             adj: ["big", "small"],
             utterance1l: utt_prenominal.utterance1b,
             utterance1r: utt_prenominal.utterance2b,
             utterance2l: utt_predicative.utterance1b,
             utterance2r: utt_predicative.utterance2b,
             picture: "images/bird-parade-basic.png",
             target: "warmup/eagle.jpg",
           },
           {
             np: ["hummingbird", "bird"],
             np_cond: ["sub", "basic"],
             adj: ["small", "big"],
             utterance1l: utt_prenominal.utterance1s,
             utterance1r: utt_prenominal.utterance2s,
             utterance2l: utt_predicative.utterance1s,
             utterance2r: utt_predicative.utterance2s,
             picture: "images/bird-parade-basic.png",
             target: "warmup/colibri.jpg"
           }
 ],
  fish:   [ {np: ["swordfish", "fish"],
             np_cond: ["sub", "basic"],
             adj: ["big", "small"],
             utterance1l: utt_prenominal.utterance1b,
             utterance1r: utt_prenominal.utterance2b,
             utterance2l: utt_predicative.utterance1b,
             utterance2r: utt_predicative.utterance2b,
             picture: "images/fish-parade-basic.png",
             target: "warmup/swordfish.jpg",
           },
           {
             np: ["goldfish", "fish"],
             np_cond: ["sub", "basic"],
             adj: ["small", "big"],
             utterance1l: utt_prenominal.utterance1s,
             utterance1r: utt_prenominal.utterance2s,
             utterance2l: utt_predicative.utterance1s,
             utterance2r: utt_predicative.utterance2s,
             picture: "images/fish-parade-basic.png",
             target: "warmup/goldfish.png"
           }
          ],
  flowers:  [{np: ["sunflower", "flower"],
              np_cond: ["sub", "basic"],
              adj: ["big", "small"],
              utterance1l: utt_prenominal.utterance1b,
              utterance1r: utt_prenominal.utterance2b,
              utterance2l: utt_predicative.utterance1b,
              utterance2r: utt_predicative.utterance2b,
              picture: "images/flower-parade-basic.png",
              target: "warmup/sunflower.png",
            },
            {
              np: ["dandelion", "flower"],
              np_cond: ["sub", "basic"],
              adj: ["small", "big"],
              utterance1l: utt_prenominal.utterance1s,
              utterance1r: utt_prenominal.utterance2s,
              utterance2l: utt_predicative.utterance1s,
              utterance2r: utt_predicative.utterance2s,
              picture: "images/flower-parade-basic.png",
              target: "warmup/dandelion.jpg"
            }
          ],
  trees:   [{np: ["sequoia", "tree"],
             np_cond: ["sub", "basic"],
             adj: ["big", "small"],
             utterance1l: utt_prenominal.utterance1b,
             utterance1r: utt_prenominal.utterance2b,
             utterance2l: utt_predicative.utterance1b,
             utterance2r: utt_predicative.utterance2b,
             picture: "images/tree-parade-basic.png",
             target: "warmup/sequoia.png",
           },
           {
             np: ["bonsai", "tree"],
             np_cond: ["sub", "basic"],
             adj: ["small", "big"],
             utterance1l: utt_prenominal.utterance1s,
             utterance1r: utt_prenominal.utterance2s,
             utterance2l: utt_predicative.utterance1s,
             utterance2r: utt_predicative.utterance2s,
             picture: "images/tree-parade-basic.png",
             target: "warmup/bonsai.jpg"
           }
         ]
}

const main_trials = {
    dogs1:   {
            context: "You and your friend see the following:",
            np_condition: items.dogs1[size[0]].np_cond[np_spec[0]], // record the NP condition
            context_picture: items.dogs1[size[0]].picture, //  context picture
            text: "You also see this " + items.dogs1[size[0]].np[0] + "." , // create text with subordinate NP
            target: items.dogs1[size[0]].target, // target picture
            question: "How well does each of these sentences describe it? (Click on the slider to provide a rating)",
            domain: "dogs1", // context item
            item: items.dogs1[size[0]].np[0], // record target
            option1: "very bad", // slider labels
            option2: "very well",
            // correct utterances are created
            //first is predicate, second is subject condition
            sentences: ["<b>" + items.dogs1[size[0]].utterance1l + items.dogs1[size[0]].adj[adj_congr[0]] + items.dogs1[size[0]].utterance1r + items.dogs1[size[0]].np[np_spec[0]] + ".</b>",
            "<b>" + items.dogs1[size[0]].utterance2l + items.dogs1[size[0]].np[np_spec[0]] + items.dogs1[size[0]].utterance2r + items.dogs1[size[0]].adj[adj_congr[0]] + ".</b>"],
            // record target size
            target_size: items.dogs1[size[0]].adj[0],
            // adjective congruence: 0 congruent, 1 incongruent
            congruence: adj_congr[0],
            // numeric syntactic condition order
            sentence_order: syntax
        },
    dogs2:    {
            context: "You and your friend see the following:",
            np_condition: items.dogs2[size[1]].np_cond[np_spec[1]],
            context_picture: items.dogs2[size[1]].picture,
            text: "You also see this " + items.dogs2[size[1]].np[0] + "."  ,
            target: items.dogs2[size[1]].target,
            question: "How well does each of these sentences describe it? (Click on the slider to provide a rating)",
            domain: "dogs2",
            item: items.dogs2[size[1]].np[0],
            option1: "very bad",
            option2: "very well",
            sentences: ["<b>" + items.dogs2[size[1]].utterance1l + items.dogs2[size[1]].adj[adj_congr[1]] + items.dogs2[size[1]].utterance1r + items.dogs2[size[1]].np[np_spec[1]] + ".</b>",
            "<b>" + items.dogs2[size[1]].utterance2l + items.dogs2[size[1]].np[np_spec[1]] + items.dogs2[size[1]].utterance2r + items.dogs2[size[1]].adj[adj_congr[1]] + ".</b>"],
            target_size: items.dogs2[size[1]].adj[0],
            // 0 congruent, 1 incongruent
            congruence: adj_congr[1],
            sentence_order: syntax
        },
    birds:    {
            context: "You and your friend see the following:",
            np_condition : items.birds[size[2]].np_cond[np_spec[2]],
            context_picture: items.birds[size[2]].picture,
            text: "You also see this " + items.birds[size[2]].np[0] + "." ,
            target: items.birds[size[2]].target,
            question: "How well does each of these sentences describe it? (Click on the slider to provide a rating)",
            domain: "birds",
            item: items.birds[size[2]].np[0],
            option1: "very bad",
            option2: "very well",
            sentences: ["<b>" + items.birds[size[2]].utterance1l + items.birds[size[2]].adj[adj_congr[2]] + items.birds[size[2]].utterance1r + items.birds[size[2]].np[np_spec[2]] + ".</b>",
            "<b>" + items.birds[size[2]].utterance2l + items.birds[size[2]].np[np_spec[2]] + items.birds[size[2]].utterance2r + items.birds[size[2]].adj[adj_congr[2]] + ".</b>"],
            target_size: items.birds[size[2]].adj[0],
            // 0 congruent, 1 incongruent
            congruence: adj_congr[2],
            sentence_order: syntax
        },
  fish:      {
            context: "You and your friend see the following:",
            np_condition: items.fish[size[3]].np_cond[np_spec[3]],
            context_picture: items.fish[size[3]].picture,
            text: "You also see this " + items.fish[size[3]].np[0] + ".",
            target: items.fish[size[3]].target,
            question: "How well does each of these sentences describe it? (Click on the slider to provide a rating)",
            domain: "fish",
            item: items.fish[size[3]].np[0],
            option1: "very bad",
            option2: "very well",
            sentences: ["<b>" + items.fish[size[3]].utterance1l + items.fish[size[3]].adj[adj_congr[3]] + items.fish[size[3]].utterance1r + items.fish[size[3]].np[np_spec[3]] + ".</b>",
            "<b>" + items.fish[size[3]].utterance2l + items.fish[size[3]].np[np_spec[3]] + items.fish[size[3]].utterance2r + items.fish[size[3]].adj[adj_congr[3]] + ".</b>"],
            target_size: items.fish[size[3]].adj[0],
            // 0 congruent, 1 incongruent
            congruence: adj_congr[3],
            sentence_order: syntax
        },
    flowers:      {
            context: "You and your friend see the following:",
            np_condition: items.flowers[size[4]].np_cond[np_spec[4]],
            context_picture: items.flowers[size[4]].picture,
            text: "You also see this " + items.flowers[size[4]].np[0] + "." ,
            target: items.flowers[size[4]].target,
            question: "How well does each of these sentences describe it? (Click on the slider to provide a rating)",
            domain: "flowers",
            item: items.flowers[size[4]].np[0],
            option1: "very bad",
            option2: "very well",
            sentences: ["<b>" + items.flowers[size[4]].utterance1l + items.flowers[size[4]].adj[adj_congr[4]] + items.flowers[size[4]].utterance1r + items.flowers[size[4]].np[np_spec[4]] + ".</b>",
            "<b>" + items.flowers[size[4]].utterance2l + items.flowers[size[4]].np[np_spec[4]] + items.flowers[size[4]].utterance2r + items.flowers[size[4]].adj[adj_congr[4]] + ".</b>" ] ,
            target_size: items.flowers[size[4]].adj[0],
            congruence: adj_congr[4],
            sentence_order: syntax
        },
    trees:    {
            context: "You and your friend see the following:",
            np_condition: items.trees[size[5]].np_cond[np_spec[5]],
            context_picture: items.trees[size[5]].picture,
            text: "You also see this " + items.trees[size[5]].np[0] + ".",
            target: items.trees[size[5]].target,
            question: "How well does each of these sentences describe it? (Click on the slider to provide a rating)",
            domain: "trees",
            item: items.trees[size[5]].np[0],
            option1: "very bad",
            option2: "very well",
            sentences: ["<b>" + items.trees[size[5]].utterance1l + items.trees[size[5]].adj[adj_congr[5]] + items.trees[size[5]].utterance1r + items.trees[size[5]].np[np_spec[5]] + ".</b>",
            "<b>" + items.trees[size[5]].utterance2l + items.trees[size[5]].np[np_spec[5]] + items.trees[size[5]].utterance2r + items.trees[size[5]].adj[adj_congr[5]] + ".</b>"],
            target_size: items.trees[size[5]].adj[0],
            // 0 congruent, 1 incongruent
            congruence: adj_congr[5],
            sentence_order: syntax
    }
};


const trial_info = {
  main: _.shuffle([ main_trials.dogs1, main_trials.dogs2,
                    main_trials.birds, main_trials.flowers,
                    main_trials.fish, main_trials.trees])
}
