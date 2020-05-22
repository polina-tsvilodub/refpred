// In this file trial data and some functions for trial data creation are specified

// half of the trials with big (0), half of trials with small (1) targets

// 4 trials with basic-level np (0), 4 trials with subordinate np (1), 4 trials with 'one' (2) (position in referent-list)
// const referent = _.shuffle([0,0,0,0, 1,1,1,1, 2,2,2,2])

// 6 trials with predicate syntax (0), six trials with subject syntax (1)
// const syntactic_cond = _.shuffle([0,0,0,1,1,1])

const target_size = _.shuffle([0,0,0,1,1,1])

// const syntax = ["subject", "predicate"]

const contexts = _.shuffle(["dogs1", "dogs2", "birds", "flowers", "trees"]) // , "fish"
console.log(contexts)
const synt_adj0 = _.shuffle(["congr_subj", "congr_pred", "congr_subj", "congr_pred"]);
console.log(synt_adj0)
// console.log(synt_adj)
const filler_cond = _.shuffle(["congr_pred", "congr_pred", "congr_subj", "congr_subj"]);
console.log("Filler conditions:", filler_cond)
///////////////////////////
//   adjust if needed
const num_trials = 4
//////////////////////////


// creating views with all the necessary information
function create_view(items, target_size, contexts, num_trials, synt_adj0, filler_cond ) {
  const expt_views = []
  // the iterator iterates over all the contexts and takes one target per context (either big or small)
  for ( i = 0; i < num_trials; i ++) {

    const view = {
      trial_type: "critical",
      context: items[contexts[i]][target_size[i]].context_sent + "and you see the following:", // target_size indicates if the target is big or small within the given context
      context_picture: items[contexts[i]][target_size[i]].context_picture, // context picture is chose (it is the same for both big and small targets)
      text: "Your friend goes ahead of you. You see your friend in the distance:", // text appearing above the target picture
      target_picture: items[contexts[i]][target_size[i]].target, // target picture, differs for bis and small target
      item: contexts[i],
      target: items[contexts[i]][target_size[i]].item,
      target_size: items[contexts[i]][target_size[i]].adj_congr,
      ref_np: items[contexts[i]][target_size[i]].reference
    }
    // modify utterance etc depending on specific condition
    if (synt_adj0[i].split("_")[0] == "congr") {

      view.adj = items[contexts[i]][target_size[i]].adj_congr;
      view.adj_cond = "congruent";

      if (synt_adj0[i].split("_")[1] == "subj") {
        view.syntax = "subj";
        view.utterance = "Your friend says: <br/><b>" + "That " + items[contexts[i]][target_size[i]].adj_congr + " " + items[contexts[i]][target_size[i]].item + " is a " + items[contexts[i]][target_size[i]].reference + ".</b>";
        view.question = "What do you think your friend is saying is it " + items[contexts[i]][target_size[i]].adj_congr + " relative to?";
        view.paraphrase = "It is " + items[contexts[i]][target_size[i]].adj_congr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        view.syntax = "pred";
        view.utterance = "Your friend says: <br/><b>" + "That " + items[contexts[i]][target_size[i]].reference + " is a "  + items[contexts[i]][target_size[i]].adj_congr + " " + items[contexts[i]][target_size[i]].item + ".</b>";
        view.question = "What do you think your friend is saying is it " + items[contexts[i]][target_size[i]].adj_congr + " relative to?";
        view.paraphrase = "It is " + items[contexts[i]][target_size[i]].adj_congr + " relative to other " ;// paraphrase template
      }

    } else { // incongruent condition
      view.adj = items[contexts[i]][target_size[i]].adj_incongr;
      view.adj_cond = "incongruent";
      if (synt_adj0[i].split("_")[1] == "subj") {
        view.syntax = "subj";
        view.utterance = "Your friend says: <br/><b>" +"That " + items[contexts[i]][target_size[i]].adj_incongr + " " + items[contexts[i]][target_size[i]].item + " is a " + items[contexts[i]][target_size[i]].reference + ".</b>";
        view.question = "What do you think your friend is saying is it " + items[contexts[i]][target_size[i]].adj_incongr + " relative to?";
        view.paraphrase = "It is " + items[contexts[i]][target_size[i]].adj_incongr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        view.syntax = "pred";
        view.utterance = "Your friend says: <br/><b>" + "That " + items[contexts[i]][target_size[i]].reference + " is a "  + items[contexts[i]][target_size[i]].adj_incongr + " " + items[contexts[i]][target_size[i]].item + ".</b>";
        view.question = "What do you think your friend is saying is it " + items[contexts[i]][target_size[i]].adj_incongr + " relative to?";
        view.paraphrase = "It is " + items[contexts[i]][target_size[i]].adj_incongr + " relative to other " ;// paraphrase template
      }
    }
    // end of critical view creation
    expt_views.push(view);

    // filler view
    function check_size(size_var) {
      if(size_var == 0) {
        return 1
      } else {
        return 0
      }
    }

    const filler_size = check_size(target_size[i])
    const filler = {
      trial_type: "filler",
      context: "You and your friend see the following:", // target_size indicates if the target is big or small within the given context
      context_picture: items[contexts[i]][filler_size].context_picture_filler, // context picture is chose (it is the same for both big and small targets)
      text: "Your friend goes ahead of you. You see your friend in the distance:", // text appearing above the target picture
      target_picture: items[contexts[i]][filler_size].target_filler, // target picture, differs for bis and small target
      item: contexts[i],
      target: items[contexts[i]][filler_size].item,
      target_size: items[contexts[i]][filler_size].adj_congr,
      ref_np: items[contexts[i]][filler_size].reference
    }

    if (filler_cond[i].split("_")[0] == "congr") {

      filler.adj = items[contexts[i]][filler_size].adj_congr;
      filler.adj_cond = "congruent";

      if (filler_cond[i].split("_")[1] == "subj") {
        filler.syntax = "subj";
        filler.utterance = "Your friend says: <br/><b>" + "That "  + items[contexts[i]][filler_size].item + " is " + items[contexts[i]][filler_size].adj_congr + ".</b>";
        filler.question = "What do you think your friend is saying is it " + items[contexts[i]][filler_size].adj_congr + " relative to?";
        filler.paraphrase = "It is " + items[contexts[i]][filler_size].adj_congr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        filler.syntax = "pred";
        filler.utterance = "Your friend says: <br/><b>" + "That's a "  + items[contexts[i]][filler_size].adj_congr + " " + items[contexts[i]][filler_size].item + ".</b>";
        filler.question = "What do you think your friend is saying is it " + items[contexts[i]][filler_size].adj_congr + " relative to?";
        filler.paraphrase = "It is " + items[contexts[i]][filler_size].adj_congr + " relative to other " ;// paraphrase template
      }

    } else { // incongruent condition
      filler.adj = items[contexts[i]][filler_size].adj_incongr;
      filler.adj_cond = "incongruent";
      if (filler_cond[i].split("_")[1] == "subj") {
        filler.syntax = "subj";
        filler.utterance = "Your friend says: <br/><b>" +"That " + items[contexts[i]][filler_size].item + " is " + items[contexts[i]][filler_size].adj_incongr + ".</b>";
        filler.question = "What do you think your friend is saying is it " + items[contexts[i]][filler_size].adj_incongr + " relative to?";
        filler.paraphrase = "It is " + items[contexts[i]][filler_size].adj_incongr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        filler.syntax = "pred";
        filler.utterance = "Your friend says: <br/><b>" + "That's a "  + items[contexts[i]][filler_size].adj_incongr + " " + items[contexts[i]][filler_size].item + ".</b>";
        filler.question = "What do you think your friend is saying is it " + items[contexts[i]][filler_size].adj_incongr + " relative to?";
        filler.paraphrase = "It is " + items[contexts[i]][filler_size].adj_incongr + " relative to other " ;// paraphrase template
      }
    }
    // end filler creation
    expt_views.push(filler);

  }  // end for

      return expt_views;

} // end function



const items = {
  // "_b" are the big referents
  dogs1: [
  // first set contains the basic-level context
    {
     item: "doberman",
     context_sent: "You and your friend are at an animal training ground ",
     context_picture: "images/dog-parade-basic.png",
     context_picture_filler: "images/dog-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/doberman.png",
     target_filler: "images/doberman_filler.png",
     // first array is subject  , second is predicate
     reference: "service animal" // critical utterances
   },
   // second set contains the subordinate context
   {
    item: "chihuahua",
    context_sent: "You and your friend are at an animal training ground ",
    context_picture: "images/dog-parade-basic.png",
    context_picture_filler: "images/dog-parade-basic_filler.png",
    adj_congr: "small",
    adj_incongr: "big",
    target: "images/chihuahua.png",
    target_filler: "images/chihuahua_filler.png",
    // first array are utterances in predicate, second is subject utterances
    reference: "service animal"
    }
  ],
  // "_s" are the small referents

  dogs2: [
    {
     item: "Great Dane",
     context_sent: "You and your friend are at a pet show ",
     context_picture: "images/dog-parade-basic2.png",
     context_picture_filler: "images/dog-parade-basic2_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/great-dane.png",
     target_filler: "images/great-dane_filler.png",
     reference: "prize-winner"
   },
   {
    item: "pug",
    context_sent: "You and your friend are at a pet show ",
    context_picture: "images/dog-parade-basic2.png",
    context_picture_filler: "images/dog-parade-basic2_filler.png",
    adj_congr: "small",
    adj_incongr: "big",
    target: "images/pug.png",
    target_filler: "images/pug_filler.png",
    reference: "prize-winner"
   }
  ],
  birds: [
    {
     item: "eagle",
     context_sent: "You visit your friend who works at an animal shelter ",
     context_picture: "images/bird-parade-basic.png",
     context_picture_filler: "images/bird-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/eagle.png",
     target_filler: "images/eagle_filler.png",
     reference: "rescue"
   },
   {
    item: "hummingbird",
    context_sent: "You visit your friend who works at an animal shelter ",
    context_picture: "images/bird-parade-basic.png",
    context_picture_filler: "images/bird-parade-basic_filler.png",
    adj_congr: "small",
    adj_incongr: "big",
    target: "images/hummingbird.png",
    target_filler: "images/hummingbird_filler.png",
    reference: "rescue"
   }
  ],
  // fish: [
  //   {
  //    item: "tuna",
  //    context_sent: "You and your friend are at a fish market ",
  //    context_picture: "images/fish-parade-basic.png",
  //    adj_congr: "big",
  //    adj_incongr: "small",
  //    target: "images/tuna_net.png",
  //    utterances: ["That big tuna is a gem", "That gem is a big tuna"],
  //    reference: "gem"
  //  },
  //  {
  //   item: "clownfish",
  //   context_sent: "You and your friend are at a fish market ",
  //   context_picture: "images/fish-parade-basic.png",
  //   adj_congr: "small",
  //   adj_incongr: "big",
  //   target: "images/clownfish_net.png",
  //   utterances: ["That small clownfish is a gem", "That gem is a small clownfish"],
  //   reference: "gem"
  //  }
  // ],
  flowers: [
  {
   item: "sunflower",
   context_sent: "You and your friend are at their garden ",
   context_picture: "images/flower-parade-basic.png",
   context_picture_filler: "images/flower-parade-basic_filler.png",
   adj_congr: "big",
   adj_incongr: "small",
   target: "images/sunflower.png",
   target_filler: "images/sunflower_filler.png",
   reference: "gift"
 },
  {
   item: "dandelion",
   context_sent: "You and your friend are at their garden ",
   context_picture: "images/flower-parade-basic.png",
   context_picture_filler: "images/flower-parade-basic_filler.png",
   adj_congr: "small",
   adj_incongr: "big",
   target: "images/dandelion.png",
   target_filler: "images/dandelion_filler.png",
   reference: "gift"
  }
 ],
  trees: [
    {
     item: "redwood",
     context_sent: "You and your friend walk to your friend's cabin in a park for the first time. You want to memorize the path ",
     context_picture: "images/tree-parade-basic.png",
     context_picture_filler: "images/tree-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/redwood_sign.png",
     target_filler: "images/redwood_filler.png",
     reference: "landmark"
   },
  {
   item: "bonsai",
   context_sent:  "You and your friend walk to your friend's cabin in a park for the first time. You want to memorize the path ",
   context_picture: "images/tree-parade-basic.png",
   context_picture_filler: "images/tree-parade-basic_filler.png",
   adj_congr: "small",
   adj_incongr: "big",
   target: "images/bonsai_stick.png",
   target_filler: "images/bonsai_filler.png",
   reference: "landmark"
 }
 ]
}

// warm-up trial information
// a warm-up block contains the same targets that the following main block contains
const warmup_trials = {
  dogs1: {
    train: {
    item: "dogs1",
    picture1: "warmup/chihuahua1.png",
    picture2: "warmup/doberman1.png",
    text: "This one is done for you.",
    question1: "This is a chihuahua.",
    question3: "This is a doberman."
   },
  label: {
    item: "dogs1",
    picture1: "warmup/chihuahua2.png",
    picture2: "warmup/doberman2.png",
    correct1: ["chihuahua"], // correct labels for the feedback
    correct2: ["doberman"],
    text: "Your turn! Please label the pictures below.",
    question1: "This is a ",
    question3: "This is a "
  },
  ref: {
    item: "dogs1",
    picture1: "warmup/beagle_service.png",
    picture2: "warmup/doberman_service.png",
    question: " <br> These are service animals."
  }
},
dogs2: {
  train: {
  item: "dogs2",
  picture1: "warmup/pug1.png",
  picture2: "warmup/great-dane1.png",
  default1: ["pug"],
  default2: ["great dane"],
  text: "This one is done for you.",
  question1: "This is a pug.",
  question3: "This is a Great Dane."
  },
  label: {
  item: "dogs2",
  picture1: "warmup/pug2.png",
  picture2: "warmup/great-dane2.png",
  correct1: ["pug"],
  correct2: ["great dane"],
  text: "Your turn! Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a "
},
ref: {
  item: "dogs2",
  picture1: "warmup/pug_prize.png",
  picture2: "warmup/great-dane_prize.png",
  question:" <br>These are prize-winners."
}
},
birds: {
  train: {
  item: "birds",
  picture1: "warmup/hummingbird1.png",
  picture2: "warmup/eagle1.png",
  default1: ["hummingbird"],
  default2: ["eagle"],
  text: "This one is done for you.",
  question1: "This is a hummingbird.",
  question3: "This is an eagle."
  },
  label: {
  item: "birds",
  picture1: "warmup/hummingbird2.png",
  picture2: "warmup/eagle2.png",
  correct1: ["hummingbird"],
  correct2: ["eagle"],
  text: "Your turn! Please label the pictures below.",
  question1: "This is a ",
  question3: "This is an "
},
ref: {
  item: "birds",
  picture1: "warmup/hummingbird_rescue.png",
  picture2: "warmup/eagle_rescue.png",
  question: " <br> These are rescues."
}
},
flowers: {
  train: {
  item: "flowers",
  picture1: "warmup/dandelion1.png",
  picture2: "warmup/sunflower1.png",
  default1: ["dandelion"],
  default2: ["sunflower"],
  text: "This one is done for you.",
  question1: "This is a dandelion.",
  question3: "This is a sunflower."
  },
  label: {
  item: "flowers",
  picture1: "warmup/dandelion2.png",
  picture2: "warmup/sunflower2.png",
  correct1: ["dandelion"],
  correct2: ["sunflower"],
  text: "Your turn! Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a "
},
ref: {
  item: "flowers",
  picture1: "warmup/dandelion_gift.png",
  picture2: "warmup/sunflower_gift.png",
  question: " <br> These are gifts."
}
},
// fish: {
//   train: {
//   item: "fish",
//   picture1: "warmup/swordfish1.png",
//   picture2: "warmup/goldfish1.png",
//   default1: ["swordfish"],
//   default2: ["goldfish"],
//   text: "This one is done for you.",
//   question1: "This is a ",
//   question3: "This is a "
//   },
//   label:  {
//   item: "fish",
//   picture1: "warmup/swordfish2.png",
//   picture2: "warmup/goldfish2.png",
//   correct1: ["swordfish"],
//   correct2: ["goldfish"],
//   text: "Your turn! Please label the pictures below.",
//   question1: "This is a ",
//   question3: "This is a "
//   }
// },
trees: {
  train: {
  item: "trees",
  picture1: "warmup/redwood1.png",
  picture2: "warmup/bonsai1.png",
  default1: ["redwood"],
  default2: ["bonsai"],
  text: "This one is done for you.",
  question1: "This is a redwood.",
  question3: "This is a bonsai."
  },
  label: {
  item: "trees",
  picture1: "warmup/redwood2.png",
  picture2: "warmup/bonsai2.png",
  correct1: "redwood or sequoia (choose one)",
  correct2: ["bonsai"],
  text: "Your turn! Please label the pictures below.",
  question1: "This is a ",
  question3: "This is a "
},
ref: {
  item: "trees",
  picture1: "warmup/redwood_landmark.png",
  picture2: "warmup/tree_landmark.png",
  question: " <br> These are landmarks."
}
}
}

const main_trials = create_view(items, target_size, contexts, num_trials, synt_adj0, filler_cond)

// shuffle sets of warmup trials and the corresponding big and small targets
 // x: warmup trial
 // y: big target
 // z: small target
 // the trials are already shuffled in the context list

const trials = [
  {x:warmup_trials[contexts[0]], y:main_trials[0], z:main_trials[1]},
  {x:warmup_trials[contexts[1]], y:main_trials[2], z:main_trials[3]},
  {x:warmup_trials[contexts[2]], y:main_trials[4], z:main_trials[5]},
  {x:warmup_trials[contexts[3]], y:main_trials[6], z:main_trials[7]},
  //{x:warmup_trials[contexts[4]], y:main_trials[], z:main_trials[0]},
//  {x:warmup_trials[contexts[5]], y:main_trials[5]}
]

  const trial_info = {

     text_insertion_main1: [
// get three items for the first main trial block

       trials[0].y,
       trials[1].y,
       trials[0].z,
       trials[1].z
    ],
    // get items for the second block
    text_insertion_main2 :[

      trials[2].y,
      trials[3].y,
      trials[2].z,
      trials[3].z
    ],
     text_insertion_warmup1: [
       // get the warmup trials corresponding to the main trials in the first main block
       trials[0].x,
       trials[1].x
       // trials[2].x
    ],
    text_insertion_warmup2: [
      // warm-up trials for second block
      trials[2].x,
      trials[3].x
  //    trials[5].x
    ],
    ref_warmup1: [
      trials[0].x,
      trials[1].x
    ],
    ref_warmup2: [
      trials[2].x,
      trials[3].x
    ]


  };

console.log("Unshuffled trial order:", trial_info.text_insertion_main1)
