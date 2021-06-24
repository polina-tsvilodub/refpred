// In this file trial data and some functions for trial data creation are specified

///////////////////////////
// number of items used (up to 5 possible)
// total number of main trials is then 2 x num_trials
//   adjust if needed
const num_trials = 8//10
//////////////////////////

// order of forced-choice options that is shuffled between-subjects
// 0 is basic label, 1 is subordinate label; whatever comes first in the list appears as the left option
const fc = _.shuffle([0,1]);
console.log("FC order:", fc);

const noun_item_list = {
  service: ["dogs1"],
  prize: ["dogs2"],
  landmark: ["trees", "buildings", "flowers"],
  present: ["flowers"], // , "dogs2"
  rescue: ["dogs1", "fish", "birds"],
  stray: ["dogs2"]
}

// sample four our of five critical nouns with corresponding items, and make sure items are used =< 1 time
function get_items(noun_item_list, num_trials){
  // get four nouns

  const nouns = _.shuffle(Object.keys(noun_item_list)).slice(0, num_trials);
  // console.log("nouns:", nouns);
  var items = [];
  // check if there are items which occur twice
  while (_.uniq(items).length < num_trials) {
  // get items for the nouns
      var new_items = [];
      for(i = 0; i < num_trials; i++) {
        // grab one random item possible for a noun
        var item = noun_item_list[nouns[i]][Math.floor(Math.random() * noun_item_list[nouns[i]].length)];
        new_items.push(item);
      };
      // console.log("items:", new_items);
        // if all items unique, return item-noun pairs list (of the form ["item1_noun1", "item2_noun2"...])
      var items_list = _.flatten(_.map(_.zip(new_items, nouns), function([a, b]){return a.concat("_", b);}));
      // console.log(items_list);
      items = new_items;
    }
    return items_list;
}

// get item-noun pairs used for a participant
// const item_noun_pairs = get_items(noun_item_list, num_trials);
const item_noun_pairs = _.shuffle(["dogs1_service", "dogs2_prize", "trees_landmark", "buildings_landmark", "flowers_landmark", "flowers_present", "dogs2_stray", "dogs1_rescue", "fish_rescue", "birds_rescue"]);
console.log("Item-noun pairs:", item_noun_pairs)
console.log(item_noun_pairs.length)
////////////////////////
// based on the item-nouns, create single trials


// half of the trials with big (0), half of trials with small (1) targets

// const target_size = _.shuffle([0,0,0,1,1,1])

// const contexts = _.shuffle(["dogs1", "dogs2", "birds", "flowers", "trees"]) // , "fish"
// console.log(contexts)
/////
// for 3 items, the big target is in the subject condition, for the other 3 the small target is in the subject condition. the second item is in predicate condition, respectively
const synt_adj0 = _.shuffle(["congr_subj_0", "congr_subj_0", "congr_pred_0", "congr_pred_0", "congr_subj_1", "congr_subj_1", "congr_pred_1", "congr_pred_1"]); // "congr_subj_0", "congr_subj_0", , "congr_subj_1", "congr_subj_1"
console.log(synt_adj0)
// console.log(synt_adj)
const filler_cond = _.shuffle(["congr_pred", "congr_pred", "congr_pred", "congr_pred", "congr_subj", "congr_subj", "congr_subj", "congr_subj"]);
// console.log("Filler conditions:", filler_cond)

// creating views with all the necessary information
function create_view(items, contexts, num_trials, synt_adj0, filler_cond, fc ) {
  const expt_views = []
  // the iterator iterates over all the contexts and takes one target per context (either big or small)
  for ( i = 0; i < num_trials; i ++) {
    const critical_size = synt_adj0[i].split("_")[2];
    // get the condition for the target of indicated size
    const first_condition = synt_adj0[i].split("_")[1];
    // console.log(contexts[i]);
    var n2 = contexts[i].split("_")[1];

    // if( n2 == "service") {
    //   n2 = "service-animal"
    // } else if (n2 == "prize") {
    //   n2 = "prize-winner"
    // } else {
    //   n2 = n2
    // }
    // console.log("N2:", n2);
    const item = contexts[i].split("_")[0];
    console.log("item:", item);
    const view = {
      trial_type: "critical",
      context: items[item][n2][critical_size].context_sent + "and you see the following:", // target_size indicates if the target is big or small within the given context
      context_picture: items[item][n2][critical_size].context_picture, // context picture is chose (it is the same for both big and small targets)
      text: "Your friend points to something in the distance (not shown).", // text appearing above the target picture
      target_picture: items[item][n2][critical_size].target, // target picture, differs for bis and small target
      item: item,
      item_noun: contexts[i],
      option1: items[item][n2][critical_size].options[fc[0]], // grab left and right FC options according to order for the specific subject determined in fc
      option2: items[item][n2][critical_size].options[fc[1]],
      optionLeft: fc[0],
      target: items[item][n2][critical_size].item,
      target_size: items[item][n2][critical_size].adj_congr,
      ref_np: items[item][n2][critical_size].reference
    }
    // modify utterance etc depending on specific condition
    if (synt_adj0[i].split("_")[0] == "congr") {

      view.adj = items[item][n2][critical_size].adj_congr;
      view.adj_cond = "congruent";

      if (synt_adj0[i].split("_")[1] == "subj") {
        view.syntax = "subj";
        view.utterance = "Your friend says: <br/><b>" + "That " + items[item][n2][critical_size].adj_congr + " " + items[item][n2][critical_size].item + " is a " + items[item][n2][critical_size].reference + ".</b>";
        view.question = "What do you think your friend is saying it is " + items[item][n2][critical_size].adj_congr + " relative to?";
        view.paraphrase = "It is " + items[item][n2][critical_size].adj_congr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        view.syntax = "pred";
        view.utterance = "Your friend says: <br/><b>" + "That " + items[item][n2][critical_size].reference + " is a "  + items[item][n2][critical_size].adj_congr + " " + items[item][n2][critical_size].item + ".</b>";
        view.question = "What do you think your friend is saying it is " + items[item][n2][critical_size].adj_congr + " relative to?";
        view.paraphrase = "It is " + items[item][n2][critical_size].adj_congr + " relative to other " ;// paraphrase template
      }

    } else { // incongruent condition
      view.adj = items[item][n2][critical_size].adj_incongr;
      view.adj_cond = "incongruent";
      if (synt_adj0[i].split("_")[1] == "subj") {
        view.syntax = "subj";
        view.utterance = "Your friend says: <br/><b>" +"That " + items[item][n2][critical_size].adj_incongr + " " + items[item][n2][critical_size].item + " is a " + items[item][n2][critical_size].reference + ".</b>";
        view.question = "What do you think your friend is saying it is " + items[item][n2][critical_size].adj_incongr + " relative to?";
        view.paraphrase = "It is " + items[item][n2][critical_size].adj_incongr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        view.syntax = "pred";
        view.utterance = "Your friend says: <br/><b>" + "That " + items[item][n2][critical_size].reference + " is a "  + items[item][n2][critical_size].adj_incongr + " " + items[item][n2][critical_size].item + ".</b>";
        view.question = "What do you think your friend is saying it is " + items[item][n2][critical_size].adj_incongr + " relative to?";
        view.paraphrase = "It is " + items[item][n2][critical_size].adj_incongr + " relative to other " ;// paraphrase template
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
    function check_condition(first_condition){
      if(first_condition == "subj") {
        return "pred"
      } else {
        return "subj"
      }
    }

    const filler_size = check_size(critical_size);
    // const filler_condition = check_condition(first_condition);
    const second_size = check_size(critical_size);
    const second_condition = check_condition(first_condition);
    const second_item = "congr".concat("_", second_condition.concat("_", second_size) );
    console.log("second item constructed:", second_item);

    // const second_view = {
    //   trial_type: "critical",
    //   context: items[item][n2][second_size].context_sent + "and you see the following:", // target_size indicates if the target is big or small within the given context
    //   context_picture: items[item][n2][second_size].context_picture, // context picture is chose (it is the same for both big and small targets)
    //   text: "Your friend goes ahead of you. You see your friend in the distance:", // text appearing above the target picture
    //   target_picture: items[item][n2][second_size].target, // target picture, differs for bis and small target
    //   item: item,
    //   item_noun: contexts[i],
    //   option1: items[item][n2][second_size].options[0],
    //   option2:  items[item][n2][second_size].options[1], // donts record this as list -- make mapper instead
    //   target: items[item][n2][second_size].item,
    //   target_size: items[item][n2][second_size].adj_congr,
    //   ref_np: items[item][n2][second_size].reference
    // }
    // // modify utterance etc depending on specific condition
    // if (synt_adj0[i].split("_")[0] == "congr") {
    //
    //   second_view.adj = items[item][n2][second_size].adj_congr;
    //   second_view.adj_cond = "congruent";
    //
    //   if (second_item.split("_")[1] == "subj") {
    //     second_view.syntax = "subj";
    //     second_view.utterance = "Your friend says: <br/><b>" + "That " + items[item][n2][second_size].adj_congr + " " + items[item][n2][second_size].item + " is a " + items[item][n2][second_size].reference + ".</b>";
    //     second_view.question = "What do you think your friend is saying it is " + items[item][n2][second_size].adj_congr + " relative to?";
    //     second_view.paraphrase = "It is " + items[item][n2][second_size].adj_congr + " relative to other " ;// paraphrase template
    //   } else { // predicative condition
    //     second_view.syntax = "pred";
    //     second_view.utterance = "Your friend says: <br/><b>" + "That " + items[item][n2][second_size].reference + " is a "  + items[item][n2][second_size].adj_congr + " " + items[item][n2][second_size].item + ".</b>";
    //     second_view.question = "What do you think your friend is saying it is " + items[item][n2][second_size].adj_congr + " relative to?";
    //     second_view.paraphrase = "It is " + items[item][n2][second_size].adj_congr + " relative to other " ;// paraphrase template
    //   }
    //
    // } else { // incongruent condition
    //   second_view.adj = items[item][n2][second_size].adj_incongr;
    //   second_view.adj_cond = "incongruent";
    //   if (second_item.split("_")[1] == "subj") {
    //     second_view.syntax = "subj";
    //     second_view.utterance = "Your friend says: <br/><b>" +"That " + items[item][n2][second_size].adj_incongr + " " + items[item][n2][second_size].item + " is a " + items[item][n2][second_size].reference + ".</b>";
    //     second_view.question = "What do you think your friend is saying it is " + items[item][n2][second_size].adj_incongr + " relative to?";
    //     second_view.paraphrase = "It is " + items[item][n2][second_size].adj_incongr + " relative to other " ;// paraphrase template
    //   } else { // predicative condition
    //     second_view.syntax = "pred";
    //     second_view.utterance = "Your friend says: <br/><b>" + "That " + items[item][n2][second_size].reference + " is a "  + items[item][n2][second_size].adj_incongr + " " + items[item][n2][second_size].item + ".</b>";
    //     second_view.question = "What do you think your friend is saying it is " + items[item][n2][second_size].adj_incongr + " relative to?";
    //     second_view.paraphrase = "It is " + items[item][n2][second_size].adj_incongr + " relative to other " ;// paraphrase template
    //   }
    // }
    // // add second trial
    // expt_views.push(second_view);


    const filler = {
      trial_type: "filler",
      context: "You and your friend see the following:", // target_size indicates if the target is big or small within the given context
      context_picture: items[item][n2][filler_size].context_picture_filler, // context picture is chose (it is the same for both big and small targets)
      text: "Your friend points to something in the distance (not shown).", // text appearing above the target picture
      target_picture: items[item][n2][filler_size].target_filler, // target picture, differs for bis and small target
      item: item,
      item_noun: contexts[i],
      option1: items[item][n2][filler_size].options[fc[0]],
      option2:  items[item][n2][filler_size].options[fc[1]],
      optionLeft: fc[0],
      target: items[item][n2][filler_size].item,
      target_size: items[item][n2][filler_size].adj_congr,
      ref_np: items[item][n2][filler_size].reference
    }

    if (filler_cond[i].split("_")[0] == "congr") {

      filler.adj = items[item][n2][filler_size].adj_congr;
      filler.adj_cond = "congruent";

      if (synt_adj0[i].split("_")[1] == "pred") { // if critical trials is predicate N, do subject N filler
        filler.syntax = "subj";
        filler.utterance = "Your friend says: <br/><b>" + "That "  + items[item][n2][filler_size].item + " is " + items[item][n2][filler_size].adj_congr + ".</b>";
        filler.question = "What do you think your friend is saying it is " + items[item][n2][filler_size].adj_congr + " relative to?";
        filler.paraphrase = "It is " + items[item][n2][filler_size].adj_congr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        filler.syntax = "pred";
        filler.utterance = "Your friend says: <br/><b>" + "That's a "  + items[item][n2][filler_size].adj_congr + " " + items[item][n2][filler_size].item + ".</b>";
        filler.question = "What do you think your friend is saying it is " + items[item][n2][filler_size].adj_congr + " relative to?";
        filler.paraphrase = "It is " + items[item][n2][filler_size].adj_congr + " relative to other " ;// paraphrase template
      }

    } else { // incongruent condition
      filler.adj = items[item][n2][filler_size].adj_incongr;
      filler.adj_cond = "incongruent";
      if (synt_adj0[i].split("_")[1] == "pred") {
        filler.syntax = "subj";
        filler.utterance = "Your friend says: <br/><b>" +"That " + items[item][n2][filler_size].item + " is " + items[item][n2][filler_size].adj_incongr + ".</b>";
        filler.question = "What do you think your friend is saying it is " + items[item][n2][filler_size].adj_incongr + " relative to?";
        filler.paraphrase = "It is " + items[item][n2][filler_size].adj_incongr + " relative to other " ;// paraphrase template
      } else { // predicative condition
        filler.syntax = "pred";
        filler.utterance = "Your friend says: <br/><b>" + "That's a "  + items[item][n2][filler_size].adj_incongr + " " + items[item][n2][filler_size].item + ".</b>";
        filler.question = "What do you think your friend is saying it is " + items[item][n2][filler_size].adj_incongr + " relative to?";
        filler.paraphrase = "It is " + items[item][n2][filler_size].adj_incongr + " relative to other " ;// paraphrase template
      }
    }
    // end filler creation
    expt_views.push(filler);

  }  // end for

      return expt_views;

} // end function



const items = {
  // "_b" are the big referents
  dogs1: {
  // first set contains the basic-level context
    service: [{
     item: "doberman",
     context_sent: "You and your friend are at an animal training ground ",
     context_picture: "images/dog-parade-basic.png",
     context_picture_filler: "images/dog-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/doberman.png",
     target_filler: "images/doberman_filler.png",
     // first array is subject  , second is predicate
     reference: "service animal", // critical utterances
     options: ["dogs", "dobermans"]
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
    reference: "service animal",
    options: ["dogs", "chihuahuas"]
  }
],
rescue: [
  {
   item: "doberman",
   context_sent: "You visit your friend who works at an animal shelter ",
   context_picture: "images/dog-parade-basic-rescue.png",
   context_picture_filler: "images/dog-parade-basic_filler.png",
   adj_congr: "big",
   adj_incongr: "small",
   target: "images/doberman-rescue.png",
   target_filler: "images/doberman_filler.png",
   // first array is subject  , second is predicate
   reference: "rescue", // critical utterances
   options: ["dogs", "dobermans"]
 },
 // second set contains the subordinate context
 {
  item: "chihuahua",
  context_sent: "You visit your friend who works at an animal shelter ",
  context_picture: "images/dog-parade-basic-rescue.png",
  context_picture_filler: "images/dog-parade-basic_filler.png",
  adj_congr: "small",
  adj_incongr: "big",
  target: "images/chihuahua-rescue.png",
  target_filler: "images/chihuahua_filler.png",
  // first array are utterances in predicate, second is subject utterances
  reference: "rescue",
  options: ["dogs", "chihuahuas"]
}
]
  },

  dogs2: {
    prize: [
      {
     item: "Great Dane",
     context_sent: "You and your friend are at a pet show ",
     context_picture: "images/dog-parade-basic2.png",
     context_picture_filler: "images/dog-parade-basic2_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/great-dane.png",
     target_filler: "images/great-dane_filler.png",
     reference: "prize-winner",
     options: ["dogs", "Great Danes"]
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
    reference: "prize-winner",
    options: ["dogs", "pugs"]
  }],
  present: [
    {
   item: "Great Dane",
   context_sent: "You and your friend are at a pet store ",
   context_picture: "images/dog-parade-basic2-gift.png",
   context_picture_filler: "images/dog-parade-basic2_filler.png",
   adj_congr: "big",
   adj_incongr: "small",
   target: "images/great-dane-gift.png",
   target_filler: "images/great-dane_filler.png",
   reference: "present",
   options: ["dogs", "Great Danes"]
 },
 {
  item: "pug",
  context_sent: "You and your friend are at a pet store ",
  context_picture: "images/dog-parade-basic2-gift.png",
  context_picture_filler: "images/dog-parade-basic2_filler.png",
  adj_congr: "small",
  adj_incongr: "big",
  target: "images/pug-gift.png",
  target_filler: "images/pug_filler.png",
  reference: "present",
  options: ["dogs", "pugs"]
}
],
stray: [
      {
     item: "Great Dane",
     context_sent: "You and your friend take a walk in the city park ",
     context_picture: "images/dog-parade-basic2_stray_wLeash.png",
     context_picture_filler: "images/dog-parade-basic2_filler2.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/great-dane-gift.png",
     target_filler: "images/great-dane_filler.png",
     reference: "stray",
     options: ["dogs", "Great Danes"]
    },
    {
    item: "pug",
    context_sent: "You and your friend take a walk in the city park ",
    context_picture: "images/dog-parade-basic2_stray_wLeash.png",
    context_picture_filler: "images/dog-parade-basic2_filler2.png",
    adj_congr: "small",
    adj_incongr: "big",
    target: "images/pug-gift.png",
    target_filler: "images/pug_filler.png",
    reference: "stray",
    options: ["dogs", "pugs"]
    }
]
  },
  birds: {
    rescue:[
          {
     item: "eagle",
     context_sent: "You visit your friend who works at an animal shelter ",
     context_picture: "images/bird-parade-basic.png",
     context_picture_filler: "images/bird-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/eagle.png",
     target_filler: "images/eagle_filler.png",
     reference: "rescue",
     options: ["birds", "eagles"]
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
    reference: "rescue",
    options: ["birds", "hummingbirds"]
   }
   ]
  },
  fish: {
    rescue:[
    {
     item: "tuna",
     context_sent: "You visit your friend who works as a marine biologist ",
     context_picture: "images/fish-parade-basic.png",
     context_picture_filler: "images/fish-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/tuna_net.png",
     target_filler: "images/tuna_filler.png",
     reference: "rescue",
     options: ["fish", "tunas"]
   },
   {
    item: "clownfish",
    context_sent: "You visit your friend who works as a marine biologist ",
    context_picture: "images/fish-parade-basic.png",
    context_picture_filler: "images/fish-parade-basic_filler.png",
    adj_congr: "small",
    adj_incongr: "big",
    target: "images/clownfish_net.png",
    target_filler: "images/clownfish_filler.png",
    reference: "rescue",
    options: ["fish", "clownfish"]
   }]
  },
  flowers: {
  present: [
  {
   item: "sunflower",
   context_sent: "You and your friend are at a garden store ",
   context_picture: "images/flower-parade-basic.png",
   context_picture_filler: "images/flower-parade-basic_filler.png",
   adj_congr: "big",
   adj_incongr: "small",
   target: "images/sunflower.png",
   target_filler: "images/sunflower_filler.png",
   reference: "present",
   options: ["flowers", "sunflowers"]
 },
  {
   item: "dandelion",
   context_sent: "You and your friend are at a garden store ",
   context_picture: "images/flower-parade-basic.png",
   context_picture_filler: "images/flower-parade-basic_filler.png",
   adj_congr: "small",
   adj_incongr: "big",
   target: "images/dandelion.png",
   target_filler: "images/dandelion_filler.png",
   reference: "present",
   options: ["flowers", "dandelions"]
 }],
 landmark: [
   {
    item: "sunflower",
    context_sent: "You and your friend walk to a cabin you are renting for the weekend for the first time. You go along the path ",
    context_picture: "images/flower-parade-basic-landmark.png",
    context_picture_filler: "images/flower-parade-basic_filler.png",
    adj_congr: "big",
    adj_incongr: "small",
    target: "images/sunflower-landmark.png",
    target_filler: "images/sunflower_filler.png",
    reference: "landmark",
    options: ["flowers", "sunflowers"]
  },
   {
    item: "dandelion",
    context_sent: "You and your friend walk to a cabin you are renting for the weekend for the first time. You go along the path ",
    context_picture: "images/flower-parade-basic-landmark.png",
    context_picture_filler: "images/flower-parade-basic_filler.png",
    adj_congr: "small",
    adj_incongr: "big",
    target: "images/dandelion-landmark.png",
    target_filler: "images/dandelion_filler.png",
    reference: "landmark",
    options: ["flowers", "dandelions"]
  }
 ]
 },
  trees: {
    landmark: [    {
     item: "redwood",
     context_sent: "You and your friend walk to your friend's cabin in a park for the first time. You want to memorize the path ",
     context_picture: "images/tree-parade-basic.png",
     context_picture_filler: "images/tree-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/redwood_sign.png",
     target_filler: "images/redwood_filler.png",
     reference: "landmark",
     options: ["trees", "redwoods"]
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
   reference: "landmark",
   options: ["trees", "bonsais"]
 }]
},
buildings: {
  landmark: [
    {
     item: "skyscraper",
     context_sent: "You visit your friend who lives in another city. You take a walk ",
     context_picture: "images/buildings-parade-basic.png",
     context_picture_filler: "images/buildings-parade-basic_filler.png",
     adj_congr: "big",
     adj_incongr: "small",
     target: "images/skyscraper-landmark.png",
     target_filler: "images/skyscraper-landmark-filler.png",
     reference: "landmark",
     options: ["buildings", "skyscrapers"]
   },
  {
   item: "strip mall",
   context_sent:  "You visit your friend who lives in another city. You take a walk ",
   context_picture: "images/buildings-parade-basic.png",
   context_picture_filler: "images/buildings-parade-basic_filler.png",
   adj_congr: "small",
   adj_incongr: "big",
   target: "images/stripmall-landmark.png",
   target_filler: "images/stripmall-landmark-filler.png",
   reference: "landmark",
   options: ["buildings", "strip malls"]
 }
  ]
}
}

// warm-up trial information
// a warm-up block contains the same targets that the following main block contains
const warmup_trials = {
  dogs1: {
    service: {
        train: {
          item: "dogs1",
          examples: _.shuffle([{picture:"warmup/chihuahua1.png", question:"This is a chihuahua."},
                               {picture: "warmup/doberman1.png", question: "This is a doberman."}]),
          picture1: "warmup/chihuahua1.png",
          picture2: "warmup/doberman1.png",
          text: "Please look at the objects below.",
          question1: "This is a chihuahua.",
          question3: "This is a doberman."
       },
      label: {
        item: "dogs1",
        examples: _.shuffle([{picture:"warmup/chihuahua2.png", correct: ["chihuahua"], question: "This is a "},
                   {picture: "warmup/doberman2.png", correct:  ["doberman"], question: "This is a "}]),
        // picture1: "warmup/chihuahua2.png",
        // picture2: "warmup/doberman2.png",
        // correct1: ["chihuahua"], // correct labels for the feedback
        // correct2: ["doberman"],
        correct3: "dogs",
        text: "Please label the pictures below.",
        question1: "This is a ",
        question2: "These are both ",
        question3: "This is a "
      },
      ref: {
        item: "dogs1",
        picture1: "warmup/beagle_service.png",
        picture2: "warmup/doberman_service.png",
        question: " <br> These dogs are service animals. Notice the leash on them."
      }
    },
  rescue: {
    train: {
      item: "dogs1",
      examples: _.shuffle([{picture:"warmup/chihuahua1.png", question:"This is a chihuahua."},
                           {picture: "warmup/doberman1.png", question: "This is a doberman."}]),
      picture1: "warmup/chihuahua1.png",
      picture2: "warmup/doberman1.png",
      text: "Please look at the objects below.",
      question1: "This is a chihuahua.",
      question3: "This is a doberman."
   },
    label: {
      item: "dogs1",
      examples: _.shuffle([{picture:"warmup/chihuahua2.png", correct: ["chihuahua"], question: "This is a "},
                 {picture: "warmup/doberman2.png", correct:  ["doberman"], question: "This is a "}]),
      // picture1: "warmup/chihuahua2.png",
      // picture2: "warmup/doberman2.png",
      // correct1: ["chihuahua"], // correct labels for the feedback
      // correct2: ["doberman"],
      correct3: "dogs",
      text: "Please label the pictures below.",
      question1: "This is a ",
      question2: "These are both ",
      question3: "This is a "
    },
    ref: {
      item: "dogs1",
      picture1: "warmup/beagle-rescue.png",
      picture2: "warmup/doberman-rescue.png",
      question: " <br> These dogs are rescues. Notice the collar on them."
    }
  }
},
dogs2: {
  prize: {
      train: {
          item: "dogs2",
          examples: _.shuffle([{picture:"warmup/pug1.png", question:"This is a pug."},
                               {picture: "warmup/great-dane1.png", question: "This is a Great Dane."}]),
          picture1: "warmup/pug1.png",
          picture2: "warmup/great-dane1.png",
          default1: ["pug"],
          default2: ["great dane"],
          text: "Please look at the objects below.",
          question1: "This is a pug.",
          question3: "This is a Great Dane."
      },
      label: {
          item: "dogs2",
          examples: _.shuffle([{picture: "warmup/pug2.png", question: "This is a ", correct: ["pug"]},
                               {picture: "warmup/great-dane2.png",question: "This is a ", correct: ["great dane"]}]),
          // picture1: "warmup/pug2.png",
          // picture2: "warmup/great-dane2.png",
          // correct1: ["pug"],
          // correct2: ["great dane"],
          correct3: "dogs",
          text: "Please label the pictures below.",
          question1: "This is a ",
          question2: "These are both ",
          question3: "This is a "
        },
      ref: {
        item: "dogs2",
        picture1: "warmup/pug_prize.png",
        picture2: "warmup/great-dane_prize.png",
        question:" <br>These dogs are prize-winners. Notice the bow on them."
      }
    },
  present: {
    train: {
        item: "dogs2",
        examples: _.shuffle([{picture:"warmup/pug1.png", question:"This is a pug."},
                             {picture: "warmup/great-dane1.png", question: "This is a Great Dane."}]),
        picture1: "warmup/pug1.png",
        picture2: "warmup/great-dane1.png",
        default1: ["pug"],
        default2: ["great dane"],
        text: "Please look at the objects below.",
        question1: "This is a pug.",
        question3: "This is a Great Dane."
    },
    label: {
        item: "dogs2",
        examples: _.shuffle([{picture: "warmup/pug2.png", question: "This is a ", correct: ["pug"]},
                             {picture: "warmup/great-dane2.png",question: "This is a ", correct: ["great dane"]}]),
        // picture1: "warmup/pug2.png",
        // picture2: "warmup/great-dane2.png",
        // correct1: ["pug"],
        // correct2: ["great dane"],
        correct3: "dogs",
        text: "Please label the pictures below.",
        question1: "This is a ",
        question2: "These are both ",
        question3: "This is a "
      },
    ref: {
      item: "dogs2",
      picture1: "warmup/pug-gift.png",
      picture2: "warmup/great-dane-gift.png",
      question:" <br>You are at a pet store, and you see these dogs. These dogs are already sold and are being given as presents. Notice the bows on them."
    }
  }
},
birds: {
  rescue: {
      train: {
          item: "birds",
          examples: _.shuffle([{picture:"warmup/hummingbird1.png", question:"This is a hummingbird."},
                               {picture: "warmup/eagle1.png", question: "This is an eagle."}]),
          picture1: "warmup/hummingbird1.png",
          picture2: "warmup/eagle1.png",
          default1: ["hummingbird"],
          default2: ["eagle"],
          text: "Please look at the objects below.",
          question1: "This is a hummingbird.",
          question3: "This is an eagle."
      },
      label: {
          item: "birds",
          examples: _.shuffle([{picture: "warmup/hummingbird2.png", question: "This is a ", correct: ["hummingbird"]},
                               {picture: "warmup/eagle2.png",question: "This is an ", correct: ["eagle"]}]),
          // picture1: "warmup/hummingbird2.png",
          // picture2: "warmup/eagle2.png",
          // correct1: ["hummingbird"],
          // correct2: ["eagle"],
          correct3: "birds",
          text: "Please label the pictures below.",
          question1: "This is a ",
          question2: "These are both ",
          question3: "This is an "
    },
      ref: {
        item: "birds",
        picture1: "warmup/hummingbird_rescue.png",
        picture2: "warmup/eagle_rescue.png",
        question: " <br> These are rescue animals (or, rescues). Notice the tag on them."
      }
}
},
flowers: {
  present: {
      train: {
        item: "flowers",
        examples: _.shuffle([{picture:"warmup/dandelion1.png", question:"This is a dandelion."},
                             {picture: "warmup/sunflower1.png", question: "This is a sunflower."}]),
        picture1: "warmup/dandelion1.png",
        picture2: "warmup/sunflower1.png",
        default1: ["dandelion"],
        default2: ["sunflower"],
        text: "Please look at the objects below.",
        question1: "This is a dandelion.",
        question3: "This is a sunflower."
      },
      label: {
        item: "flowers",
        examples: _.shuffle([{picture: "warmup/dandelion2.png", question: "This is a ", correct: ["dandelion"]},
                             {picture: "warmup/sunflower2.png",question: "This is a ", correct: ["sunflower"]}]),
        // picture1: "warmup/dandelion2.png",
        // picture2: "warmup/sunflower2.png",
        // correct1: ["dandelion"],
        // correct2: ["sunflower"],
        correct3: "flowers",
        text: "Please label the pictures below.",
        question1: "This is a ",
        question2: "These are both ",
        question3: "This is a "
    },
      ref: {
        item: "flowers",
        picture1: "warmup/dandelion_gift.png",
        picture2: "warmup/sunflower_gift.png",
        question: " <br>You are at a garden store, and you see these flowers. These flowers are already sold and are being given as presents. Notice the bows on the pots."
      }
  },
  landmark: {
    train: {
      item: "flowers",
      examples: _.shuffle([{picture:"warmup/dandelion1.png", question:"This is a dandelion."},
                           {picture: "warmup/sunflower1.png", question: "This is a sunflower."}]),
      picture1: "warmup/dandelion1.png",
      picture2: "warmup/sunflower1.png",
      default1: ["dandelion"],
      default2: ["sunflower"],
      text: "Please look at the objects below.",
      question1: "This is a dandelion.",
      question3: "This is a sunflower."
    },
    label: {
      item: "flowers",
      examples: _.shuffle([{picture: "warmup/dandelion2.png", question: "This is a ", correct: ["dandelion"]},
                           {picture: "warmup/sunflower2.png",question: "This is a ", correct: ["sunflower"]}]),
      // picture1: "warmup/dandelion2.png",
      // picture2: "warmup/sunflower2.png",
      // correct1: ["dandelion"],
      // correct2: ["sunflower"],
      correct3: "flowers",
      text: "Please label the pictures below.",
      question1: "This is a ",
      question2: "These are both ",
      question3: "This is a "
  },
    ref: {
      item: "flowers",
      picture1: "warmup/dandelion-landmark.png",
      picture2: "warmup/sunflower-landmark.png",
      question: " <br> These flowers are landmarks. Notice the signs in the pots."
    }
  }
},
fish: {
  rescue: {
      train: {
        item: "fish",
        examples: _.shuffle([{picture:"warmup/clownfish1.png", question:"This is a clownfish."},
                             {picture: "warmup/tuna1.png", question: "This is a tuna."}]),
        picture1: "warmup/tuna1.png",
        picture2: "warmup/clownfish1.png",
        default1: ["tuna"],
        default2: ["clownfish"],
        text: "This one is done for you.",
        question1: "This is a ",
        question3: "This is a "
      },
      label:  {
        item: "fish",
        examples: _.shuffle([{picture: "warmup/clownfish2.png", question: "This is a ", correct: ["clownfish"]},
                             {picture: "warmup/tuna2.png",question: "This is a ", correct: ["tuna"]}]),
        // picture1: "warmup/tuna2.png",
        // picture2: "warmup/clownfish2.png",
        // correct1: ["tuna"],
        // correct2: ["clownfish"],
        correct3: "fish",
        text: "Please label the pictures below.",
        question1: "This is a ",
        question2: "These are both ",
        question3: "This is a "
      },
      ref: {
        item: "flowers",
        picture1: "warmup/tuna_rescue.png",
        picture2: "warmup/clownfish_rescue.png",
        question: " <br> These fish are rescues. Notice the nets they were caught in."
      }
  }
},
trees: {
  landmark: {
  train: {
    item: "trees",
    examples: _.shuffle([{picture:"warmup/redwood1.png", question:"This is a redwood."},
                         {picture: "warmup/bonsai1.png", question: "This is a bonsai."}]),
    picture1: "warmup/redwood1.png",
    picture2: "warmup/bonsai1.png",
    default1: ["redwood"],
    default2: ["bonsai"],
    text: "Please look at the objects below.",
    question1: "This is a redwood.",
    question3: "This is a bonsai."
  },
  label: {
    item: "trees",
    examples: _.shuffle([{picture: "warmup/redwood2.png", question: "This is a ", correct: "redwood or sequoia (choose one)"},
                         {picture: "warmup/bonsai2.png",question: "This is a ", correct: ["bonsai"]}]),
    // picture1: "warmup/redwood2.png",
    // picture2: "warmup/bonsai2.png",
    // correct1: "redwood or sequoia (choose one)",
    // correct2: ["bonsai"],
    correct3: "trees",
    text: "Please label the pictures below.",
    question1: "This is a ",
    question2: "These are both ",
    question3: "This is a "
},
ref: {
  item: "trees",
  picture1: "warmup/redwood_landmark.png",
  picture2: "warmup/tree_landmark.png",
  question: " <br> These trees are landmarks. Notice the signs on them."
}
}
},

buildings: {
  landmark: {
    train: {
      item: "buildings",
      examples: _.shuffle([{picture:"warmup/skyscraper1.png", question:"This is a skyscraper."},
                           {picture: "warmup/stripmall1.png", question: "This is a strip mall."}]),
      picture1: "warmup/skyscraper1.png",
      picture2: "warmup/stripmall1.png",
      default1: ["skyscraper"],
      default2: ["strip mall"],
      text: "Please look at the objects below.",
      question1: "This is a skyscraper.",
      question3: "This is a strip mall."
    },
    label: {
      item: "buildings",
      examples: _.shuffle([{picture: "warmup/skyscraper2.png", question: "This is a ", correct: "skyscraper"},
                           {picture: "warmup/stripmall2.png",question: "This is a ", correct: ["strip mall"]}]),
      // picture1: "warmup/redwood2.png",
      // picture2: "warmup/bonsai2.png",
      // correct1: "redwood or sequoia (choose one)",
      // correct2: ["bonsai"],
      correct3: "buildings",
      text: "Please label the pictures below.",
      question1: "This is a ",
      question2: "These are both ",
      question3: "This is a "
   },
    ref: {
      item: "buildings",
      picture1: "warmup/skyscraper_landmark.png",
      picture2: "warmup/stripmall_landmark.png",
      question: " <br> These buildings are landmarks. Notice the flags on them."
    }
  }
}

}

const main_trials = _.shuffle(create_view(items, item_noun_pairs, num_trials, synt_adj0, filler_cond, fc))
console.log("main trials before reorder: ", main_trials);

// shuffle main trials such that N2 isn't used back to back
function shuffle_trials(main_trials) {
  while(1) {
    for (var i = 0; i < main_trials.length-1; i++) {
			// console.log(i)
      // if (main_trials[i].item_noun == main_trials[i+1].item_noun) {
      if ((main_trials[i].syntax == main_trials[i+1].syntax) && (main_trials[i].trial_type == main_trials[i+1].trial_type)) {
        main_trials = _.shuffle(main_trials);
        break;
      }
      // return once we have reached the end of the trials array without finding adjacent degrees
      if (i == main_trials.length-2) { return main_trials; }
    }
  }
}

console.log("main trials after reorder:", shuffle_trials(main_trials));
// shuffle sets of warmup trials and the corresponding big and small targets
 // x: warmup trial
 // y: big target
 // z: small target
 // the trials are already shuffled in the context list

// item_noun_pairs[0].split("_")[0] - item; [1] - N2

// x: labeling + N2 warmup
// y: critical main trial
// z: filler main trial
const trials = [
  {x:warmup_trials[item_noun_pairs[0].split("_")[0]][item_noun_pairs[0].split("_")[1]], y:main_trials[0], z:main_trials[1]},
  {x:warmup_trials[item_noun_pairs[1].split("_")[0]][item_noun_pairs[1].split("_")[1]], y:main_trials[2], z:main_trials[3]},
  {x:warmup_trials[item_noun_pairs[2].split("_")[0]][item_noun_pairs[2].split("_")[1]], y:main_trials[4], z:main_trials[5]},
  {x:warmup_trials[item_noun_pairs[3].split("_")[0]][item_noun_pairs[3].split("_")[1]], y:main_trials[6], z:main_trials[7]},
  //{x:warmup_trials[contexts[4]], y:main_trials[], z:main_trials[0]},
//  {x:warmup_trials[contexts[5]], y:main_trials[5]}
]

  const trial_info = {

     text_insertion_main1: //[
// get three items for the first main trial block

       // trials[0].y,
       // trials[1].y,
       shuffle_trials(main_trials)
       // uncomment for getting filler trials

       // trials[0].z,
       // trials[1].z
  //  ],
    // get items for the second block
    // text_insertion_main2 :[
    //
    //   trials[2].y,
    //   trials[3].y,

      // uncomment for getting filler trials

      // trials[2].z,
      // trials[3].z
    // ],
    //  text_insertion_warmup1: [
    //    // get the warmup trials corresponding to the main trials in the first main block
    //    trials[0].x,
    //    trials[1].x
       // trials[2].x
    // ],
    // text_insertion_warmup2: [
    //   // warm-up trials for second block
    //   trials[2].x,
    //   trials[3].x
  //    trials[5].x
    // ],
    // ref_warmup1: [
    //   trials[0].x,
    //   trials[1].x
    // ],
    // ref_warmup2: [
    //   trials[2].x,
    //   trials[3].x
    // ]


  };
console.log(trial_info.text_insertion_main1.length)
console.log("Unshuffled trial order:", trial_info.text_insertion_main1)
