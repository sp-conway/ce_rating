// IMPORTANT
// NOTE THAT I REFER TO DIMENSIONS 1 AND 2 IN THIS CODE BUT THIS DOESNT MEAN THE ORDER IN WHICH THEY'RE PRESENTED
// USE THIS TERMINOLOGY BECAUSE WE NEED TO BALANCE WHICH DIM TARGET IS HIGH ON
// BUT ALSO RANDOMIZE DIMENSION ORDER ON SCREEN
// init js psych
const jsPsych = initJsPsych();
exp_start_time = performance.now();

// create a random participant ID just in case
var random_id = jsPsych.randomization.randomID(15);

// sampling function
function sample(X){
  return jsPsych.randomization.sampleWithoutReplacement(X, 1)[0]
}


jsPsych.data.addProperties({
  ppt_id: random_id
});
//var code =  // FOR PROLIFIC
// init timeline
timeline = [];

// utility functions
// capitalize the first letter of a string
function capitalize(val) {
  return String(val).charAt(0).toUpperCase() + String(val).slice(1);
}

// functions for sampling best and worst option attribute values
// intentionally just sampling whole / 5s to avoid ppt suspicion
function sample_catch_best() {
  return sample([50, 55, 60, 65, 70, 75, 80, 85, 90, 95]);
}
function sample_catch_worst() {
  return sample([5, 10, 15, 20, 25, 30, 35, 40, 45, 50]);
}
var enter_fullscreen = {
  type: jsPsychFullscreen,
  fullscreen_mode: true
}

// product category to attribute keys
// dimension 1 vs 2 is arbitrary, but just need to be consistent
// since d1 vs d2 matters for target high !!!
var d1_key = new Map();
d1_key.set("microwave ovens", "Warranty");
d1_key.set("laptops", "Processing speed");
d1_key.set("washing machines", "Average lifespan");
d1_key.set("televisions", "Screen size");

var d2_key = new Map();
d2_key.set("microwave ovens", "Cooking power");
d2_key.set("laptops", "Memory (RAM)");
d2_key.set("washing machines", "Energy savings");
d2_key.set("televisions", "Average lifespan");

// all orders for critical trials
var orders_crit = ["tcd", "tdc", "dct", "dtc", "ctd", "cdt"]

// all orders for catch trials
var orders_catch = ["bww", "wbw", "wwb"] // best worst worst

// preload all image files
var img_preload = {
  data: {
    screen_id: "img preload"
  },
  type: jsPsychPreload,
  images: ['laptops.png',
    'rating_example.png',
    'rating_example_2.png',
    'microwave ovens.png',
    'washing machines.png',
    'televisions.png',
    'choice_example.png']
}

// General experiment instructions
var rating_instructions = {
  type: jsPsychInstructions,
  choices: ['Continue'],
  show_clickable_nav: true,
  button_label_next: "Continue",
  button_label_previous: "Previous Page",
  pages: [
    "<div style='text-align: center; padding-left: 40px;'><p>Welcome! Thank you for participating!</p></div>",
    "<div width='100px'; style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>The experiment takes place in two phases.</p>"+
    "<p>We will describe the first phase now and the second phase after the first phase is over.</p>",
    "<div width='100px'; style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>In the first phase, you are taking the perspective of someone who runs an online consumer goods resale business.</p>",
    "<div width='100px'; style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>On each trial, you will see three products.</p></div>"+
    "<p>Your job is to provide a price for each of three products. You will enter the price using the keyboard.</p>",
    "<div width='100px'; style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>The price you select should maximize both the profit and the likelihood that people would purchase your product.</p>"+
    "<p>You will want to select a price high enough that you can make a profit but low enough that people actually purchase the product.</p>",
    "<div width='100px';style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>There will be a different set of three products on each trial. The products will either be microwave ovens, laptops, televisions, or washing machines.</p>",
    "<div width='100px';style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>Each of these products will have information about its rating on two relevant characteristics. Laptops, for example, will have information on Memory (RAM) and Processing Speed.</p>",
    "<div width='100px';style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>The ratings will vary from 0 to 100, where 0 is the lowest possible rating and 100 is the highest possible rating. Here's an example of what you will see.</p>"+
    "<div  width='100px';style='float: center;'><img src='rating_example.png' width='500' height='350'/> ",
    "<div  width='100px';style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>You will use this information to price each product.</p>"+
    "<p>The highlighted red box shows where you will enter your prices. Note that you can only use whole numbers (i.e., dollars not cents).</p>"+
    "<div  width='100px';style='float: center;'><img src='rating_example_2.png' width='500' height='350'/> ",
    "<p>There will be 40 pricing trials. After these are finished, we will explain the second phase of the experiment.</p>"+
    "<div  width='100px';style='text-align: left; padding-left: 20px;'><p>Click to start the experiment.</p></div>"
  ]
};

// setup params for rating trials  / have a blank screen between trials
var setup_rating_trial = {
  type: jsPsychHtmlKeyboardResponse,
  choices: jsPsych.NO_CHOICES,
  trial_duration: 300,
  stimulus: "<p></p>",
  data: {
    screen_id: "rating_trial_setup",
    phase: "rating",
    // randomize order
    order: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        //console.log("This is a catch trial.")
        return sample(orders_catch);
      } else {
        //console.log("This is a critical trial.")
        return sample(orders_crit);
      }
    },
    effect: function () {
      return jsPsych.timelineVariable("effect");
    },
    t_high: function () {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        return jsPsych.timelineVariable("t_high");
      }
    },
    // randomize product category for catch trials
    // otherwise timeline variable for crit variables
    category: function () {
      return jsPsych.timelineVariable("category");
    },

    // dimension1  for best option in catch trials (doesn't apply if this is a critical trial)
    d1_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_best())
      } else {
        return null
      }
    },
    // dimension2  for best option in catch trials (doesn't apply if this is a critical trial)
    d2_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_best())
      } else {
        return null
      }
    },
    // dimension1 for worst option 1 in catch trials (doesn't apply if this is a critical trial)

    d1_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // dimension2 for worst option 1 in catch trials (doesn't apply if this is a critical trial)

    d2_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // dimension1 for worst option2 in catch trials (doesn't apply if this is a critical trial)

    d1_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // dimension2 for worst option2 in catch trials (doesn't apply if this is a critical trial)

    d2_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // important - which dimension to present first
    first_dim: function() {
      var d = sample([1, 2]);
      return d;
    }
  }
}

// actual rating trials
var rating_trial = {
  type: jsPsychSurveyHtmlForm,
  // present product category name
  // capitalize first letter
  preamble: function() {
    var category = jsPsych.timelineVariable("category");
    return "<p><b>" + capitalize(category) + "</b></p>";
  },
  data: {
    screen_id: "rating_trial",
    phase: "rating",
    // order was randomized in setup
    order: function() {
      return jsPsych.data.get().last(1).values()[0].order;
    },
    // effect comes from timeline
    effect: function () {
      //console.log("Effect");
      //console.log(jsPsych.timelineVariable("effect"));
      return jsPsych.timelineVariable("effect");
    },
    /// t high comes from timeline but is not applicable to catch
    t_high: function () {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("Target high on:");
        //console.log(jsPsych.timelineVariable("t_high"));
        return jsPsych.timelineVariable("t_high");
      }
    },
    // product category comes from timeline for crit but not for catch (randomly sampledin setup)
    category: function () {
      return jsPsych.timelineVariable("category");
    },

    d1_t: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D1 t (in Data)");
        //console.log(jsPsych.timelineVariable("d1_t"));
        return jsPsych.timelineVariable("d1_t");
      }
    },
    d1_c: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D1 c (in Data)");
        //console.log(jsPsych.timelineVariable("d1_c"));
        return jsPsych.timelineVariable("d1_c");
      }
    },
    d1_d: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D1 d (in Data)");
        //console.log(jsPsych.timelineVariable("d1_d"));
        return jsPsych.timelineVariable("d1_d");
      }
    },
    d2_t: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("d2 t (in Data)");
        //console.log(jsPsych.timelineVariable("d2_t"));
        return jsPsych.timelineVariable("d2_t");
      }
    },
    d2_c: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D2 c (in Data)");
        //console.log(jsPsych.timelineVariable("d2_c"));
        return jsPsych.timelineVariable("d2_c");
      }
    },
    d2_d: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D2 d (in Data)");
        //console.log(jsPsych.timelineVariable("d2_d"));
        return jsPsych.timelineVariable("d2_d");
      }
    },
    // dimension1 for best option (again arbitrary but still matters)
    d1_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d1_best)
      } else {
        return null
      }
    },
    // dimension2 for best option (again arbitrary but still matters)
    d2_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d2_best)
      } else {
        return null
      }
    },
    // dimension1 name
    // does not necessarily mean first dimension displayed
    d1_name: function() {
      //console.log("D1 name (in data):");
      //console.log(d1_key.get(jsPsych.timelineVariable("category")));
      return d1_key.get(jsPsych.timelineVariable("category"));
    },
    // dimension2 name
    d2_name: function() {
      //console.log("D2 name (in data):");
      //console.log(d2_key.get(jsPsych.timelineVariable("category")));
      return d2_key.get(jsPsych.timelineVariable("category"));
    },
    // dimension1 for worst option1 (again arbitrary but still matters)
    d1_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d1_worst1)
      } else {
        return null
      }
    },
    // dimension2 for worst option1 (again arbitrary but still matters)
    d2_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d2_worst1)
      } else {
        return null
      }
    },
    // dimension1 for worst option2 (again arbitrary but still matters)

    d1_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d1_worst2);
      } else {
        return null;
      }
    },
    // dimension2 for worst option2 (again arbitrary but still matters)

    d2_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d2_worst2);
      } else {
        return null
      }
    },
    // WHICH DIMENSION IS PRESENTED FIRST
    first_dim: function() {
      //console.log("First dim");
      //console.log(jsPsych.data.get().last(1).values()[0].first_dim);
      return jsPsych.data.get().last(1).values()[0].first_dim;
    }

  },
  html: function() {
    var opt1_d1,
    opt1_d2,
    opt2_d1,
    opt2_d2,
    opt3_d1,
    opt3_d2;

    // which dim target is high on
    // AGAIN DOES NOT MEAN WHICH DIM IS PRESENTED FIRST
    if (jsPsych.timelineVariable("effect") == "catch") {
      var t_high = null;
    } else {
      var t_high = jsPsych.timelineVariable("t_high");
    }

    // category of product
    var category = jsPsych.timelineVariable('category');

    // stimulus order
    var order = jsPsych.data.get().last(1).values()[0].order;
    //console.log("Order", jsPsych.data.get().last(1).values()[0].order)
    // first figure out d1 and d2 for each option
    // later we figure out if we need to flip the attributes around
    if (jsPsych.timelineVariable('effect') == "catch") {
      if (order == "bww") {
        // best worst worst order
        // option 1 - best
        opt1_d1 = jsPsych.data.get().last(1).values()[0].d1_best;
        opt1_d2 = jsPsych.data.get().last(1).values()[0].d2_best;

        // option 2 - worst1
        opt2_d1 = jsPsych.data.get().last(1).values()[0].d1_worst1;
        opt2_d2 = jsPsych.data.get().last(1).values()[0].d2_worst1;

        // option  3 - worst2
        opt3_d1 = jsPsych.data.get().last(1).values()[0].d1_worst2;
        opt3_d2 = jsPsych.data.get().last(1).values()[0].d2_worst2;
      } else if (order == "wwb") {
        // option 1 - worst1
        opt1_d1 = jsPsych.data.get().last(1).values()[0].d1_worst1;
        opt1_d2 = jsPsych.data.get().last(1).values()[0].d2_worst1;

        // option 2 - worst 2
        opt2_d1 = jsPsych.data.get().last(1).values()[0].d1_worst2;
        opt2_d2 = jsPsych.data.get().last(1).values()[0].d2_worst2;

        // option 3 - best
        opt3_d1 = jsPsych.data.get().last(1).values()[0].d1_best;
        opt3_d2 = jsPsych.data.get().last(1).values()[0].d2_best;
      } else if (order == "wbw") {

        // option 1 - worst1
        opt1_d1 = jsPsych.data.get().last(1).values()[0].d1_worst1;
        opt1_d2 = jsPsych.data.get().last(1).values()[0].d2_worst1;

        // option 2 - best
        opt2_d1 = jsPsych.data.get().last(1).values()[0].d1_best;
        opt2_d2 = jsPsych.data.get().last(1).values()[0].d2_best;

        // option 3 - worst2
        opt3_d1 = jsPsych.data.get().last(1).values()[0].d1_worst2;
        opt3_d2 = jsPsych.data.get().last(1).values()[0].d2_worst2;
      }
    } else {
      // critical trial orders
      if (order == "tdc") {
        opt1_d1 = jsPsych.timelineVariable("d1_t");
        opt1_d2 = jsPsych.timelineVariable("d2_t");
        opt2_d1 = jsPsych.timelineVariable("d1_d");
        opt2_d2 = jsPsych.timelineVariable("d2_d");
        opt3_d1 = jsPsych.timelineVariable("d1_c");
        opt3_d2 = jsPsych.timelineVariable("d2_c");
      } else if (order == "tcd") {
        opt1_d1 = jsPsych.timelineVariable("d1_t");
        opt1_d2 = jsPsych.timelineVariable("d2_t");
        opt2_d1 = jsPsych.timelineVariable("d1_c");
        opt2_d2 = jsPsych.timelineVariable("d2_c");
        opt3_d1 = jsPsych.timelineVariable("d1_d");
        opt3_d2 = jsPsych.timelineVariable("d2_d");
      } else if (order == "cdt") {
        opt1_d1 = jsPsych.timelineVariable("d1_c");
        opt1_d2 = jsPsych.timelineVariable("d2_c");
        opt2_d1 = jsPsych.timelineVariable("d1_d");
        opt2_d2 = jsPsych.timelineVariable("d2_d");
        opt3_d1 = jsPsych.timelineVariable("d1_t");
        opt3_d2 = jsPsych.timelineVariable("d2_t");
      } else if (order == "ctd") {
        opt1_d1 = jsPsych.timelineVariable("d1_c");
        opt1_d2 = jsPsych.timelineVariable("d2_c");
        opt2_d1 = jsPsych.timelineVariable("d1_t");
        opt2_d2 = jsPsych.timelineVariable("d2_t");
        opt3_d1 = jsPsych.timelineVariable("d1_d");
        opt3_d2 = jsPsych.timelineVariable("d2_d");
      } else if (order == "dtc") {
        opt1_d1 = jsPsych.timelineVariable("d1_d");
        opt1_d2 = jsPsych.timelineVariable("d2_d");
        opt2_d1 = jsPsych.timelineVariable("d1_t");
        opt2_d2 = jsPsych.timelineVariable("d2_t");
        opt3_d1 = jsPsych.timelineVariable("d1_c");
        opt3_d2 = jsPsych.timelineVariable("d2_c");
      } else if (order == "dct") {
        opt1_d1 = jsPsych.timelineVariable("d1_d");
        opt1_d2 = jsPsych.timelineVariable("d2_d");
        opt2_d1 = jsPsych.timelineVariable("d1_c");
        opt2_d2 = jsPsych.timelineVariable("d2_c");
        opt3_d1 = jsPsych.timelineVariable("d1_t");
        opt3_d2 = jsPsych.timelineVariable("d2_t");
      }
    }

    // CRUCIAL - WHICH ATTRIBUTE IS PRESENTED FIRST
    var first_dim = jsPsych.data.get().last(1).values()[0].first_dim;

    // IF FIRST DIMENSION IS 1 THEN EVERYTHING IS AS IS
    if (first_dim == 1) {
      opt1_first_dim_displayed = opt1_d1;
      opt1_second_dim_displayed = opt1_d2;
      opt2_first_dim_displayed = opt2_d1;
      opt2_second_dim_displayed = opt2_d2;
      opt3_first_dim_displayed = opt3_d1;
      opt3_second_dim_displayed = opt3_d2;
      first_dim_name_displayed = d1_key.get(category);
      second_dim_name_displayed = d2_key.get(category);
    } else {
      // OTHERWISE HAVE TO FLIP AROUND THE ATTRIBUTES
      opt1_first_dim_displayed = opt1_d2;
      opt1_second_dim_displayed = opt1_d1;
      opt2_first_dim_displayed = opt2_d2;
      opt2_second_dim_displayed = opt2_d1;
      opt3_first_dim_displayed = opt3_d2;
      opt3_second_dim_displayed = opt3_d1;
      first_dim_name_displayed = d2_key.get(category);
      second_dim_name_displayed = d1_key.get(category);
    }
    //console.log("Dimension 1 Name (Displayed)");
    //console.log(first_dim_name_displayed);

    //console.log("Dimension 2 Name (Displayed)");
    //console.log(second_dim_name_displayed);

    //console.log("option 1, dimension 1:")
    //console.log(opt1_first_dim_displayed);
    //console.log("option 1, dimension 2:")
    //console.log(opt1_second_dim_displayed);

    //console.log("option 2, dimension 1:")
    //console.log(opt2_first_dim_displayed);
    //console.log("option 2, dimension 2:")
    //console.log(opt2_second_dim_displayed);

    //console.log("option 3, dimension 1:")
    //console.log(opt3_first_dim_displayed);
    //console.log("option 3, dimension 2:")
    //console.log(opt3_second_dim_displayed);

    return `
    <p>
    <img src="${category}.png" style="width:300px;height:200px;">
    <p> Please enter the price you want to sell each product for.</p>
    <table border="1">
    <tr>
    <th><b>Product</b></th>
    <th><b>${first_dim_name_displayed}</b><br>(1=worst, 100=best)</th>
    <th><b>${second_dim_name_displayed}</b><br>(1=worst, 100=best)</th>
    <th><b>Your Selling Price</b></th>
    </tr>
    <tr>
    <td><b>Product A</b></td>
    <td>${(opt1_first_dim_displayed)}</td>
    <td>${(opt1_second_dim_displayed)}</td>
    <td><label for="price_1">$</label>
    <input id="price_1" name="price_1" type="number" min="0" step="1" placeholder="0" style="width: 80px; text-align: center" oninput="this.value = this.value.replace(/[^0-9]/g, '');" required />
    </tr>
    <tr>
    <td><b>Product B</b></td>
    <td>${(opt2_first_dim_displayed)}</td>
    <td>${(opt2_second_dim_displayed)}</td>
    <td><label for="price_2">$</label>
    <input id="price_2" name="price_2" type="number" min="0" step="1" placeholder="0" style="width: 80px; text-align: center" oninput="this.value = this.value.replace(/[^0-9]/g, '');" required />
    </tr>
    <tr>
    <td><b>Product C</b></td>
    <td>${(opt3_first_dim_displayed)}</td>
    <td>${(opt3_second_dim_displayed)}</td>
    <td><label for="price_3">$</label>
    <input id="price_3" name="price_3" type="number" min="0" step="1" placeholder="0" style="width: 80px; text-align: center" oninput="this.value = this.value.replace(/[^0-9]/g, '');" required />
    </tr>
    </table>
    </p>
    `;
  }
};

var choice_instructions = {
  type: jsPsychInstructions,
  choices: ['Continue'],
  show_clickable_nav: true,
  button_label_next: "Continue",
  button_label_previous: "Previous Page",
  pages: [
    "<div style='text-align: center; padding-left: 40px;'><p>You have completed the first phase of the experiment!</p></div>",
    "<p>We will now describe the second phase.</p>",
    "<p>Now, you are taking the perspective of someone who is purchasing consumer goods in bulk.</p>",
    "<p>On each trial, you will see three products.</p></div>"+
    "<p>You are to select the product you would like to purchase.</p></div>",
    "<p>There will be a different set of three products on each trial. The products will either be microwave ovens, laptops, televisions, or washing machines - as in the first phase.</p>",
    "<p>Each of these products will have information about its rating on two relevant characteristics, as in the first phase.</p>",
    "<p>Below is an example of what you will see on each trial. You will click the circle next to the option you wish to select.</p>"+
    "<div  width='100px';style='float: center;'><img src='choice_example.png' width='500' height='440'/> ",
    "<div  width='100px';style='text-align: center; padding-left: 40px;'><p><b>The Task:</b></p><br>" +
    "<p>You will use this information to make your decisions.</p>"+
    "<p>There will be 40 choice trials. After these are finished, the experiment will be concluded.</p>"+
    "<div  width='100px';style='text-align: left; padding-left: 20px;'><p>Click to start phase two of the experiment.</p></div>"
  ]
};

var setup_choice_trial = {
  type: jsPsychHtmlKeyboardResponse,
  choices: jsPsych.NO_CHOICES,
  trial_duration: 300,
  stimulus: "<p></p>",
  data: {
    screen_id: "choice_trial_setup",
    phase: "choice",
    // randomize order
    order: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        //console.log("This is a catch trial.")
        return sample(orders_catch);
      } else {
        //console.log("This is a critical trial.")
        return sample(orders_crit);
      }
    },
    effect: function () {
      //console.log(jsPsych.timelineVariable("effect"));
      return jsPsych.timelineVariable("effect");
    },
    t_high: function () {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("T high on dim");
        //console.log(jsPsych.timelineVariable("t_high"));
        return jsPsych.timelineVariable("t_high");
      }
    },
    // randomize product category for catch trials
    // otherwise timeline variable for crit variables
    category: function () {
      return jsPsych.timelineVariable("category");
    },

    // dimension1  for best option in catch trials (doesn't apply if this is a critical trial)
    d1_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_best())
      } else {
        return null
      }
    },
    // dimension2  for best option in catch trials (doesn't apply if this is a critical trial)
    d2_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_best())
      } else {
        return null
      }
    },
    // dimension1 for worst option 1 in catch trials (doesn't apply if this is a critical trial)

    d1_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // dimension2 for worst option 1 in catch trials (doesn't apply if this is a critical trial)

    d2_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // dimension1 for worst option2 in catch trials (doesn't apply if this is a critical trial)

    d1_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // dimension2 for worst option2 in catch trials (doesn't apply if this is a critical trial)

    d2_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(sample_catch_worst())
      } else {
        return null
      }
    },
    // important - which dimension to present first
    first_dim: function() {
      var d = sample([1, 2]);
      //console.log(d);
      return d;
    }
  }
}


var choice_trial = {
  type: jsPsychSurveyHtmlForm,
  preamble: function() {
    var category = jsPsych.timelineVariable("category");
    //console.log("category", category);
    return "<p><b>" + capitalize(category) + "</b></p>";
  },
  data: {
    screen_id: "choice_trial",
    phase: "choice",
    order: function() {
      return jsPsych.data.get().last(1).values()[0].order;
    },
    effect: function () {
      //console.log(jsPsych.timelineVariable("effect"));
      return jsPsych.timelineVariable("effect");
    },
    t_high: function () {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("T high on dim");
        //console.log(jsPsych.timelineVariable("effect"));
        return jsPsych.timelineVariable("t_high");
      }
    },
    category: function () {
      return jsPsych.timelineVariable("category");
    },

    d1_t: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D1 t (in Data)");
        //console.log(jsPsych.timelineVariable("d1_t"));
        return jsPsych.timelineVariable("d1_t");
      }
    },
    d1_c: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D1 c (in Data)");
        //console.log(jsPsych.timelineVariable("d1_c"));
        return jsPsych.timelineVariable("d1_c");
      }
    },
    d1_d: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D1 d (in Data)");
        //console.log(jsPsych.timelineVariable("d1_d"));
        return jsPsych.timelineVariable("d1_d");
      }
    },
    d2_t: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D2 t (in Data)");
        //console.log(jsPsych.timelineVariable("d2_t"));
        return jsPsych.timelineVariable("d2_t");
      }
    },
    d2_c: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D2 c (in Data)");
        //console.log(jsPsych.timelineVariable("d2_c"));
        return jsPsych.timelineVariable("d2_c");
      }
    },
    d2_d: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return null
      } else {
        //console.log("D2 d (in Data)");
        //console.log(jsPsych.timelineVariable("d2_d"));
        return jsPsych.timelineVariable("d2_d");
      }
    },
    // dimension1 for best option (again arbitrary but still matters)
    d1_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d1_best)
      } else {
        return null
      }
    },
    // dimension2 for best option (again arbitrary but still matters)
    d2_best: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d2_best)
      } else {
        return null
      }
    },
    // dimension1 name DOES NOT MEAN THIS WAS NECESSARILY FIRST DIM DISPLAYED, SEE BELOOW
    // shuffle dim values
    d1_name: function() {
      //console.log("d1 name (in data)");
      //console.log(d1_key.get(jsPsych.timelineVariable("category")))
      return d1_key.get(jsPsych.timelineVariable("category"));
    },
    // dimension2 name
    d2_name: function() {
      //console.log("d2 name (in data)");
      //console.log(d2_key.get(jsPsych.timelineVariable("category")))
      return d2_key.get(jsPsych.timelineVariable("category"));
    },
    // dimension1 for worst option1 (again arbitrary but still matters)
    d1_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d1_worst1)
      } else {
        return null
      }
    },
    // dimension2 for worst option1 (again arbitrary but still matters)
    d2_worst1: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d2_worst1)
      } else {
        return null
      }
    },
    // dimension1 for worst option2 (again arbitrary but still matters)

    d1_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d1_worst2);
      } else {
        return null;
      }
    },
    // dimension2 for worst option2 (again arbitrary but still matters)

    d2_worst2: function() {
      if (jsPsych.timelineVariable("effect") == "catch") {
        return(jsPsych.data.get().last(1).values()[0].d2_worst2);
      } else {
        return null
      }
    },
    // WHICH DIMENSION IS PRESENTED FIRST
    first_dim: function() {
      //console.log("First dim");
      //console.log(jsPsych.data.get().last(1).values()[0].first_dim);
      return jsPsych.data.get().last(1).values()[0].first_dim;
    }
    // ... (Other data parameters unchanged)
  },
  html: function() {
    var opt1_d1,
    opt1_d2,
    opt2_d1,
    opt2_d2,
    opt3_d1,
    opt3_d2;

    // which dim target is high on
    // AGAIN DOES NOT MEAN WHICH DIM IS PRESENTED FIRST
    var t_high = jsPsych.timelineVariable("t_high");

    // category of product
    var category = jsPsych.timelineVariable('category');

    // stimulus order
    var order = jsPsych.data.get().last(1).values()[0].order;
    //console.log("Order", jsPsych.data.get().last(1).values()[0].order)
    // first figure out d1 and d2 for each option
    // later we figure out if we need to flip the attributes around
    if (jsPsych.timelineVariable('effect') == "catch") {
      if (order == "bww") {
        // best worst worst order
        // option 1 - best
        opt1_d1 = jsPsych.data.get().last(1).values()[0].d1_best;
        opt1_d2 = jsPsych.data.get().last(1).values()[0].d2_best;

        // option 2 - worst1
        opt2_d1 = jsPsych.data.get().last(1).values()[0].d1_worst1;
        opt2_d2 = jsPsych.data.get().last(1).values()[0].d2_worst1;

        // option  3 - worst2
        opt3_d1 = jsPsych.data.get().last(1).values()[0].d1_worst2;
        opt3_d2 = jsPsych.data.get().last(1).values()[0].d2_worst2;
      } else if (order == "wwb") {
        // option 1 - worst1
        opt1_d1 = jsPsych.data.get().last(1).values()[0].d1_worst1;
        opt1_d2 = jsPsych.data.get().last(1).values()[0].d2_worst1;

        // option 2 - worst 2
        opt2_d1 = jsPsych.data.get().last(1).values()[0].d1_worst2;
        opt2_d2 = jsPsych.data.get().last(1).values()[0].d2_worst2;

        // option 3 - best
        opt3_d1 = jsPsych.data.get().last(1).values()[0].d1_best;
        opt3_d2 = jsPsych.data.get().last(1).values()[0].d2_best;
      } else if (order == "wbw") {

        // option 1 - worst1
        opt1_d1 = jsPsych.data.get().last(1).values()[0].d1_worst1;
        opt1_d2 = jsPsych.data.get().last(1).values()[0].d2_worst1;

        // option 2 - best
        opt2_d1 = jsPsych.data.get().last(1).values()[0].d1_best;
        opt2_d2 = jsPsych.data.get().last(1).values()[0].d2_best;

        // option 3 - worst2
        opt3_d1 = jsPsych.data.get().last(1).values()[0].d1_worst2;
        opt3_d2 = jsPsych.data.get().last(1).values()[0].d2_worst2;
      }
    } else {
      // critical trial orders
      if (order == "tdc") {
        opt1_d1 = jsPsych.timelineVariable("d1_t");
        opt1_d2 = jsPsych.timelineVariable("d2_t");
        opt2_d1 = jsPsych.timelineVariable("d1_d");
        opt2_d2 = jsPsych.timelineVariable("d2_d");
        opt3_d1 = jsPsych.timelineVariable("d1_c");
        opt3_d2 = jsPsych.timelineVariable("d2_c");
      } else if (order == "tcd") {
        opt1_d1 = jsPsych.timelineVariable("d1_t");
        opt1_d2 = jsPsych.timelineVariable("d2_t");
        opt2_d1 = jsPsych.timelineVariable("d1_c");
        opt2_d2 = jsPsych.timelineVariable("d2_c");
        opt3_d1 = jsPsych.timelineVariable("d1_d");
        opt3_d2 = jsPsych.timelineVariable("d2_d");
      } else if (order == "cdt") {
        opt1_d1 = jsPsych.timelineVariable("d1_c");
        opt1_d2 = jsPsych.timelineVariable("d2_c");
        opt2_d1 = jsPsych.timelineVariable("d1_d");
        opt2_d2 = jsPsych.timelineVariable("d2_d");
        opt3_d1 = jsPsych.timelineVariable("d1_t");
        opt3_d2 = jsPsych.timelineVariable("d2_t");
      } else if (order == "ctd") {
        opt1_d1 = jsPsych.timelineVariable("d1_c");
        opt1_d2 = jsPsych.timelineVariable("d2_c");
        opt2_d1 = jsPsych.timelineVariable("d1_t");
        opt2_d2 = jsPsych.timelineVariable("d2_t");
        opt3_d1 = jsPsych.timelineVariable("d1_d");
        opt3_d2 = jsPsych.timelineVariable("d2_d");
      } else if (order == "dtc") {
        opt1_d1 = jsPsych.timelineVariable("d1_d");
        opt1_d2 = jsPsych.timelineVariable("d2_d");
        opt2_d1 = jsPsych.timelineVariable("d1_t");
        opt2_d2 = jsPsych.timelineVariable("d2_t");
        opt3_d1 = jsPsych.timelineVariable("d1_c");
        opt3_d2 = jsPsych.timelineVariable("d2_c");
      } else if (order == "dct") {
        opt1_d1 = jsPsych.timelineVariable("d1_d");
        opt1_d2 = jsPsych.timelineVariable("d2_d");
        opt2_d1 = jsPsych.timelineVariable("d1_c");
        opt2_d2 = jsPsych.timelineVariable("d2_c");
        opt3_d1 = jsPsych.timelineVariable("d1_t");
        opt3_d2 = jsPsych.timelineVariable("d2_t");
      }
    }

    // CRUCIAL - WHICH ATTRIBUTE IS PRESENTED FIRST
    var first_dim = jsPsych.data.get().last(1).values()[0].first_dim;

    // IF FIRST DIMENSION IS 1 THEN EVERYTHING IS
    if (first_dim == 1) {
      opt1_first_dim_displayed = opt1_d1;
      opt1_second_dim_displayed = opt1_d2;
      opt2_first_dim_displayed = opt2_d1;
      opt2_second_dim_displayed = opt2_d2;
      opt3_first_dim_displayed = opt3_d1;
      opt3_second_dim_displayed = opt3_d2;
      first_dim_name_displayed = d1_key.get(category);
      second_dim_name_displayed = d2_key.get(category);
    } else {
      // OTHERWISE HAVE TO FLIP AROUND THE ATTRIBUTES
      opt1_first_dim_displayed = opt1_d2;
      opt1_second_dim_displayed = opt1_d1;
      opt2_first_dim_displayed = opt2_d2;
      opt2_second_dim_displayed = opt2_d1;
      opt3_first_dim_displayed = opt3_d2;
      opt3_second_dim_displayed = opt3_d1;
      first_dim_name_displayed = d2_key.get(category);
      second_dim_name_displayed = d1_key.get(category);
    }

    //console.log("Dimension 1 Name (Displayed)");
    //console.log(first_dim_name_displayed);

    //console.log("Dimension 2 Name (Displayed)");
    //console.log(second_dim_name_displayed);

    //console.log("option 1, dimension 1:")
    //console.log(opt1_first_dim_displayed);
    //console.log("option 1, dimension 2:")
    //console.log(opt1_second_dim_displayed);

    //console.log("option 2, dimension 1:")
    //console.log(opt2_first_dim_displayed);
    //console.log("option 2, dimension 2:")
    //console.log(opt2_second_dim_displayed);

    //console.log("option 3, dimension 1:")
    //console.log(opt3_first_dim_displayed);
    //console.log("option 3, dimension 2:")
    //console.log(opt3_second_dim_displayed);


    return `
    <p>
    <img src="${category}.png" style="width:300px;height:200px;">
    <p>Please select one of the following products::</p>
    <table border="1">
    <tr>
    <th><b>Product</b></th>
    <th><b>${first_dim_name_displayed}</b><br>(1=worst, 100=best)</th>
    <th><b>${second_dim_name_displayed}</b><br>(1=worst, 100=best)</th>
    </tr>
    <tr>
    <td><b>Product A</b></td>
    <td>${(opt1_first_dim_displayed)}</td>
    <td>${(opt1_second_dim_displayed)}</td>
    <td><input type="radio" name="product_choice" value="A" required /></td>
    </tr>
    <tr>
    <td><b>Product B</b></td>
    <td>${(opt2_first_dim_displayed)}</td>
    <td>${(opt2_second_dim_displayed)}</td>
    <td><input type="radio" name="product_choice" value="B" required /></td>
    </tr>
    <tr>
    <td><b>Product C</b></td>
    <td>${(opt3_first_dim_displayed)}</td>
    <td>${(opt3_second_dim_displayed)}</td>
    <td><input type="radio" name="product_choice" value="C" required /></td>
    </tr>
    </table>
    </p>`;
  }
};

// let people know experiment has ended
var end_mssg = {
  type: jsPsychHtmlButtonResponse,
  stimulus: '<p> <b>Thank you for your patience!</b></p>' +
  '<p>  The study will be done after completing a brief form. </p>',
  choices: ["Continue"]
};

var post_exp_survey = {
  type: jsPsychSurveyText,
  questions: [{
    prompt: "We would like to know how you chose to go about this task. " +
    "There are no right or wrong answers - we are interested in your strategy."+
    "Write as little or as much as you would like below.",
    required: false
  }],
  on_finish: function(data) {
    var d = jsPsych.data.getLastTrialData().trials[0].response;
    var strategy_response = d.Q0;
    data.strategy_response = strategy_response;
    return data
  }
}

// demographics_1: age
var demo1 = {
  type: jsPsychSurveyText,
  questions: [{
    prompt: "How old are you?",
    required: true
  }],
  preamble: "<p> <strong>  DEMOGRAPHIC FORM </strong> </p>",
  on_finish: function(data) {
    var d = jsPsych.data.getLastTrialData().trials[0].response;

    jsPsych.data.addProperties({
      age: d.Q0,
    });
  }
};

// demographics 2 - race
var demo2 = {
  type: jsPsychSurveyMultiSelect,
  questions: [{
    prompt: "Race: What race do you consider yourself to be?",
    options: ["American Indian or Alaska Native",
      "Asian",
      "Black or African-American",
      "White",
      "Other",
      "Prefer not to say"],
    required: true
  }],
  preamble: "<p> <strong>  DEMOGRAPHIC FORM </strong> </p>",
  on_finish: function(data) {
    var d = jsPsych.data.getLastTrialData().trials[0].response;
    jsPsych.data.addProperties({
      race: d.Q0,
    });

  }
};


// demographics 3 - gender ethnicity
var demo3 = {
  type: jsPsychSurveyMultiChoice,
  questions: [{
    prompt: "Ethnicity: Do you consider yourself to be Hispanic or Latino?",
    options: ["Yes",
      "No"],
    required: false
  },
    {
      prompt: "Gender: What best describes your gender?",
      options: ["Female",
        "Male",
        "Non-binary",
        "Prefer not to say",
        "Other"],
      required: true
    }],
  preamble: "<p> <strong>  DEMOGRAPHIC FORM </strong> </p>",
  on_finish: function(data) {
    var time_taken = performance.now() - exp_start_time;

    var d = jsPsych.data.getLastTrialData().trials[0].response;

    jsPsych.data.addProperties({
      ethnicity: d.Q0,
      gender: d.Q1,
      exp_duration: time_taken
    });
  }
};

var end_exp_present_code = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: '',
  choices: ['q'],
  prompt: "<p>Thank you for participating! Please copy this code and enter it on Prolific: <b>"+code+"</b></p>"+
  "<p>Press the <b>'q'</b> key to end the experiment. This page will automatically close in 2 minutes. </p>",
  trial_duration: 120000
};


timeline.push(img_preload);
timeline.push(enter_fullscreen);
timeline.push(rating_instructions);


var rating_procedure = {
  timeline: [setup_rating_trial,
    rating_trial],
  timeline_variables: stim,
  randomize_order: true,
  repetitions: 1
}
var choice_procedure = {
  timeline: [setup_choice_trial,
    choice_trial],
  timeline_variables: stim,
  randomize_order: true,
  repetitions: 1
}
timeline.push(rating_procedure);
timeline.push(choice_instructions);
timeline.push(choice_procedure);
timeline.push(end_mssg);
timeline.push(post_exp_survey);
timeline.push(demo1);
timeline.push(demo2);
timeline.push(demo3);
timeline.push(end_exp_present_code);

jsPsych.run(timeline);