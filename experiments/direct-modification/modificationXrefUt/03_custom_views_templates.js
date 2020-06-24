// In this file we create our own custom view templates


// botcaptcha
const custom_botcaptcha = function(config){
  const view = {
    name: config.name,
    CT: 0,
    trials: config.trials,
    render: function(CT, magpie) {
      $("main").html(`<div class="magpie-view">
        <h1 class='magpie-view-title'>Are you a bot?</h1>
        <br />
        <section class="magpie-text-container" align="center">
            <p class="magpie-text-container">${config.speaker} says to ${config.listener}: It's a beautiful day, isn't it?</p>
        </section>
        <br />
        <section class="magpie-text-container" align="center">
            <p class="magpie-text-container" id="quest-response">Who is ${config.speaker} talking to?</p>
            <section class="magpie-text-container" align="center">
                <p class="magpie-text-container">Please enter your answer in lower case.</p>
            </section
            <br />
            <textarea rows="1" cols="15" name="botresponse" id="listener-response"></textarea>

        </section>
        <br />
        <button class="magpie-view-button" id='next'>Let's go!</button>
        <section class="answer-container" align="center">
            <p class="text" id="error_incorrect" style="color: #7CB637">This is incorrect.</p>
            <p class="text" id="error_2more" style="color: #7CB637">You have 2 more trials.</p>
            <p class="text" id="error_1more" style="color: #7CB637">You have 1 more trial.</p>
            <p class="text" id="error" style="color: #7CB637">Error: You failed the comprehension questions too many times.
            You are not permitted to complete the HIT. Please click 'Return HIT' to avoid any impact on your approval rating.
            <br/><br/>
            If you believe you are receiving thin message in error, please email <a href="mailto:polinats@mit.edu">polinats@mit.edu</a> </p>
        </section>
        </div>`);
// don't allow to press enter in the response field
        $('#listener-response').keypress(function(event) {
            if (event.keyCode == 13) {
                event.preventDefault();
            }
        });
        let next = $("#next");
        // don't show any error message
        $("#error").hide();
        $("#error_incorrect").hide();
        $("#error_2more").hide();
        $("#error_1more").hide();

        // amount of trials to enter correct response
        var trial = 0;

        $("#next").on("click", function() {
            response = $("#listener-response").val().replace(" ","");

            // response correct
            if (listener.toLowerCase() == response.toLowerCase()) {
                magpie.global_data.botresponse = $("#listener-response").val();
                magpie.findNextView();

            // response false
            } else {
                trial = trial + 1;
                $("#error_incorrect").show();
                if (trial == 1) {
                    $("#error_2more").show();
                } else if (trial == 2) {
                    $("#error_2more").hide();
                    $("#error_1more").show();
                } else {
                    $("#error_incorrect").hide();
                    $("#error_1more").hide();
                    $("#next").hide();
                //    $('#quest-response').css("opacity", "0.2");
                    $('#listener-response').prop("disabled", true);
                    $("#error").show();
                };
            };

        });

    }
  };
  return view;
};


// main trials template
const custom_free_paraphrase = function(config, startingTime) {
  const view = {
    name: config.name,
    CT: 0,
    trials: config.trials,
    render: function(CT, magpie, startingTime) {
      const startTime = Date.now()
      jQuery(document).ready(function() {
        jQuery(this).scrollTop(0);
      });
      $("main").html(`<div class='magpie-view'>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.data[CT].context}</p>
      </section>
      <div class="picture" align="center">
        <img src="${config.data[CT].context_picture}" >
      </div>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.data[CT].text}</p>
      </section>
      <div class="picture" align="center">
        <img src="${config.data[CT].target_picture}" >
      </div>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.data[CT].utterance}</p>
      </section>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.data[CT].question}</p>
      </section>
      <div class='magpie-view-answer-container'>
      ${config.data[CT].paraphrase}
            <textarea name='textbox-input' rows=1 cols=15 class='textbox-input'/>

      </div>
          <button id='next' class='magpie-view-button magpie-nodisplay'>next</button>
      </div>`);

        let next;
        let textInput;


        next = $("#next");
        textInput = $("textarea");
        // minimal answer length until next-button is rendered - 2 chracters
        const minChars = config.data[CT].min_chars === undefined ? 2 : config.data[CT].min_chars;

        // attaches an event listener to the textbox input
        textInput.on("keyup", function() {
            // if the text is longer than (in this case) 2 characters without the spaces
            // the 'next' button appears
            if (textInput.val().trim().length > minChars) {
                next.removeClass("magpie-nodisplay");
            } else {
                next.addClass("magpie-nodisplay");
            }
        });

// record trial data
        $("#next").on("click", function() {
            const RT = Date.now() - startTime; // measure RT before anything else
            let trial_data = {
                trial_name: config.name,
                trial_number: CT + 1,
                question: config.data[CT].paraphrase.concat("..."),
                response: textInput.val().trim(),
                RT: RT
            };

            trial_data = magpieUtils.view.save_config_trial_data(config.data[CT], trial_data);

            magpie.trial_data.push(trial_data);
            magpie.findNextView();
        });
    },
  };
 return view;
};

// custom comparison class inference task comprehension warm-up trial view
const custom_comp_class_warmup = function(config, startingTime) {
  const view = {
    name: config.name,
    CT: 0,
    trials: config.trials,
    render: function(CT, magpie, startingTime) {
      $("main").html(`<div class='magpie-view'>
      <h1 class='magpie-view-title'> Instructions</h1>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.context}</p>
      </section>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.sentence}</p>
      </section>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.question}</p>
      </section>
      <div class='magpie-view-answer-container'>
        <p class= 'magpie-view-text'> ${config.paraphrase}
            <textarea name='textbox-input' rows=1 cols=15 class='textbox-input'/>
        </p>
        <p class = 'correct-answer magpie-nodisplay'>A possible answer is: ${config.correct}</p>
        <p class = 'correct-answer2 magpie-nodisplay'>Please correct your answer before proceeding.</p>
      </div>
          <button id='next' class='magpie-view-button magpie-nodisplay'>Continue</button>

    </div>`);

        let next;
        let textInput;

       next = $("#next");
       textInput = $("textarea");
       const minChars =  4;

        // attaches an event listener to the textbox input
        textInput.on("keyup", function() {
            // if the text is longer than (in this case) 4 characters without the spaces
            // the 'next' button appears
            if (textInput.val().trim().length > minChars) {
                next.removeClass("magpie-nodisplay");
            } else {
                next.addClass("magpie-nodisplay");
            }
        });
            var attempts = 0;
            // the trial data gets added to the trial object
            next.on("click", function(startingTime) {
              attempts = attempts + 1;
              let trial_data = {
                  trial_name: config.name,
                  trial_number: CT + 1,
                  attempts: attempts, // record how often the participants re-insert the label
                  response: textInput.val().trim()
              };
              trial_data = magpieUtils.view.save_config_trial_data(config, trial_data);
              magpie.trial_data.push(trial_data);

              var flag = true;
              // check if all the answer is correct
              if (config.correct.includes(textInput.val().trim().toLowerCase()) == false) {
                flag = false;
                $(".correct-answer").removeClass("magpie-nodisplay")
                $(".correct-answer2").removeClass("magpie-nodisplay")
              } else {
                $(".correct-answer").addClass("magpie-nodisplay")
              }

              if (flag) {
                magpie.findNextView();
              }
        });
        $('#next').on("click");
  }
 };
return view;
};

const custom_ref_warmup = function(config, startingTime) {
  const view = {
    name: config.name,
    CT: 0,
    trials: config.trials,
    render: function(CT, magpie, startingTime) {
      $("main").html(`<div class='magpie-view'>
      <h1 class='magpie-view-title'>Warm-up trials</h1>


    <div style="width:100%;height:300px;">
     <div style="width:50%;height:100%;float:left;position:relative;align:center;">
        <div style="position:absolute;bottom:0;right:40px;">
          <div class="picture"  align="center" >
            <img src="${config.data[CT].ref.picture1}">
          </div>

        </div>
    </div>
    <div style="width:50%;height:100%;float:right; position:relative;align:center;">
      <div style="position:absolute;bottom:0;left:40px;">
          <div  class="picture" align="center" >
            <img src="${config.data[CT].ref.picture2}">
          </div>

      </div>
      </div>


    </div>


    <div class='magpie-view-text-container' >

      <p id='1' class='magpie-view-question' >${config.data[CT].ref.question} </p>

    </div>



    <div  class='magpie-view-answer-container'>
      <p class = 'correct-answer4 magpie-nodisplay'>Please enter the correct labels to proceed</p>
    </div>
          <button id='next' class='magpie-view-button '>next</button>
    </div>  `);

      $('#next').hide().delay(3500).show(0);

      $("#next").on("click", function(){
        magpie.findNextView();
      })
    }
  }
  return view;
}


// labelling warm-up trials warm-up views
const custom_textfield_warmup = function(config, startingTime) {
  const view = {
    name: config.name,
    CT: 0,
    trials: config.trials,
    render: function(CT, magpie, startingTime) {
// render the pre-defined labeling view
      $("main").html(`<div class='magpie-view'>
      <h1 class='magpie-view-title'>Warm-up trials</h1>


    <div style="width:100%;">
     <div style="width:50%;height:400px;float:left;position:relative;align:center;">
        <div style="position:absolute;bottom:0;right:20px;">
          <div class="picture"  align="center" >
            <img src="${config.data[CT].train.examples[0].picture}">
          </div>
          <div  class='magpie-view-answer-container'>
            <p id='1' class='magpie-view-text'>${config.data[CT].train.examples[0].question}
            </p>
          </div>
        </div>
    </div>
    <div style="width:50%;height:400px;float:right; position:relative;align:center;">
      <div style="position:absolute;bottom:0;left:20px;">
          <div  class="picture" align="center" >
            <img src="${config.data[CT].train.examples[1].picture}">
          </div>
          <div class='magpie-view-answer-container' >
            <p id='2' class='magpie-view-text'>${config.data[CT].train.examples[1].question}
            </p>
          </div>
      </div>
      </div>
    </div>

    <div  class='magpie-view-answer-container'>

      <p class = 'correct-answer4 magpie-nodisplay'>Please enter the correct labels to proceed</p>
    </div>
          <button id='next1' class='magpie-view-button '>next</button>
    </div>  `);

    $('#next1').hide().delay(3500).show(0);

    next1 = $("#next1")

    next1.on("click" , function() {
// render the fill-in view
        $("main").html(`<div class='magpie-view'>
        <h1 class='magpie-view-title'>Warm-up trials</h1>
        <section class="magpie-text-container">
          <p class="magpie-view-question">${config.data[CT].label.text}</p>
        </section>
        <div style="width:100%;">
        <div style="width:50%;height:400px;float:left;position:relative;align:center;">
          <div style="position:absolute;bottom:0;right:20px;">
            <div class="picture"  align="center" >
              <img src="${config.data[CT].label.examples[0].picture}">
            </div>
            <div  class='magpie-view-answer-container'>
              <p id='1' class='magpie-view-text'>${config.data[CT].label.examples[0].question}
                <textarea id='textbox-input1' rows=1 cols=15 class='textbox-input'/>
              </p>
              <p class = 'correct-answer1 magpie-nodisplay'>A possible answer is: ${config.data[CT].label.examples[0].correct}</p>
            </div>
          </div>
        </div>
        <div style="width:50%;height:400px;float:right; position:relative;align:center;">
        <div style="position:absolute;bottom:0;left:20px;">
            <div  class="picture" align="center" >
              <img src="${config.data[CT].label.examples[1].picture}">
            </div>
            <div class='magpie-view-answer-container' >
              <p id='2' class='magpie-view-text'>${config.data[CT].label.examples[1].question}
                <textarea id='textbox-input2' rows=1 cols=15 class='textbox-input'/>
              </p>
              <p class = 'correct-answer2 magpie-nodisplay'>A possible answer is: ${config.data[CT].label.examples[1].correct}</p>
            </div>
        </div>
        </div>
        </div>
        <div  class='magpie-view-answer-container'>
        <p id='3' class='magpie-view-text'>${config.data[CT].label.question2}
          <textarea id='textbox-input3' rows=1 cols=15 class='textbox-input'/>
        <p id='4'></p>
        <p class = 'correct-answer3 magpie-nodisplay'>A possible answer is: ${config.data[CT].label.correct3}</p>
        </p>
        <br />
        <p class = 'correct-answer4 magpie-nodisplay'>Please enter the correct labels to proceed</p>
        </div>
            <button id='next' class='magpie-view-button magpie-nodisplay'>next</button>
        </div>  `)
    // function delayButton() {
    //   setTimeout( function() {document.getElementById('next1').removeClass("magpie-nodisplay")}, 4000);
    // }

        let next;
        let textInput1;
        let textInput2;
        let textInput3;


        const minChars = config.data[CT].min_chars === undefined ? 2 : config.data[CT].min_chars;

        next = $("#next");
        textInput1 = $("#textbox-input1")
        textInput2 = $("#textbox-input2")
        textInput3 = $("#textbox-input3")
        // attach event listeners to the text input boxes
        // if the text is longer than (in this case) 2 characters without the spaces in all textboxes
        // the 'next' button appears
        // attach event listeners to the text input boxes
        textInput1.on("keyup", function() {
            // if the text is longer than (in this case) 2 characters without the spaces in all textboxes
            // the 'next' button appears
            if (textInput1.val().trim().length > minChars)  {
              textInput2.on("keyup", function() {
                if (textInput2.val().trim().length > minChars) {
                  textInput3.on("keyup", function() {
                    if (textInput3.val().trim().length > minChars) {
                        next.removeClass("magpie-nodisplay");
                    }
                  });
                } else if (textInput3.val().trim().length > minChars) {
                    textInput2.on("keyup", function() {
                      if (textInput2.val().trim().length > minChars) {
                        next.removeClass("magpie-nodisplay");
                       }
                    })
                };
              });
            } else if (textInput2.val().trim().length > minChars) {
              textInput1.on("keyup", function() {
                if (textInput1.val().trim().length > minChars) {
                  textInput3.on("keyup", function() {
                    if (textInput3.val().trim().length > minChars) {
                        next.removeClass("magpie-nodisplay");
                    }
                  });
                } else if (textInput3.val().trim().length > minChars) {
                    textInput1.on("keyup", function() {
                      if (textInput1.val().trim().length > minChars) {
                        next.removeClass("magpie-nodisplay");
                       }
                    })
                };
              });
            } else if (textInput3.val().trim().length > minChars) {
              textInput1.on("keyup", function() {
                if (textInput1.val().trim().length > minChars) {
                  textInput2.on("keyup", function() {
                    if (textInput2.val().trim().length > minChars) {

                        next.removeClass("magpie-nodisplay");

                    }
                  });
                } else if (textInput2.val().trim().length > minChars) {
                    textInput1.on("keyup", function() {
                      if (textInput1.val().trim().length > minChars) {
                        next.removeClass("magpie-nodisplay");
                       }
                    })
                };
              });

            } else {
                next.addClass("magpie-nodisplay");
            }
        });

        var attempts = 0;
        // the trial data gets added to the trial object at first submission attempt
        next.on("click", function(startingTime) {
          attempts = attempts + 1;
          let trial_data = {
              trial_name: config.name,
              trial_number: CT + 1,
              attempts: attempts, // record how often participants re-enter labels
              response1: textInput1.val().trim(),
              response2: textInput2.val().trim(),
              response3: textInput3.val().trim(),
              picture1: config.data[CT].label.examples[0].picture,
              picture2: config.data[CT].label.examples[1].picture,
              correct1: config.data[CT].label.examples[0].correct.toString(),
              correct2: config.data[CT].label.examples[1].correct.toString()

          };
          //console.log(trial_data);
          var data_to_record = config.data[CT].label;
          // const (item, correct3, text, question1, question2, question3) = data_to_record;
          var {examples, ...data_to_record} = data_to_record;
          //console.log(data_to_record);
          trial_data = magpieUtils.view.save_config_trial_data(data_to_record, trial_data);
          magpie.trial_data.push(trial_data);

          var flag = true;
          // check if all the labels are correct
          if (config.data[CT].label.examples[0].correct.includes(textInput1.val().trim().toLowerCase()) == false) {
            flag = false;
            $(".correct-answer1").removeClass("magpie-nodisplay")
            $(".correct-answer4").removeClass("magpie-nodisplay")
          } else {
            $(".correct-answer1").addClass("magpie-nodisplay")
          }

          if (config.data[CT].label.examples[1].correct.includes(textInput2.val().trim().toLowerCase()) == false) {
            flag = false;
            $(".correct-answer2").removeClass("magpie-nodisplay")
            $(".correct-answer4").removeClass("magpie-nodisplay")
          } else {
            $(".correct-answer2").addClass("magpie-nodisplay")
          }

          if (config.data[CT].label.correct3.includes(textInput3.val().trim().toLowerCase()) == false) {
            flag = false;
            $(".correct-answer3").removeClass("magpie-nodisplay")
            $(".correct-answer4").removeClass("magpie-nodisplay")
          } else {
            $(".correct-answer3").addClass("magpie-nodisplay")
          }

          // proceed if all the labels match the input
          if (flag) {
            magpie.findNextView();
           }

        });

        $('#next').on("click");

      }) // end of first view button listener

    },  // end of render

};
return view;
};



// custom post-experiment questions
const custom_post_test_view = function(config) {
  const _survey = {
      name: config.name,
      title: config.title,
      text: config.text,
      render: function(CT, magpie) {
          let startingTime;
          const viewTemplate = `
          <div class="magpie-post-test-view">
              <h1 class="magpie-view-title">${this.title}</h1>
              <section class="text-container">
                  <h4 style = "text-align:center;">${this.text}</p>
              </section>
              <form style = "margin-top:-50px">
              <p class = "magpie-view-text" >
              <label for="understand">Did you read the instructions and do you think you completed the experiment correctly?</label>
              <select id="understand" name="understand">
                  <option></option>
                  <option value="yes" >Yes</option>
                  <option value="no">No</option>
                  <option value="confused">I was confused</option>
              </select>
          </p>
          <p class = "magpie-view-text" >
              <label for="age">Age:</label>
              <input type="number" name="age" min="18" max="110" id="age" />
          </p>
          <p class = "magpie-view-text" >
              <label for="sex">Sex:</label>
              <select id="sex" name="sex">
                  <option></option>
                  <option value="male">Male</option>
                  <option value="female">Female</option>
                  <option value="other">Other</option>
              </select>
          </p>
          <p class = "magpie-view-text" >
              <label for="education">Level of Education:</label>
              <select id="education" name="education">
                  <option></option>
                  <option value="some_high_school">Some High School</option>
                  <option value="graduated_high_school">Graduated High School</option>
                  <option value="some_college">Some College</option>
                  <option value="graduated_college">Graduated College</option>
                  <option value="higher_degree">Higher Degree</option>
              </select>
          </p>
          <p class = "magpie-view-text" >
              <label for="languages" name="languages">Native Languages: <br /><span>(i.e. the language(s) spoken at home when you were a child)</</span></label>
              <input type="text" id="languages"/>
          </p>
          <p class = "magpie-view-text" >
              <label for="enjoyment">Did you enjoy the experiment?</label>
              <select id="enjoyment" name="enjoyment">
                  <option></option>
                  <option value="0">Worse than the Average Experiment</option>
                  <option value="1" >An Average Experiment</option>
                  <option value="2">Better than average Experiment</option>
              </select>
          </p>
          <p class = "magpie-view-text" >
              <label for="fairprice">What do you think is a fair amount of compensation for your participation in this experiment?</label>
              <input type="number" name="fairprice" min="0" max="100" id="fairprice" step="0.01"/>
          </p>
          <p class = "magpie-view-text" >
              <label for="problems">Were there any problems or glitches in the experiment?</label>
              <textarea id="problems" rows="2" cols="50"></textarea>
          </p>
          <p class = "magpie-view-text"  class="comment-sect">
              <label for="comments">Further comments</label>
              <textarea name="comments" id="comments"
              rows="6" cols="40"></textarea>
          </p>
              <button class = "magpie-view-button" id="next">Finish</button>
              </form>
              </div>
          `;
          $("#main").html(viewTemplate);
          let next = $("#next");
          next.on("click", function() {
            magpie.global_data.understand = $("#understand").val();
            magpie.global_data.age = $("#age").val();
            magpie.global_data.sex = $("#sex").val();
            magpie.global_data.education = $("#education").val();
            magpie.global_data.languages = $("#languages").val();
            magpie.global_data.enjoyment = $("#enjoyment").val();
            magpie.global_data.problems = $("#problems").val().trim();
            magpie.global_data.fairprice = $("#fairprice").val();
            magpie.global_data.comments = $("#comments").val().trim();
            magpie.findNextView();
          });
          startingTime = Date.now();
      },
      CT: 0,
      trials: config.trials
  };
  return _survey;
};

// custom introduction view
const custom_intro_view = function(config) {
  const view = {
      name: config.name,
      title: config.title,
      render: function(CT, magpie) {
          let startingTime;
          const viewTemplate = `
          <div class='magpie-view'>
          <h1 class='magpie-view-title'>Welcome!</h1>
          <div class="picture" align="center">
            <img src="${config.picture1}">
          </div>
          <section class="magpie-text-container">
            <p class="magpie-view-text"> Thank you for taking part in our study. We are studying how people talk about things around them. The study will take about 7-9 minutes.<br /> <br />

            <p class="magpie-view-text" style="font-family:Courier New, Courier"> Please note: There will be multiple posted versions of this HIT (name:<b> Flowers - 555</b>) throughout the next few days.
            Please attempt only one HIT in this series. You will not be allowed to complete multiple HITs in this series. </br> </br>
            </p>
            <p class="magpie-view-text">
            By continuing, you are participating in an experiment being performed by cognitive scientists in the MIT Computational Psycholinguistics Lab. If you have questions about this research, please contact MH Tessler, at <a href="mailto:tessler@mit.edu">tessler@mit.edu</a>. You must be at least 18 years old to participate. Your participation in this research is voluntary. You may decline to answer any or all of the following questions. You may decline further participation, at any time, without adverse consequences. Your anonymity is assured; the researchers who have requested your participation will not receive any personal information about you.
             </p>
          </section>

          <p class = 'magpie-nodisplay' id = 'please-return'>Please return the HIT. It seems that your IP is not from the US or you have completed this HIT before.</p>
          <button class = "magpie-view-button" id="next">Go To Trials</button>
          </div>
          `;
          $("#main").html(viewTemplate);

//unique turker check
          var bad_worker = false;

          console.log("UNIQUE TURKER?");
          (function(){
              var ut_id = magpie.uniqueTurkerID;
              if (UTWorkerLimitReached(ut_id)) {// {
                  bad_worker = true;
              }
          })();

// US IP address check
          console.log("ARE YOU FROM THE US???");
          function USOnly() {var accessKey = 'b487843addca6e9ec32e6ae28aeaa022';

               $.ajax({
                   url: 'https://api.ipstack.com/check?access_key='+accessKey,
                   dataType: 'jsonp',

                   success: function(json) {
                     if (json.country_code != 'US') {
                       bad_worker = true;
                   }
               }
             });
           }


          let next = $("#next");
          next.on("click", function() {
            if (bad_worker) {
              $("#please-return").removeClass('magpie-nodisplay')
            } else {
              magpie.findNextView();
            }
          });
          startingTime = Date.now();
      },
      CT: 0,
      trials: config.trials
  };
  return view;
};
