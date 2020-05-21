// In this file I create the custom view templates

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

// sentence rating task comprehension check
const custom_warmup_slider = function(config) {
  const view = {
    name: config.name,
    CT: 0,
    trials: config.trials,
    render: function(CT, magpie) {
      const startingTime = Date.now()
      $("main").html(`<div class='magpie-view1'>
      <link rel="preload" href="${config.data[CT].target}" as="image">
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.data[CT].text}</p>
      </section>
      <div class="picture" align="center">
        <img src="${config.data[CT].target}" >
      </div>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.data[CT].question}</p>
      </section>

      <div style="width:100%;">
          <div style="width:30%;height:100px;float:left;position:relative;align:center;">
              <div style="position:absolute;bottom:0;right:20px;align:center;">
                  <p class="magpie-view-question">${config.data[CT].sentence1}</p>
              </div>
          </div>
              <div style="width:70%;height:100px;float:right;position:relative;align:center;">
                  <span style="position:absolute;bpttom:0;left:20px;align:center">
                      <div class='magpie-view-answer-container'>
                          <span class='magpie-response-slider-option'>${config.data[CT].option1}</span>
                            <input type='range' id='response1' class='magpie-response-slider1' min='0' max='100' value='50'/>
                            <input type='range' id='response1_1' class='magpie-response-slider2 magpie-nodisplay' min='0' max='100'/>
                          <span class='magpie-response-slider-option' >${config.data[CT].option2}</span>
                      </div>
                  </span>
              </div>
      </div>
      <div style="width:100%;">
          <div style="width:30%;height:100px;float:left;position:relative;align:center;">
              <div style="position:absolute;bottom:0;right:20px;align:center;">
                  <p class="magpie-view-question">${config.data[CT].sentence2}</p>
              </div>
          </div>
              <div style="width:70%;height:100px;float:right;position:relative;align:center;">
                  <span style="position:absolute;bpttom:0;left:20px;align:center">
                      <div class='magpie-view-answer-container'>
                          <span class='magpie-response-slider-option'>${config.data[CT].option1}</span>
                            <input type='range' id='response2' class='magpie-response-slider1' min='0' max='100' value='50'/>
                            <input type='range' id='response2_1' class='magpie-response-slider2 magpie-nodisplay' min='0' max='100' />
                          <span class='magpie-response-slider-option' >${config.data[CT].option2}</span>
                      </div>
                  </span>
              </div>
      </div>

          <button id='next' class='magpie-view-button magpie-nodisplay'>next</button>
      </div>`);

      let next;
      let response1;
      let response2;

      next = $("#next");
      response1 = $("#response1");
      response2 = $("#response2");
// attach input listener to the sliders and check that both are clicked on
      response1.on("click", function() {
          response1.addClass("magpie-nodisplay");
          document.getElementsByTagName("input")[1].setAttribute("value", response1.val());
           $("#response1_1").removeClass("magpie-nodisplay");

           response2.on("click", function() {
             response2.addClass("magpie-nodisplay");
             document.getElementsByTagName("input")[3].setAttribute("value", response2.val());
             $("#response2_1").removeClass("magpie-nodisplay");
             $("#next").removeClass("magpie-nodisplay");
           })

      });
      response2.on("click", function() {
        response2.addClass("magpie-nodisplay");
        document.getElementsByTagName("input")[3].setAttribute("value", response2.val());
        $("#response2_1").removeClass("magpie-nodisplay");

        response1.on("click", function() {
          response1.addClass("magpie-nodisplay");
          document.getElementsByTagName("input")[1].setAttribute("value", response1.val());
          $("#response1_1").removeClass("magpie-nodisplay");
          $("#next").removeClass("magpie-nodisplay");
        })
      });
// record trial data
      next.on("click", function() {
        console.log("CLICK")
          const RT = Date.now() - startingTime; // measure RT before anything else
          let trial_data = {
              trial_name: config.name,
              trial_number: CT + 1,
              response1: $("#response1_1").val(),
              response2: $("#response2_1").val(),
              RT: RT
          };
          trial_data = magpieUtils.view.save_config_trial_data(config.data[CT], trial_data);
          magpie.trial_data.push(trial_data);
          magpie.findNextView();
      })

    }
  }
  return view;
}

// main trials view
const custom_slider = function(config, startingTime) {
  const view = {
    name: config.name,
    CT: 0,
    trials: config.trials,
    render: function(CT, magpie, startingTime) {
      const startTime = Date.now()
      $("main").html(`<div class='magpie-view1'>
      <link rel="preload" href="${config.data[CT].context_picture}" as="image">
      <link rel="preload" href="${config.data[CT].target}" as="image">
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
        <img src="${config.data[CT].target}" >
      </div>
      <section class="magpie-text-container">
        <p class="magpie-view-question">${config.data[CT].question}</p>
      </section>

      <div style="width:100%;">
          <div style="width:30%;height:100px;float:left;position:relative;align:center;">
              <div style="position:absolute;bottom:0;right:20px;align:center;">
                  <p class="magpie-view-question">${config.data[CT].sentences[syntax[0]]}</p>
              </div>
          </div>
              <div style="width:70%;height:100px;float:right;position:relative;align:center;">
                  <span style="position:absolute;bpttom:0;left:20px;align:center">
                      <div class='magpie-view-answer-container'>
                          <span class='magpie-response-slider-option'>${config.data[CT].option1}</span>
                            <input type='range' id='response1' class='magpie-response-slider1' min='0' max='100' value='50'/>
                            <input type='range' id='response1_1' class='magpie-response-slider2 magpie-nodisplay' min='0' max='100'/>
                          <span class='magpie-response-slider-option' >${config.data[CT].option2}</span>
                      </div>
                  </span>
              </div>
      </div>
      <div style="width:100%;">
          <div style="width:30%;height:100px;float:left;position:relative;align:center;">
              <div style="position:absolute;bottom:0;right:20px;align:center;">
                  <p class="magpie-view-question">${config.data[CT].sentences[syntax[1]]}</p>
              </div>
          </div>
              <div style="width:70%;height:100px;float:right;position:relative;align:center;">
                  <span style="position:absolute;bpttom:0;left:20px;align:center">
                      <div class='magpie-view-answer-container'>
                          <span class='magpie-response-slider-option'>${config.data[CT].option1}</span>
                            <input type='range' id='response2' class='magpie-response-slider1' min='0' max='100' value='50'/>
                            <input type='range' id='response2_1' class='magpie-response-slider2 magpie-nodisplay' min='0' max='100' />
                          <span class='magpie-response-slider-option' >${config.data[CT].option2}</span>
                      </div>
                  </span>
              </div>
      </div>

          <button id='next' class='magpie-view-button magpie-nodisplay'>next</button>
      </div>`);

        let next;
        let response1;
        let response2;

        next = $("#next");
        response1 = $("#response1");
        response2 = $("#response2");
// attach event listener to sliders and make sure both are clicked on before next-button appears
       response1.on("click", function() {
           response1.addClass("magpie-nodisplay");
           document.getElementsByTagName("input")[1].setAttribute("value", response1.val());
            $("#response1_1").removeClass("magpie-nodisplay");

            response2.on("click", function() {
              response2.addClass("magpie-nodisplay");
              document.getElementsByTagName("input")[3].setAttribute("value", response2.val());
              $("#response2_1").removeClass("magpie-nodisplay");
              $("#next").removeClass("magpie-nodisplay");
            })

       });
       response2.on("click", function() {
         response2.addClass("magpie-nodisplay");
         document.getElementsByTagName("input")[3].setAttribute("value", response2.val());
         $("#response2_1").removeClass("magpie-nodisplay");

         response1.on("click", function() {
           response1.addClass("magpie-nodisplay");
           document.getElementsByTagName("input")[1].setAttribute("value", response1.val());
           $("#response1_1").removeClass("magpie-nodisplay");
           $("#next").removeClass("magpie-nodisplay");
         })
       });

// record response data
       next.on("click", function() {
           const RT = Date.now() - startTime; // measure RT before anything else
           let trial_data = {
               trial_name: config.name,
               trial_number: CT + 1,
               // 0 is prenominal, 1 is predicative
               sentence1: possible_syntax[syntax[0]],
               sentence2: possible_syntax[syntax[1]],
               response1: $("#response1_1").val(),
               response2: $("#response2_1").val(),
               RT: RT
           };

           trial_data = magpieUtils.view.save_config_trial_data(config.data[CT], trial_data);

           magpie.trial_data.push(trial_data);
           magpie.findNextView();
       });

       }

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
              <label for="fairprice">What do you think is a fair price for the work you did?</label>
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
    //  text: config.text,
      render: function(CT, magpie) {
          let startingTime;
          const viewTemplate = `
          <div class='magpie-view'>
          <h1 class='magpie-view-title'>Welcome!</h1>
          <div class="picture" align="center">
            <img src="${config.picture1}">
          </div>
          <section class="magpie-text-container">
            <p class="magpie-view-text"> Thank you for taking part in our study. We are studying how people talk about things around them. The study will take about 3-5 minutes.<br /> <br />
            By continuing, you are participating in an experiment being performed by cognitive scientists in the MIT Computational Psycholinguistics Lab. If you have questions about this research, please contact Polina Tsvilodub, at <a href="mailto:polinats@mit.edu">polinats@mit.edu</a>, or MH Tessler, at tessler@mit.edu. You must be at least 18 years old to participate. Your participation in this research is voluntary. You may decline to answer any or all of the following questions. You may decline further participation, at any time, without adverse consequences. Your anonymity is assured; the researchers who have requested your participation will not receive any personal information about you.
             </p>
          </section>

          <p class = 'magpie-nodisplay' id = 'please-return'>Please return the HIT. It seems that your IP is not from the US or you have completed this HIT before.</p>
          <button class = "magpie-view-button" id="next">Go To Trials</button>
          </div>
          `;
          $("#main").html(viewTemplate);

// check unique tur ker id
          var bad_worker = false;

          console.log("UNIQUE TURKER?");
          (function(){
              var ut_id = magpie.uniqueTurkerID;
              if (UTWorkerLimitReached(ut_id)) {// {
                  bad_worker = true;
              }
          })();

// allow US IP addresses only
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
