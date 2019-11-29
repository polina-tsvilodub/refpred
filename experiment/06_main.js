// In this file you initialize and configure your experiment using magpieInit

$("document").ready(function() {
    // prevent scrolling when space is pressed
    window.onkeydown = function(e) {
        if (e.keyCode === 32 && e.target === document.body) {
            e.preventDefault();
        }
    };

    // calls magpieInit
    // in debug mode this returns the magpie-object, which you can access in the console of your browser
    // e.g. >> window.magpie_monitor or window.magpie_monitor.findNextView()
    // in all other modes null will be returned
    window.magpie_monitor = magpieInit({
        // You have to specify all views you want to use in this experiment and the order of them
        views_seq: [
            custom_intro,
            botcaptcha,
            instructions,
            custom_warmup,
            context1,
            custom_slider1,
            // context2,
            custom_slider2,
            custom_post_test,
            thanks,
        ],
        // Here, you can specify all information for the deployment
        deploy: {
            experimentID: "27",
            serverAppURL: "https://pragmatics-ptb.herokuapp.com/api/submit_experiment/",
            // Possible deployment methods are:
            // "debug" and "directLink"
            // As well as "MTurk", "MTurkSandbox" and "Prolific"
            deployMethod: "MTurk",
            contact_email: "polinats@mit.edu",
            prolificURL: "https://app.prolific.ac/submissions/complete?cc=SAMPLE1234"
        },

        uniqueTurkerID:  '25eacc6b3b42877acc2235f35889a2ef',
        // Here, you can specify how the progress bar should look like
        progress_bar: {
            in: [
                // list the view-names of the views for which you want a progress bar
                // warmup1.name,
                custom_warmup.name,
                custom_slider1.name,
                custom_slider2.name
            ],

             // Possible styles are "default", "separate" and "chunks"
            style: "separate",
            width: 100
        }
    });
});
