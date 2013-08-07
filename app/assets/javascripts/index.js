// This was from the original Play template.
// Sadly, I do not understand this particular
// use of JavaScript in this context, so I cannot
// really hack it to do what I want

$(function() {
    // add a click handler to the button
    $("#getMessageButton").click(function(event) {
        // make an ajax get request to get the message
        jsRoutes.controllers.MessageController.getMessage().ajax({
            success: function(data) {
                console.log(data)
                $(".well").append($("<a>").text(data.value))
            }
        })
    })
})