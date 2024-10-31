$(document).keyup(function(event) {
    if ($("#myText").is(":focus") && (event.keyCode == 13)) {
        $("#myButton").click();
    }
});