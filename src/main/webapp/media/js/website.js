//$(function() {
//  $("body").polarize({
//    "position": "right",
//    "images_dir": "/media/images/",
//    "default_topic_url": "http://polarize.it/polarize/socialfinanceapp_55868373368"
//  });
//});

// In your Javascript (external .js resource or <script> tag)
$(document).ready(function() {
    $("#button-show").click(function(){
        $(".comma_separated_api_call_list").show();
        $("#button-hide").show();
        $("#button-show").hide();
    });
    $("#button-hide").click(function(){
        $(".comma_separated_api_call_list").hide();
        $("#button-hide").hide();
        $("#button-show").show();
    });
});