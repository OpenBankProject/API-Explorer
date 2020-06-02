//$(function() {
//  $("body").polarize({
//    "position": "right",
//    "images_dir": "/media/images/",
//    "default_topic_url": "http://polarize.it/polarize/socialfinanceapp_55868373368"
//  });
//});

// In your Javascript (external .js resource or <script> tag)
$(document).ready(function() {
    $('.js-example-basic-single').select2();
    // scroll to leftside tag if url contains hash.
    if(window.top.location.hash){
        const hashValue = window.top.location.hash;
        $("a[href$="+hashValue+"]")[0].scrollIntoView();
    };

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