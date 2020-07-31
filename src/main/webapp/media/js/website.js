//$(function() {
//  $("body").polarize({
//    "position": "right",
//    "images_dir": "/media/images/",
//    "default_topic_url": "http://polarize.it/polarize/socialfinanceapp_55868373368"
//  });
//});
function openNav() {
    $("#obp-sidebar").css("width","100%");
    $(".breadcrumb-section").css("display","none");
    $(".api-info-section").css("display","none");
    $(".api-info-about").css("display","none");
    $("#right_side").css("display","none");
    $("#small-nav-collapse").attr("onclick","closeNav()");
    $(".api_list").css("display","block");
    logOnButton = $("#small-nav-log-on-button").text().indexOf("Log on")
    if (logOnButton < 0){
        $("#register-link").css("display","none")
    }
}

function closeNav() {
    $("#obp-sidebar").css("width","0");
    $(".breadcrumb-section").css("display","block");
    $(".api-info-section").css("display","block");
    $(".api-info-about").css("display","block");
    $("#right_side").css("display","block");
    $("#small-nav-collapse").attr("onclick","openNav()");
    $(".api_list").css("display","none");
}

// In your Javascript (external .js resource or <script> tag)
$(document).ready(function() {
    $('.js-example-basic-single').select2();
    // scroll to leftside tag if url contains hash.
    if(window.top.location.hash){
        const hashValue = window.top.location.hash;
        $("a[href$="+hashValue+"]").parents('.api_group_item')[0].scrollIntoView();
    };

    // $("#button-show").click(function(){
    //     $(".comma_separated_api_call_list").show();
    //     $("#button-hide").show();
    //     $("#button-show").hide();
    // });
    // $("#button-hide").click(function(){
    //     $(".comma_separated_api_call_list").hide();
    //     $("#button-hide").hide();
    //     $("#button-show").show();
    // });
});