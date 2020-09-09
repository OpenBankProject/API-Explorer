//$(function() {
//  $("body").polarize({
//    "position": "right",
//    "images_dir": "/media/images/",
//    "default_topic_url": "http://polarize.it/polarize/socialfinanceapp_55868373368"
//  });
//});
function openNav() {
    $("#left_side_small_screen").css("width","100%");
    $("#left_side_small_screen").css("display","block");
    $(".breadcrumb-section").css("display","none");
    $(".api-info-section").css("display","none");
    $("#right_side").css("display","none");
    $("#small-nav-collapse").attr("onclick","closeNav()");
    logOnButton = $("#start-login").text().indexOf("Log on");
    if (logOnButton >= 0){
        $("#left_side_small_screen .settings-box").css("display","none")
    }
}

function closeNav() {
    $("#left_side_small_screen").css("width","0");
    $("#left_side_small_screen").css("display","none");
    $(".breadcrumb-section").css("display","block");
    $(".api-info-section").css("display","block");
    $("#right_side").css("display","block");
    $("#small-nav-collapse").attr("onclick","openNav()");
}

//This function to make sure in the big screen, to close the left_side_small_screen div, then we can show the api_list 
var flag = true;
$(window).resize(function() {
    if(screen.width < 1280 && !flag){
        flag = true
    }
    if(screen.width >= 1280 && flag){
        closeNav()
        flag =false
    }
});


// In your Javascript (external .js resource or <script> tag)
$(document).ready(function() {
    $('.js-example-basic-single').select2();
    $("#select2-bank_selector-container").attr("aria-labelledby", "Banks-Dropdown")
    $("#select2-account_selector-container").attr("aria-labelledby", "Accounts-Dropdown")
    $("#select2-view_selector-container").attr("aria-labelledby", "Views-Dropdown")
    $("#select2-counterparty_selector-container").attr("aria-labelledby", "Counterparties-Dropdown")
    $("#select2-transaction_selector-container").attr("aria-labelledby", "Transactions-Dropdown")
    // scroll to leftside tag if url contains hash.
    if(window.top.location.hash){
        const hashValue = window.top.location.hash;
        $("a[href$="+hashValue+"]").parents('.api_group_item')[0].scrollIntoView();
    };

    //this will show which endpoint is slected, when you click the endpoints in the left_side_small_screen div
    $("#small-nav-collapse").click(function(){
        var hasValue = window.location.hash.substr(1);
        var smallSceenEndpointId="index_of__small_screen"+hasValue;
        $("#"+smallSceenEndpointId).parent().parent().css('height', '').attr("class","collapse in").attr("aria-expanded","true")
        $("#"+smallSceenEndpointId).css("font-family","UniversNextforHSBC-Medium")
    });
});