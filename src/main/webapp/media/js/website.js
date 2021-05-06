//$(function() {
//  $("body").polarize({
//    "position": "right",
//    "images_dir": "/media/images/",
//    "default_topic_url": "http://polarize.it/polarize/socialfinanceapp_55868373368"
//  });
//});

function openMore() {
    $("#more_panel").css("width","100%");
    $("#more_panel").css("display","block");
}

function closeMore() {
    $("#more_panel").css("width","0%");
    $("#more_panel").css("display","none");
}

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
    
    //This mean the custom_api_collections is empty, so just one line for the home page.
    if($("#custom_api_collections_ul").html().indexOf("li")===-1){
        $(".breadcrumbs__row").css('height','40px');
        $(".api-info-section").css('top','127px');
        $(".option-section").css('top','193px');
    }
    
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
        $("#"+smallSceenEndpointId).css("font-family","Roboto-Medium")
    });
    
    //get the parameters from URL, need to remove the version and tags.
    var urlParameter = window.location.search.slice(1).split('&').filter(function(item) {
        return !item.includes("version") && (!item.includes("tags")) && (!item.includes("api-collection-id"))
    }).join("&");
    
    //and update the value for .version class
    var versions =$(".breadcrumbs .breadcrumbs__row .breadcrumbs__list .version")
    if(urlParameter !== ""){
        for (i = 0; i < versions.length; i++) {
            $(".breadcrumbs .breadcrumbs__row .breadcrumbs__list .version")[i].href=versions[i].href+"&"+urlParameter
        }
    }
});