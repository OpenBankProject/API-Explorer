//$(function() {
//  $("body").polarize({
//    "position": "right",
//    "images_dir": "/media/images/",
//    "default_topic_url": "http://polarize.it/polarize/socialfinanceapp_55868373368"
//  });
//});

// function openMore() {
//     // Show the More section
//     $(".breadcrumb-more-section").css("width","100%");
//     $(".breadcrumb-more-section").css("display","block");
//
//     // Hide the normal breadcrumbs
//     $(".breadcrumb-section").css("width","0%");
//     $(".breadcrumb-section").css("display","none");
// }
//
// function closeMore() {
//     // Show the Normal breadcrumbs
//     $(".breadcrumb-section").css("width","100%");
//     $(".breadcrumb-section").css("display","block");
//
//     // Hide the More section
//     $(".breadcrumb-more-section").css("width","0%");
//     $(".breadcrumb-more-section").css("display","none");
// }

function openNav() {
    $("#left_side_small_screen").css("width","100%");
    $("#left_side_small_screen").css("display","block");
    $(".breadcrumb-section").css("display","none");
    //$(".breadcrumb-collections-section").css("display","none");
    $(".api-info-section").css("display","none");
    $("#right_side").css("display","none");
    $("#small-nav-collapse").attr("onclick","closeNav()");
    const logOnButton = $("#start-login").text().indexOf("Log on");
    if (logOnButton >= 0){
        $("#left_side_small_screen .settings-box").css("display","none")
    }
}

function closeNav() {
    $("#left_side_small_screen").css("width","0");
    $("#left_side_small_screen").css("display","none");
    $(".breadcrumb-section").css("display","block");
    //$(".breadcrumb-collections-section").css("display","block");
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
    var customApiCollectionsUl = $("#custom_api_collections_ul").html();
    if(undefined!=customApiCollectionsUl){
        if (customApiCollectionsUl.indexOf("li") === -1){
            $(".breadcrumbs__row").css('height','40px');
            $(".api-info-section").css('top','127px');
            $(".option-section").css('top','193px');
        }
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

    //this will show which endpoint is selected, when you click the endpoints in the left_side_small_screen div
    $("#small-nav-collapse").click(function(){
        var hasValue = window.location.hash.substr(1);
        var smallSceenEndpointId="index_of__small_screen"+hasValue;
        $("#"+smallSceenEndpointId).parent().parent().css('height', '').attr("class","collapse in").attr("aria-expanded","true")
        $("#"+smallSceenEndpointId).css("font-family","Roboto-Medium")
    });
    
    //get the parameters from URL, need to remove the version and tags.
    var urlParameters = window.location.search.slice(1).split('&');
    var urlParameterFilteredVersionAndTagsAndContent = urlParameters.filter(function(item) {
        return !item.includes("version") 
            && (!item.includes("tags")) 
            && (!item.includes("api-collection-id"))
            && (!item.includes("content"))
            && (!item.includes("functions"))
            && (!item.includes("space_bank_id"))
    }).join("&");
    
    //and update the value for .version class
    var versions =$(".breadcrumbs .breadcrumbs__row .breadcrumbs__list .version")
    if(urlParameterFilteredVersionAndTagsAndContent !== ""){
        for (var i = 0; i < versions.length; i++) {
            $(".breadcrumbs .breadcrumbs__row .breadcrumbs__list .version")[i].href=versions[i].href+"&"+urlParameterFilteredVersionAndTagsAndContent
        }
    }
    //only show the BankId/AccountId... filtering when the version=OBPVxxx, other case it will be hidden:
    if (urlParameters.toString().indexOf("version=OBPv") != -1) {
        $(".option-section").css("display","block");
        $("#obp\\.getAdapterInfo").css("padding","220px 0 0 0");
        $(".content-box__headline").css("margin","-332px 0 0 0").css("padding","332px 0 0 0");
        $("#right_side .resource:nth-child(5) .content-box__headline").css("padding","332px 0 0 0").css("margin","0 0 32px 0");
        $("#right_side .resource:nth-child(5) .content-box .end-point-anchor form").css("padding","332px 0 0 0").css("margin","0 0 32px 0");;
    }

    $(".option-box").click(function(){
        $(".select2-dropdown--below").css('width','187px');
    });

});


function setApiLisItemLinkToClicked(element) {
    $(".api_list_item_link").css("fontWeight","normal");
    $(element).css("fontWeight", "bold");
}


function collapseAllItems() {
    var coll = document.getElementsByClassName("api_group_item_details");
    var i;
    
    for (i = 0; i < coll.length; i++) {
      coll[i].removeAttribute("open");
    };
    document.getElementById("expand_all_items_link").removeAttribute("hidden");
    document.getElementById("collapse_all_items_link").setAttribute("hidden", true);
}
function expandAllItems() {
    var coll = document.getElementsByClassName("api_group_item_details");
    var i;
    
    for (i = 0; i < coll.length; i++) {
      coll[i].setAttribute("open", true);
    };
    document.getElementById("expand_all_items_link").setAttribute("hidden", true);
    document.getElementById("collapse_all_items_link").removeAttribute("hidden");
}

function filterEndpoints(element) {
    var searchText = String(element.value).toLowerCase();

    var coll = document.getElementsByClassName("api_group_item_details");
    var i;
    
    // Show and open al top level headings
    for (i = 0; i < coll.length; i++) {
      coll[i].setAttribute("open", true);
      coll[i].removeAttribute("hidden");
    };
    document.getElementById("expand_all_items_link").setAttribute("hidden", true);
    document.getElementById("collapse_all_items_link").removeAttribute("hidden");
    
    // Filter out the list of APIs in accordance to the search text
    var endpoints = document.getElementsByClassName("api_list_item");
    for (i = 0; i < endpoints.length; i++) {
        var endpoint = endpoints[i].querySelector(".api_list_item_link");
        if(String(endpoint.text).toLowerCase().includes(searchText)) {
            endpoints[i].removeAttribute("hidden");
        } else {
            endpoints[i].setAttribute("hidden", true);
        }
    };
    
    // Filter out all top level headings with empty list
    for (i = 0; i < coll.length; i++) {
      var groupEndpoints = coll[i].getElementsByClassName("api_list_item");
      var j;
      var emptyList = true;
      for (j = 0; j < groupEndpoints.length; j++) {
        var attribute = groupEndpoints[j].getAttribute("hidden");
        if (attribute == false || attribute == null) { emptyList = false; };
      };
      if (emptyList) { coll[i].setAttribute("hidden", true); };
    };
}

function filterGlossary(element) {
    var filterText = String(element.value).toLowerCase();

    var coll = document.getElementsByClassName("api_group_item_details");
    var i;
    
    // Show and open al top level headings
    for (i = 0; i < coll.length; i++) {
      coll[i].setAttribute("open", true);
      coll[i].removeAttribute("hidden");
    };
    document.getElementById("expand_all_items_link").setAttribute("hidden", true);
    document.getElementById("collapse_all_items_link").removeAttribute("hidden");
    
    // Filter out the list of APIs in accordance to the search text
    var endpoints = document.getElementsByClassName("api_list_item");
    for (i = 0; i < endpoints.length; i++) {
        var endpoint = endpoints[i].querySelector(".api_list_item_link");
        if(String(endpoint.text).toLowerCase().includes(filterText)) {
            endpoints[i].removeAttribute("hidden");
        } else {
            endpoints[i].setAttribute("hidden", true);
        }
    };
    
    // Filter out all top level headings with empty list
    for (i = 0; i < coll.length; i++) {
      var groupEndpoints = coll[i].getElementsByClassName("api_list_item");
      var j;
      var emptyList = true;
      for (j = 0; j < groupEndpoints.length; j++) {
        var attribute = groupEndpoints[j].getAttribute("hidden");
        if (attribute == false || attribute == null) { emptyList = false; };
      };
      if (emptyList) { coll[i].setAttribute("hidden", true); };
    };
}
function filterMessageDocs(element) {
    var filterText = String(element.value).toLowerCase();

    var coll = document.getElementsByClassName("api_group_item_details");
    var i;
    
    // Show and open al top level headings
    for (i = 0; i < coll.length; i++) {
      coll[i].setAttribute("open", true);
      coll[i].removeAttribute("hidden");
    };
    document.getElementById("expand_all_items_link").setAttribute("hidden", true);
    document.getElementById("collapse_all_items_link").removeAttribute("hidden");
    
    // Filter out the list of APIs in accordance to the search text
    var endpoints = document.getElementsByClassName("api_list_item");
    for (i = 0; i < endpoints.length; i++) {
        var endpoint = endpoints[i].querySelector(".api_list_item_link");
        if(String(endpoint.text).toLowerCase().includes(filterText)) {
            endpoints[i].removeAttribute("hidden");
        } else {
            endpoints[i].setAttribute("hidden", true);
        }
    };
    
    // Filter out all top level headings with empty list
    for (i = 0; i < coll.length; i++) {
      var groupEndpoints = coll[i].getElementsByClassName("api_list_item");
      var j;
      var emptyList = true;
      for (j = 0; j < groupEndpoints.length; j++) {
        var attribute = groupEndpoints[j].getAttribute("hidden");
        if (attribute == false || attribute == null) { emptyList = false; };
      };
      if (emptyList) { coll[i].setAttribute("hidden", true); };
    };
}
