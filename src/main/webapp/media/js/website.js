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
});