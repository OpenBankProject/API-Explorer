$(function(){
	'use strict';

	$(window).scroll(function() {
		var pos = $(this).scrollTop();
		if (pos > 300) {
			$('.header').addClass('header-scroll');
			$('.breadcrumb-section').addClass('breadcrumb-section-scroll');
			$('.option-section').addClass('option-section-scroll');
			$('.spacer-section').addClass('spacer-section-scroll');
		} else {
			$('.header').removeClass('header-scroll');
			$('.breadcrumb-section').removeClass('breadcrumb-section-scroll');
			$('.option-section').removeClass('option-section-scroll');
			$('.spacer-section').removeClass('spacer-section-scroll');
		}
	});
});
