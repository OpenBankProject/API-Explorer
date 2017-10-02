$(function(){
	'use strict';

	$(window).scroll(function() {
		console.log('scroll');
		var pos = $(this).scrollTop();
		if (pos > 10) {
			console.log('scrol > 300');
			$('.header').addClass('header-scroll');
			$('.breadcrumb-section').addClass('breadcrumb-section-scroll');
			$('.api-info-section').addClass('api-info-section-scroll');
			$('.option-section').addClass('option-section-scroll');
			$('.content-box__headline').addClass('content-box__headline-scroll');
		} else {
			$('.header').removeClass('header-scroll');
			$('.breadcrumb-section').removeClass('breadcrumb-section-scroll');
			$('.api-info-section').removeClass('api-info-section-scroll');
			$('.option-section').removeClass('option-section-scroll');
			$('.content-box__headline').removeClass('content-box__headline-scroll');
		}
	});
});
