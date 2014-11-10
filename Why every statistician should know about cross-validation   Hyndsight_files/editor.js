jQuery(document).ready(function( $ ) {

	// Navigation tabs
	$( "ul.toggle-bar a" ).click( function() {
		var tab_id = $( this ).attr( "data-tab" );

		$( "ul.toggle-bar li a" ).removeClass( "current" );
		$( ".tab-content" ).removeClass( "current" );

		$( this ).addClass( "current" );
		$( "#"+tab_id ).addClass( "current" );
		return false;
	} );

	// Close the sidebar folder icon
	$( ".toggle-bar a" ).click( function() {
		$( ".fa-folder-open" ).hide();
		$( ".fa-folder" ).show();
		return false;
	} );

	// Open the sidebar folder icon
	$( ".folder-toggle" ).click( function() {
		$( ".fa-folder, .fa-folder-open" ).toggle();
		return false;
	} );

	// Fitvid
	function fitvids() {
		$( "article iframe" ).not( ".fitvid iframe" ).wrap( "<div class='fitvid'/>" );
		$( ".fitvid" ).fitVids();
	}
	fitvids();

	// Hide the sidebar on full screen video
	$( document ).on( "webkitfullscreenchange mozfullscreenchange fullscreenchange", function () {
	    $( ".site-header" ).toggleClass( 'fullscreen' );
	});

});