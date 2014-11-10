/* Add plugins etc here. Remember to credit the authors ;) */
$(document).ready(function() {
  var scrollTop, scrollBottom, op;
  $(window).scroll(function() {
      menu();
  });
  function menu() {
    scrollTop = $(this).scrollTop();
    scrollBottom = $(document).height() - $(window).height();
    op = scrollBottom - scrollTop;
    if (scrollTop > (scrollBottom - 100)){
      $('#menu').removeClass('menu-top');
      $('#menu').addClass('menu-bottom');
      $('#menu').css({
        'opacity' : 1-(op/100)
      });
    } else {
      $('#menu').removeClass('menu-bottom');
      $('#menu').addClass('menu-top');
      $('#menu').css({
        'opacity' : 1-(scrollTop/100)
      });
    }  
  }

	/* 
	Title:			footnote printing
	Author: 		Laurens van Heems 
	Blog post: 	http://blog.vicompany.nl/printing-links-better-with-jquery 
	Comment:		Added and modified by Charlie Morris on 9/13/2011
					I changed the thisLink variable so that it grabs the whole URI, not
					just the relative link.  I also addded the ability to ignore certain
					anchors with the class "ignore"
	*/

	function footnoteLinks(container, target) {

		// only append list if there are any links
		if($(container + " a").length != 0){
			// append header and list to the target
			$(target).append("<h2 class='printOnly'>Links</h2>");
			$(target).append("<ol id='printlinks' class='printOnly'></ol>");
		}	

		var myArr = []; // to store all the links
		var thisLink;   // to store each link individually
		var num = 1; // to keep count

		// loop trough all the links inside container
		$(container + " a").each(function(){
			if($(this).attr("href") != "" && $(this).attr("href") != "#" && !$(this).hasClass("lightbox") && !$(this).hasClass("tooltip")){
				thisLink = $(this).attr("href");
				// if current link is not already in the list, add current link to the list
				if($.inArray(thisLink, myArr) == -1){
					$(this).after("<sup class='printOnly'>" + num + "</sup>");
					$("ol#printlinks").append("<li>" + thisLink + "</li>");
					myArr.push(thisLink);
					num++;	
				}else{
					$(this).after("<sup class='printOnly'>" + parseInt($.inArray(thisLink, myArr) + 1) + "</sup>");
				}
			}
		});
		if(num == 1){
				$("h2.printOnly").remove();
				$("ol.printOnly").remove();
		}
	}
    
   
/* for markdown in disqus, from http://code.lancepollard.com/jquery-disqus-plugin */

$(document).ready(function() {
  $('#disqus_thread').disqus({
    domain:     "your-domain",
    title:      document.title,
    developer:  window.location.hostname == "localhost" ? 1 : 0,
    show_count: true,
    prettify:   true,
    markdown:   true,
    iframe_css: "http://somewhere.com/stylesheets/disqus.css",
    ready: function() {
      // this is when your disqus comments finally load
      console.log("Comment count: " + $.disqus.commentCount().toString());
      console.log("Reaction count: " + $.disqus.reactionCount().toString());
    },
    added: function(comments) {
      // do something with the newly added comment divs.
    },
    edit: function(textarea) {
      // called when someone clicks the "reply" button.
    }
  });
});
