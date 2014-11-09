var baseURL = '/Content';

//text-overflow function, from http://devongovett.wordpress.com/2009/04/06/text-overflow-ellipsis-for-firefox-via-jquery/

(function($) {
    $.fn.ellipsis = function(enableUpdating) {
        var s = document.documentElement.style;
        if (!('textOverflow' in s || 'OTextOverflow' in s)) {
            return this.each(function(){
                var el = $(this);
                if(el.css("overflow") == "hidden"){
                    var originalText = el.html();
                    var w = el.width();
                    
                    var t = $(this.cloneNode(true)).hide().css({
                        'position': 'absolute',
                        'width': 'auto',
                        'overflow': 'visible',
                        'max-width': 'inherit'
                    });
                    el.after(t);
                    
                    var text = originalText;
                    while(text.length > 0 && t.width() > el.width()){
                        text = text.substr(0, text.length - 1);
                        t.html(text + "...");
                    }
                    el.html(t.html());
                    
                    t.remove();
                    
                    if(enableUpdating == true){
                        var oldW = el.width();
                        setInterval(function(){
                            if(el.width() != oldW){
                                oldW = el.width();
                                el.html(originalText);
                                el.ellipsis();
                            }
                        }, 200);
                    }
                }
            });
        } else return this;
    };
})(jQuery);


$(document).ready(function(){
    
    // accordions
    $('h2:has(a.expand)').next().hide();
    
    $('h2:has(a.accord)').click(function(){
        if ($(this).find('a.accord').hasClass('expand'))
        {
            $(this).next('div').slideDown('fast');
            $(this).find('a.accord').removeClass('expand');
            $(this).find('a.accord').addClass('contract');
            $(this).find('a.accord').html('&#9660;');
            
        } else {
            $(this).next('div').slideUp('fast');
            $(this).find('a.accord').removeClass('contract');
            $(this).find('a.accord').addClass('expand');
            $(this).find('a.accord').html('&#9650;');
        }
    });

    // fix ff text-overflow issue

    $('a.lastpost-link').ellipsis();
    $('.topiclist-topic-name h3').ellipsis();
    $('.forumlist-forum-name h3').ellipsis();
    
    // topic icons (from h3 classes)
    
    $('h3.new').after('<div class="newicon"> </div>');
    $('h3.pinned').before('<div class="pinnedicon"> </div>');
    $('h3.locked').before('<div class="lockedicon"> </div>');
    
    //add private icon to private comp forums
    $('.forumlist-forum.private .forumlist-forum-name h3').prepend('<img class="private-icon-forum" src="'+baseURL+'/shared/img/private-icon-small.png" alt="Private Forum" title="Private Forum" />');

    //add limited icon to limited forums
    $('.forumlist-forum.limited .forumlist-forum-name h3').prepend('<img class="limited-icon-forum" src="' + baseURL + '/shared/img/limited-icon-small.png" alt="Limited Participation Forum" title="Limited Participation Forum" />');

    // dialogs    
    $('.flag-link').click(function (e) {
        link = $(this).attr('href');
        e.preventDefault();
        $("#dialog-confirm-flag").dialog({
            resizable: false,
            width: 370,
            modal: true,
            buttons: {
                "Flag as inappropriate": function () {
                    window.location = link;
                },
                Cancel: function () {
                    $(this).dialog("close");
                }
            }
        });
    });

    var upvoteMessage = '+1 Helpful';
    var downvoteMessage = '-1 Unhelpful';
    var votedMessage = 'Unvote';

    $('.postmeta-vote .vote.set').each(function () {
        $(this).attr('title', votedMessage);
    });

    $('.postmeta-vote .vote.up.unset').each(function () {
        $(this).attr('title', upvoteMessage);
    });

    $('.postmeta-vote .vote.down.unset').each(function () {
        $(this).attr('title', downvoteMessage);
    });

    $('.postmeta-vote').on('click', '.vote.unset, .vote.set', function (e) {
        var me = $(this);

        var label = me.siblings('label:first');
        var originalValue = parseInt(label.text());
        var direction = (me.hasClass('set') ? 'un' : me.hasClass('up') ? 'up' : 'down');
        var otherDirection = (me.hasClass('up') ? 'down' : 'up');

        var otherArrow = me.siblings('.vote.' + otherDirection);

        var alreadyUpvoted = me.parent().children('.vote.up.set').length == 1;
        var alreadyDownvoted = me.parent().children('.vote.down.set').length == 1;

        var delta = 0;

        if (direction == 'un' && alreadyDownvoted) delta = 1;
        if (direction == 'un' && alreadyUpvoted) delta = -1;
        if (direction == 'up' && alreadyDownvoted) delta = 2;
        if (direction == 'up' && !alreadyDownvoted) delta = 1;
        if (direction == 'down' && alreadyUpvoted) delta = -2;
        if (direction == 'down' && !alreadyUpvoted) delta = -1;

        // Go ahead and assume the vote will succeed for better UX.
        me.removeClass(direction == 'un' ? 'set' : 'unset').addClass(direction == 'un' ? 'unset' : 'set');
        me.attr('title', direction != 'un' ? votedMessage : me.hasClass('up') ? upvoteMessage : downvoteMessage);

        otherArrow.removeClass('set').removeClass('unset').addClass('unset');
        otherArrow.attr('title', otherArrow.hasClass('up') ? upvoteMessage : downvoteMessage);

        label.text(originalValue + delta);

        // Post upvote -- do nothing on success or error
        jQuery.ajax({ url: "/forums/messages/" + direction + "vote?forumMessageId=" + $(this).attr('data-id') });
    });
});