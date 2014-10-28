(function () {
    function a() {
        e = window.jQuery.noConflict(true);
        d()
    }

	function versionIsLessThan(minimum_version, checked_version) {// checked_version < minimum_version
		if (minimum_version == null)
			return false;
		if ((checked_version == null) || (checked_version.indexOf('.') < 0))
			return true;	
		revisions_minimum = minimum_version.split('.');
		revisions_checked = checked_version.split('.');
		for (i = 0; i < revisions_minimum.length; i++) {
			if (parseInt(revisions_minimum[i]) < parseInt(revisions_checked[i])) {
				return false;
			} else if (parseInt(revisions_minimum[i]) > parseInt(revisions_checked[i])) {
				return true;
			} else if (parseInt(revisions_checked[i]) == null) {
				return true;
			}
		}
		return false;
	}

    function assert_versions(minimum, checked, expectation) {
		var pass = false;
		var returned = versionIsLessThan(minimum, checked); 
		if (returned == expectation) {
			pass = true;
		}
		console.log('if (checked[' + checked + '] is less than minimum[' + minimum + ']) returned[' + returned + '] expectation[' + expectation + '] pass[' + pass + ']');
		if (pass)
			return 0;
		return 1;
	}
	
	function unit_test() {
		var failures = 0;
		failures += assert_versions('1.10', '2.1.0', false);
		failures += assert_versions('1.10.2', '2.1', false);
		failures += assert_versions('1.10.2', '1.10.2', false);
		failures += assert_versions('1.10.2', '1.9.11', true);
		failures += assert_versions('1.10.2', '1', true);
		failures += assert_versions('1.10.2', null, true);
		failures += assert_versions('1.10.2', '', true);
		failures += assert_versions(null, '1.10.2', false);
		failures += assert_versions('', '1.10.2', false);
	
		if (failures > 0) {
			console.log('FAILURES[' + failures + ']');
		} else {
			console.log('PASS!');
		}
	}   

	var widget_cta = "Was this helpful?";
	var widget_yes_btn = "YES";
	var widget_no_btn = "NO";
	var widget_yes_cta = "What else can we do to make it even better?";
	var widget_no_cta = "Any idea, why this page is not helpful?";
	var widget_thanks = "Thank you for your feedback!";
	var widget_cta_txt_color = "#000000";
	var widget_bg_color = "#FF9900";
	var widget_visibility = "100";
	    		    
   		if(wfsf_config.cta)
   		widget_cta = wfsf_config.cta;
   		if(wfsf_config.cta_txt_color)
   		widget_cta_txt_color = wfsf_config.cta_txt_color;   		
   		if(wfsf_config.yes_btn)
   		widget_yes_btn = wfsf_config.yes_btn;
   		if(wfsf_config.no_btn)
   		widget_no_btn = wfsf_config.no_btn;
   		if(wfsf_config.yes_cta)
   		widget_yes_cta = wfsf_config.yes_cta;
   		if(wfsf_config.no_cta)
   		widget_no_cta = wfsf_config.no_cta;
   		if(wfsf_config.thanks)
   		widget_thanks = wfsf_config.thanks;
   		if(wfsf_config.tab_bg_color)
   		widget_bg_color = wfsf_config.tab_bg_color;
   		if(wfsf_config.position)
   		widget_position = wfsf_config.position;
	   	if(wfsf_config.visibility)
   		widget_visibility = wfsf_config.visibility;
	   	
    var expirydate = 30;
    var today = new Date();
    var expiry = new Date(today.getTime() + (expirydate * 24 * 60 * 60 * 1000));

    function getCookieVal(offset) {
        var endstr = document.cookie.indexOf(";", offset);
        if (endstr == -1) {
            endstr = document.cookie.length
        }
        return unescape(document.cookie.substring(offset, endstr))
    }
    function GetCookie(name) {
        var arg = name + "=";
        var alen = arg.length;
        var clen = document.cookie.length;
        var i = 0;
        while (i < clen) {
            var j = i + alen;
            if (document.cookie.substring(i, j) == arg) {
                return getCookieVal(j)
            }
            i = document.cookie.indexOf(" ", i) + 1;
            if (i == 0) break
        }
        return null
    }
    function DeleteCookie(name, path, domain) {
        if (GetCookie(name)) {
            document.cookie = name + "=" + ((path) ? "; path=" + path : "") + ((domain) ? "; domain=" + domain : "") + "; expires=Thu, 01-Jan-70 00:00:01 GMT"
        }
    }
    function SetCookie(name, value, expires, path, domain, secure) {
        document.cookie = name + "=" + escape(value) + ((expires) ? "; expires=" + expires.toGMTString() : "") + ((path) ? "; path=" + path : "") + ((domain) ? "; domain=" + domain : "") + ((secure) ? "; secure" : "")
    }
    
    function d() {
    	
        var a = true;
        e(document).ready(function (b) {
            function q(a) {
                e(".wfsf-errormsg").html(a);
                e(".wfsf-errormsg").fadeIn(300).delay(2e3).fadeOut(800)
            }
            function r() {
            	e(".wfsf-widget-cta").html(widget_thanks);
            	e(".wfsf-widget-textarea").remove();
            	e(".wfsf-submit").remove();                
                e("#wfsf-widget").delay(2e3).slideUp();
                a = false
            }
            var c = window.location;
            var d = c.hostname.replace(/^www\./, "");
            var f = c.href;
            if (document.getElementById("wfwhtsf")) {
                var g = document.getElementById("wfwhtsf").getAttribute("_pubid");
                e.getJSON("http://www.sitefeedback.com/wth5/verify.php?callback=?", "&pubid=" + g, function (a) {})
            }
            var h = b("<link>", {
                rel: "stylesheet",
                type: "text/css",
                href: "http://www.sitefeedback.com/wth6/css/stylenomedia.css?ver=13384"
            });
            h.appendTo("head");
           
            var i = null;
            var j = null;
            var k = null;
           
            var l = ['<div class="wfsf-widget-title">{0}</div>',
					'<div class="wfsf-widget-button wfsf-positive">{1}</div>',
					'<div class="wfsf-widget-button wfsf-negative">{2}</div>',
						'<div class="wfsf-widget-feedback">',
							'<div class="wfsf-widget-cta">',
								'{3}',
							'</div>',
							'<textarea class="wfsf-widget-textarea" id="wfsf-text-feedback"></textarea>',
							'<input type="hidden" id="wfsf-wth-feedback-id" value="" />',
							'<div class="wfsf-widget-button wfsf-submit">SEND</div>',
							'<div class="wfsf-widget-logo"><a href="http://www.sitefeedback.com/"></a></div>',
								'<span style="clear:both;display:block"></span>',							
							'<div class="wfsf-widget-error"></div>',
							'<!--<div class="wfsf-widget-pointer"></div>-->',
							'<div class="wfsf-widget-feedback-close"></div>',
							'<div class="wfsf-errormsg"></div>',
						'</div>',					
					'<div class="wfsf-widget-close"></div>',].join("");           
            //var l = ['<div id="wfsf-widget-box" class="wfsf-wth-widget wfsf-widget-background wfsf-widget-box">', '<p class="wfsf-widget-title">{0}</p>', '<div class="wfsf-widget-action">', '<div class="wfsf-buttons wfsf-yes">YES</div>', '<div class="wfsf-buttons wfsf-no">NO</div>', '<div class="wfsf-clear-div"></div>', "</div>", '<span class="wfsf-force-close wfsf-widget-close" id="wfsf-close"></span>', "</div>"].join("");
           
            i = e(l.format(widget_cta,widget_yes_btn,widget_no_btn,"")); //format widget for append
           
            if (GetCookie('forceclose') == null) {
                e("#wfsf-widget").append(i);   //check cookie and append the widget             
           		if(widget_position != 'inline'){
           		e("#wfsf-widget").css('background',widget_bg_color);
           		}
           		if(widget_position == 'bottom-middle'){
           			var posformid = e(window).width() / 2 - e("#wfsf-widget").width() / 2;
           			e(".wfsf-widget-bottom-middle").css('left',posformid);
           		}
           		
           		
           		e(".wfsf-widget-title").css('color',widget_cta_txt_color);
           	}else{
           		e("#wfsf-widget").hide();
           	}
           
           e(".wfsf-positive").click(function() {
           			//show comment box first//
           	 		e(".wfsf-widget-cta").html(widget_yes_cta);
                    e(".wfsf-widget-feedback").slideDown('fast');
                    e("#wfsf-text-feedback").focus(); 
                    //change button state//
                    e(this).removeClass("wfsf-widget-button-deactivate");
	                e(this).addClass("wfsf-widget-button-activate");
	                e(this).next().removeClass("wfsf-widget-button-activate");
	                e(this).next().addClass("wfsf-widget-button-deactivate");
               
                //send response data//    
                var checkb = e("#wfsf-wth-feedback-id").val();
	            if(checkb !=''){
	            	e.getJSON("http://www.sitefeedback.com/wth6/save-feedback.php?callback=?", "&wth=yes&update="+checkb, function (a){
	            		
	            	})
	            }else{
                	e.getJSON("http://www.sitefeedback.com/wth6/save-feedback.php?callback=?", "&wth=yes", function (a) {
                    var b = a.updateid;
                    //console.log(b);                    
                    e("#wfsf-wth-feedback-id").attr('value',b);
                	})                
              	}
            });
            e(".wfsf-negative").click(function() {
            		e(".wfsf-widget-cta").html(widget_no_cta);
                    e(".wfsf-widget-feedback").slideDown('fast');
                    e("#wfsf-text-feedback").focus();
                    e(this).removeClass("wfsf-widget-button-deactivate");
	                e(this).addClass("wfsf-widget-button-activate");
	                e(this).prev().removeClass("wfsf-widget-button-activate");
	                e(this).prev().addClass("wfsf-widget-button-deactivate");
	            
	            var checkb = e("#wfsf-wth-feedback-id").val();
	            if(checkb !=''){
	            	e.getJSON("http://www.sitefeedback.com/wth6/save-feedback.php?callback=?", "&wth=no&update="+checkb, function (a){
	            		
	            	})
	            }else{
                	e.getJSON("http://www.sitefeedback.com/wth6/save-feedback.php?callback=?", "&wth=no", function (a) {
                    var b = a.updateid;
                    //console.log(b);                    
                    e("#wfsf-wth-feedback-id").attr('value',b);
                	})                
              	}
            });
           
            e(".wfsf-submit").bind("click", function () {
                var a = e("#wfsf-text-feedback").val();
                var b = e("#wfsf-wth-feedback-id").val();
                if (e.trim(a) == "" || e.trim(a).length < 3) {
                    q("Comment has to be at least 3 characters long!")
                } else {
                    e.getJSON("http://www.sitefeedback.com/wth3/save-feedback.php?callback=?", "&updateid=" + b + "&message=" + a, function (a) {
                        if (a.msg = "success") {
                            r()
                        }
                        //console.log(b);
                    })
                }
                die()
            })
           
           e(".wfsf-widget-close").bind("click", function () {
                if (a == true) {
                    e("#wfsf-widget").slideToggle("fast");
                    SetCookie('forceclose', 'true', expiry, '/')
                    a = false
                } else {
                    return false
                }
            });
            
            e(".wfsf-widget-feedback-close").bind("click", function (){
              	r();
            });
            
            
        })
    }
    var e;
    var random = Math.random()*100;
  	if(random <= widget_visibility){// define percentage users can see the 'helpful'
	    if (window.jQuery === undefined || versionIsLessThan("1.10.2", window.jQuery.fn.jquery)) {
	        var f = document.createElement("script");

	        f.setAttribute("type", "text/javascript");
	        f.setAttribute("src", "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js");

	        if (f.readyState) {
	            f.onreadystatechange = function () {
	                if (this.readyState == "complete" || this.readyState == "loaded") {
	                    a()
	                }
	            }
	        } else {
	            f.onload = a
	        }(document.getElementsByTagName("head")[0] || document.documentElement).appendChild(f)
	    } else {
	        e = window.jQuery;
	        d()
	    }
    }
    String.prototype.format = function () {
        var a = this;
        for (var b = 0; b < arguments.length; b++) {
            a = a.replace(new RegExp("\\{" + b + "\\}", "gm"), arguments[b])
        }
        return a
    }
})()