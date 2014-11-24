/**
  *  Preserve Measure v 0.1
  *  @author Rob Goodlatte: http://robgoodlatte.com
  *  Copyright 2009 Rob Goodlatte
  *
  *  Keeps line lengths the same in a liquid layout by scaling text
  */

function /* class */ MeasureFix() {
  //  Storing the page element so we don't have to run expensive DOM function more than once
  this.page = document.getElementById('page');
  window.onresize = MeasureFix.combine(window.onresize, this.update.bind(this));
  this.update();
  this.page.style.visibility = 'visible'; // hides the initial pageload blip
}

// constants
MeasureFix.RATIO = 560;
MeasureFix.MIN = 1.4;
MeasureFix.MAX = 2.3;

/**
  *  update
  *
  *  Triggered on window resize.  Sets the font size on page element relative to window width
  */
MeasureFix.prototype.update = function() {
    // Calculate new font size from window width and ratio 
    var newSize = MeasureFix.roundVal(window.document.body.clientWidth / MeasureFix.RATIO);
    this.page.style.fontSize = (newSize < MeasureFix.MIN) ? 
                                  MeasureFix.MIN : 
                                  ((newSize > MeasureFix.MAX) ? 
                                  MeasureFix.MAX : newSize) + 'em';
}

/**
  *  bind
  *
  *  An unfortunate neccessity of Object-Oriented JS
  */
Function.prototype.bind = function (obj) {
	var fn = this;
	return function () {
		var args = [this];
		for (var i = 0, ix = arguments.length; i < ix; i++) {
			args.push(arguments[i]);
		}
		return fn.apply(obj, args);
	};
};


function activatePlaceholders() {
	var detect = navigator.userAgent.toLowerCase(); 
	if (detect.indexOf("safari") > 0) return false;
	var inputs = document.getElementsByTagName("input");
	for (var i=0;i<inputs.length;i++) {
		if (inputs[i].getAttribute("type") == "text" && inputs[i].hasAttribute("placeholder")) {
			var placeholder = inputs[i].getAttribute("placeholder");
			if (placeholder.length > 0) {
				inputs[i].value = placeholder;
				inputs[i].onclick = function() {
					if (this.value == this.getAttribute("placeholder")) {
						this.value = "";
					}
					return false;
				}
				inputs[i].onblur = function() {
					if (this.value.length < 1) {
						this.value = this.getAttribute("placeholder");
					}
				}
			}
		}
	}
}

window.onload = function() {
	if (!isIE()) {activatePlaceholders()};
}

function isIE() {
  return /msie/i.test(navigator.userAgent) && !/opera/i.test(navigator.userAgent);
}

/**
  *  roundVal
  *
  *  @param   val float number to be rounded
  *  @returns float
  *  Rounds a number to four decimal places
  */
MeasureFix.roundVal = function(val) {
  	return Math.round(val*Math.pow(10,4))/Math.pow(10,4);
}

/**
  *  combine (STATIC)
  *
  *  Combines two function calls into one.  Useful for assigning multiple window events
  *  @param func1   first function to be combined
  *  @param func2   second function to be combined
  *  @returns function
  */
MeasureFix.combine = function(func1, func2){return function(){if (func1){func1();}if (func2){func2();}};};

// Let's boot this sucker up!
window.onload = MeasureFix.combine(window.onload, function() {window.measure = new MeasureFix();});

function wrap_logout(logoutUrl) {
  if (window.FBConnect) {
    FBConnect.logout();
  } else {
    window.location = logoutUrl;
  }
    return false;
}