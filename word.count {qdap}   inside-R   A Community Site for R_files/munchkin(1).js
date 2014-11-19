/*
 * Copyright (c) 2007-2014, Marketo, Inc. All rights reserved.
 * Marketo marketing automation web activity tracking script
 * Version: prod r487
 */
 (function(a){if(!a.Munchkin){var c=a.document,e=[],k,l={fallback:"147"},g=[],m=function(){if(!k){for(;0<e.length;){var f=e.shift();a.MunchkinTracker[f[0]].apply(a.MunchkinTracker,f[1])}k=!0}},p=function(f){var b=c.createElement("script"),a=c.getElementsByTagName("base")[0]||c.getElementsByTagName("script")[0];b.type="text/javascript";b.async=!0;b.src=f;b.onreadystatechange=function(){"complete"!==this.readyState&&"loaded"!==this.readyState||m()};b.onload=m;a.parentNode.insertBefore(b,a)},h={ASSOCIATE_LEAD:"ASSOCIATE_LEAD",
CLICK_LINK:"CLICK_LINK",VISIT_WEB_PAGE:"visitWebPage",init:function(a){e.push(["init",arguments]);var b=l[a];if(!b&&0<g.length){var b=a,c=0,d;if(0!==b.length)for(d=0;d<b.length;d+=1)c+=b.charCodeAt(d);b=g[c%g.length]}b||(b=l.fallback);p("//munchkin.marketo.net/"+b+"/munchkin.js")}},n=function(a){return h[a]=function(){e.push([a,arguments])}};a.mktoMunchkinFunction=n("munchkinFunction");n("createTrackingCookie");a.Munchkin=h;a.mktoMunchkin=h.init}})(window);
