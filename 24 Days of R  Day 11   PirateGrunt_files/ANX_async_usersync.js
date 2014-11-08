var ANX_async_load_flag;
if (!ANX_async_load_flag) {
	ANX_async_load_flag = true;
	function ANX_async_load(response) {
		var u;
		if (response.tagtype === "iframe" || response.tagtype === "javascript") {
			u = document.createElement("iframe");
		} else {
			u = document.createElement("img");
		}
		u.height = 1;
		u.width = 1;
		u.style.display = "none";
		function callback() {
			var s = document.createElement("script");
			s.type = "text/javascript";
			s.src = ("https:" == document.location.protocol ? "https://secure" : "http://ib") + ".adnxs.com/a_usersync?c=" + response.c + "&cbfn=ANX_async_load";
			var x = document.getElementsByTagName("script")[0];
			x.parentNode.insertBefore(s, x); 
		}
		u.onload = callback;
		u.onerror = callback;
		u.src = response.url;
		document.body.appendChild(u);
	}
	(function() {
		function ANX_async_load_init() {
			var s = document.createElement("script");
			s.type = "text/javascript";
			s.async = true;
			s.src = ("https:" == document.location.protocol ? "https://secure" : "http://ib") + ".adnxs.com/a_usersync?cbfn=ANX_async_load";
			var x = document.getElementsByTagName("script")[0];
			x.parentNode.insertBefore(s, x);
		}
		if (window.attachEvent) {
			window.attachEvent("onload", ANX_async_load_init);
		} else if (window.addEventListener) {
			window.addEventListener("load", ANX_async_load_init, false);
		}
	})();
}
