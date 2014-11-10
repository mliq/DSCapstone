(function() {  // Randall Farmer, twotwotwo at gmail.  Public domain, 2007-2009.
  var esc = function(str) { 
    var s = escape(str); 
    return s.replace(/\+/g, '%2B') 
  };
  var escHTML = function(str) {
    return str.replace( // escape for HTML attribute
        /[\'\"<>\=\$\\]/g, 
 function(c) { return '&#' + c.charCodeAt(0) + ';' } 
    );
  };
function substr( f_string, f_start, f_length ) {
    // Returns part of a string  
    // 
    // version: 810.1317
    // discuss at: http://phpjs.org/functions/substr
    // +     original by: Martijn Wieringa
    // +     bugfixed by: T.Wild
    // +      tweaked by: Onno Marsman
    // *       example 1: substr('abcdef', 0, -1);
    // *       returns 1: 'abcde'
    // *       example 2: substr(2, 0, -6);
    // *       returns 2: ''
    f_string += '';

    if(f_start < 0) {
        f_start += f_string.length;
    }

    if(f_length == undefined) {
        f_length = f_string.length;
    } else if(f_length < 0){
        f_length += f_string.length;
    } else {
        f_length += f_start;
    }

    if(f_length < f_start) {
        f_length = f_start;
    }

    return f_string.substring(f_start, f_length);
}

  var images = [];  // for hide-raw-TeX-on-load hack
  var mathUrls = {};
  var mathRegExp = 
    /\$\$(.*?)\$\$|\$(.*?)\$|\\\((.|\n)*?\\\)|\\\[(.*?)\\\]/g;
  var mathRegExp2 = 
    /(\$\$(.*?)\$\$|\$(.*?)\$|\\\((.*?)\\\)|\\\[(.|\n)*?\\\])/;

  if ( !window.mathSite ) 
    window.mathSite = window.location.hostname + window.location.pathname + 
      window.location.search;
//===================================================================================================================================
//===================================================================================================================================
//========================================================================
// ||          Set Your Preference Here                                 || 
// ||                                                                   ||
// =======================================================================
  if ( !window.mathServer ) 
    window.mathServer = 'http://www.forkosh.dreamhost.com/mathtex.cgi?';
  if ( !window.mathJsServer ) 
    window.mathJsServer = 'http://mathcache.appspot.com/';
  if ( !window.mathPreamble ) window.mathPreamble = '\\usepackage[usenames]{color}\\gammacorrection{1}\\png ';

//================================================================================
//||           Don't MOdify Under THis Line Unless You Know What You Are Doing !!  ||
//==================================================================================


  var div = document.createElement('div');
  var $L = function(i) { if ( window.console ) window.console.log(i) }
  
  var isMath = window.mathChecker || function( mathText ) {
      if ( window.mathNoDollar )
          return /^(\$\$|\\\(|\\\[)/.test(mathText);
      if ( /[_\\^]|\$\$|\w\(/.test(mathText) ) 
      	  return true;
      if ( /[A-Za-z]{2,} [A-Za-z]{2,}/.test(mathText) )
          return false;
      if ( !!/^\$[ \t\n]/.test(mathText) != !!/[ \t\n]\$$/.test(mathText) )
          return false;
      return true;
  };

  
  var replacement = function( mathHtml ) {
    div.innerHTML = mathHtml;
    var mathText = div.firstChild.nodeValue;
    if ( !isMath(mathText) ) return mathHtml;
 if (substr(mathText,0,2)== '\${')   return "\$"+substr(mathText,2,-2)+"\$";
if (substr(mathText,0,3)== '\\[{')   return "\\["+substr(mathText,3,-3)+"\\]"
if (substr(mathText,0,2)== '$!')   return "\$"+substr(mathText,2,-1)+" ";   
// if (substr(mathText,0,2)== '\$\$')   var mathText = substr(mathText,2,-2);
// if (substr(mathText,0,2)== '\\\]')   var src = 
 //             mathServer  + mathPreamble + substr(mathText,2,-2);
 //if (substr(mathText,0,1)== '\$')   var src = 
 //             mathServer  + mathPreamble  + substr(mathText,1,-1);
var src =  mathServer  + mathPreamble;
    var alt = escHTML(mathText);

    var num = images.length;
    var html = '<span class="math" id="math' + num + '">' 
               + mathHtml 
               + '</span>';
    var img = images[num] = new Image();
    img.onload = function() {
if (substr(mathText,0,2)== '\\[')  var imgHtml = "<center><img class='mathimg' src='"+src+substr(mathText,2,-2)+"'  alt='" +alt + "' title='" + alt + "' /></center>";
if (substr(mathText,0,1)== '\$')  var imgHtml = "<img class='mathimg' src='"+src+"\\textstyle "+substr(mathText,1,-1)+"' align ='absmiddle' alt='" +alt + "' title='" + alt + "' />";
if (substr(mathText,0,2)== '\$\$')  var imgHtml = "<img class='mathimg' src='"+src+"\\displaystyle "+substr(mathText,2,-2)+"' align ='absmiddle' alt='" +alt + "' title='" + alt + "' />";

      var span = document.getElementById( 'math' + num );
      if ( span ) span.innerHTML = imgHtml;
      else html = imgHtml;
    };
    img.src = src;
    return html;
  }

  if ( !window.mathCleaner ) window.mathCleaner = function(orig) {
      var cleaned = orig.replace(/<\/?(br|p)\b.*?>/ig, '');
      return /[<>]/.test( cleaned ) ? orig : cleaned;
  };
  var replace = window.replaceMath = function(elem, replacer) {
    if ( !mathRegExp2.test( elem.innerHTML ) ) return;
    if ( /pre|code/.test( elem.tagName ) )
      return elem.innerHTML = elem.innerHTML.replace(/([\$\\])/g, escHTML);
    
    for ( var child = elem.firstChild; child; child = child.nextSibling )
      if ( child.tagName ) replace( child, replacer );

    var newHtml = elem.innerHTML.replace( mathRegExp, window.mathCleaner );
    if ( elem.innerHTML != newHtml ) elem.innerHTML = newHtml;
    
    for ( var child = elem.firstChild; child; child = child.nextSibling )
      if ( child.nodeType == 3 && mathRegExp2.test( child.nodeValue ) ) {
        // simple case for "only children"
        if ( child == elem.firstChild && !child.nextSibling ) {
          elem.innerHTML = 
            elem.innerHTML.replace( mathRegExp, replacer || replacement );
          continue;
        }
        var s = elem.insertBefore( document.createElement('span'), child );
        s.appendChild( child );
        child = s;
        child.innerHTML = 
          child.innerHTML.replace( mathRegExp, replacer || replacement );
      }
  }
})();
