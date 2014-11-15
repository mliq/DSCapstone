gaTrackerAttach = function () {

  $('a').click( function() {
    var trackDownload = Drupal.settings.googleanalytics.trackDownload;

    // Extract the domain from the location (the domain are in domain[2])
    var domain = /^(http|https):\/\/([a-z-.0-9]+)[\/]{0,1}/i.exec(window.location);
    // Expression for check internal links
    var internalLink = new RegExp("^(http|https):\/\/"+domain[2], "i");
    // Expression for check downloads
    var isDownload = new RegExp("("+trackDownload+")$", "i");

    if(internalLink.test(this.href)){

      // ... and if the extension are in this.trackDownload ...
      if(this.trackDownload && isDownload.test(this.href)) {
        // Clean and track the URL
        urchinTracker('/download/'+this.href.replace(/^(http|https):\/\/([a-z-.0-9]+)\//i, '').split('/').join('--'));
      }
    } else {
      // are external
      // Clean and track the URL
      urchinTracker('/outgoing/'+this.href.replace(/^http:\/\/|https:\/\//i, '').split('/').join('--'));
    }
  });
};

if (Drupal.jsEnabled) {
 $(document).ready(gaTrackerAttach);
}