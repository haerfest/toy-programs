/**
 * Fetches the daily ESV verse via AJAX.
 *
 * @param {function} callback  Will be called with the daily verse text.
 *
 * @note This example may fail on certain browsers because of CORS (Cross
 *       Origin Resource Sharing) limitations (Firefox, Chrome) or because
 *       of a different AJAX API (older versions of Internet Explorer).
 */
function fetchDailyVerse(callback) {
    'use strict';

    var url = 'http://www.esvapi.org/v2/rest/dailyVerse' + '?' +
              'key=IP'                                   + '&' +
              'output-format=plain-text'                 + '&' +
              'include-first-verse-numbers=false'        + '&' +
              'include-verse-numbers=false'              + '&' +
              'include-footnotes=false'                  + '&' +
              'include-short-copyright=false'            + '&' +
              'include-passage-horizontal-lines=false'   + '&' +
              'include-heading-horizontal-lines=false'   + '&' +
              'include-headings=false'                   + '&' +
              'include-subheadings=false',
        httpRequest;

    httpRequest = new XMLHttpRequest();
    httpRequest.onreadystatechange = function () {
        if (httpRequest.readyState === XMLHttpRequest.DONE &&
                  httpRequest.status === 200) {
            callback(httpRequest.responseText);
        }
    };

    // Make an asynchronous request (true).
    httpRequest.open('GET', url, true);
    httpRequest.send();
}


// Fetch the daily verse and update the DOM.
fetchDailyVerse(function (text) {
    'use strict';

    var verse,
        lines,
        line,
        i;

    // Remove all child nodes of the #verse element.
    verse = document.getElementById('verse');
    while (verse.hasChildNodes()) {
        verse.removeChild(verse.lastChild);
    }

    // Separate each verse line by <br> tags, and use no-break spaces.
    lines = text.split('\n');
    for (i in lines) {
        line = lines[i].replace(' ', '\u00A0');
        verse.appendChild(document.createTextNode(line));
        verse.appendChild(document.createElement('br'));
    }
});
