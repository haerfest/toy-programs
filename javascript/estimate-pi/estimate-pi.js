/**
 * Throws one dart at a quarter circle of radius one and returns whether it hit
 * the quarter circle or not.
 *
 * @return {boolean} True when the dart hit the quarter circle.
 */
function throwOne () {
    var x = Math.random();
    var y = Math.random();

    return Math.sqrt(x*x + y*y) <= 1;
}


/**
 * Returns an estimate of pi by throwing darts at a quarter circle.
 *
 * @param {number} throwCount  The number of darts to throw.
 *
 * @return {number} An estimation of pi.
 */
function estimatePi (throwCount) {
    var hitCount = 0;
    
    for (i = 0; i < throwCount; i++) {
        if (throwOne()) {
            hitCount++;
        }
    }

    return 4 * hitCount / throwCount;
}


/**
 * Event handler to trigger an estimation of pi.
 *
 * Retrieves the number of throws from the element identified by 'throwCount',
 * estimates pi by throwing that many darts, and sets the value of the element
 * identified by 'pi' to the estimation.
 */
function estimatePiEvent () {
    var throwCount = document.getElementById('throwCount').value;
    var pi         = estimatePi(throwCount);

    document.getElementById('pi').value = pi;
}


/**
 * Returns whether the script is being run in a browser.
 *
 * @return {boolean} True when running in a browser.
 */
function isRunningInBrowser () {
    return typeof(document) != "undefined";
}


/**
 * If we run from the command-line (jsc on Mac OS X), expect
 * the number of throws as the only parameter, and print an
 * estimation of pi.
 */
if (!isRunningInBrowser()) {
    var throwCount = arguments[0] ? parseInt(arguments[0]) : 1000;
    var pi         = estimatePi(throwCount);

    print('pi = ' + pi);
}
