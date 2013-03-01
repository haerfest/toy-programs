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

    
