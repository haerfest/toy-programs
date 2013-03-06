/**
 * Throws one dart at a quarter circle of radius one and returns whether it hit
 * the quarter circle or not.
 *
 * @return {boolean} True when the dart hit the quarter circle.
 */
function throwOne() {
    'use strict';

    var x = Math.random(),
        y = Math.random();

    return Math.sqrt(x * x + y * y) <= 1;
}


/**
 * Returns an estimate of pi by throwing darts at a quarter circle.
 *
 * @param {number} throwCount  The number of darts to throw.
 *
 * @return {number} An estimation of pi.
 */
function estimatePi(throwCount) {
    'use strict';

    var hitCount = 0,
        i;

    for (i = 0; i < throwCount; i += 1) {
        if (throwOne()) {
            hitCount += 1;
        }
    }

    return 4 * hitCount / throwCount;
}
