window.onload = function () {
    rascal(25, 80);
}

function rascal(rowCount, colCount) {
    var playfield = document.getElementById('playfield'),
        level     = new Array(rowCount),
        emptyTile = '\u00A0',  // No-break space.
        leftKey   = 37,
        upKey     = 38,
        rightKey  = 39,
        downKey   = 40,
        hKey      = 72,
        jKey      = 74,
        kKey      = 75,
        lKey      = 76,
        guyRow,
        guyCol;
    
    function createLevel() {
        var row,
            col,
            line = '';

        for (col = 0; col < colCount; col += 1) {
            line += emptyTile;
        }
        
        for (row = 0; row < rowCount; row += 1) {
            level[row] = line;
        }

        guyRow    = Math.floor(rowCount / 2);
        guyCol = Math.floor(colCount / 2);
    }

    function setChar(string, pos, ch) {
        return string.substr(0, pos) + ch + string.substr(pos + 1);
    }

    function deepCopyLevel() {
        var copy = new Array(rowCount),
            row;

        for (row = 0; row < rowCount; row += 1) {
            copy[row] = String(level[row]);
        }

        return copy;
    }
    
    function refresh() {
        var flattened = deepCopyLevel();
        
        flattened[guyRow] = setChar(level[guyRow], guyCol, '@');

        playfield.innerHTML = flattened.join('<br>');
    }

    function canGoThere(row, col) {
        return (row >= 0 && row < rowCount &&
                col >= 0 && col < colCount);
    }

    function goThere(row, col) {
        guyRow    = row;
        guyCol = col;
    }
    
    function onKeyDown(event) {
        var intendedRow = guyRow,
            intendedCol = guyCol;
        
        switch (event.keyCode) {
        case upKey:
        case kKey:
            intendedRow -= 1;
            break;

        case downKey:
        case jKey:
            intendedRow += 1;
            break;

        case leftKey:
        case hKey:
            intendedCol -= 1;
            break;

        case rightKey:
        case lKey:
            intendedCol += 1;
            break;

        default:
            return;
        }

        if (canGoThere(intendedRow, intendedCol)) {
            goThere(intendedRow, intendedCol);
            refresh();
        }
    }
    
    function initialize() {
        window.onkeydown = onKeyDown;
        createLevel();
    }

    initialize();
    refresh();
}

