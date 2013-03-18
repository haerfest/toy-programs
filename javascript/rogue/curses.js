var rogue = rogue || {};


rogue.init_curses = function () {
    var i;

    this.curses = {};
    
    this.curses.terminal = new Array(this.DROWS);
    for (i = 0; i < this.DROWS; i += 1) {
        this.curses.terminal[i] = new Array(this.DCOLS);
    }

    this.curses.buffer = new Array(this.DROWS);
    for (i = 0; i < this.DROWS; i += 1) {
        this.curses.buffer[i] = new Array(this.DCOLS);
    }

    this.curses.lines_dirty = new Array(this.DROWS);
    
    this.curses.curscr = {};

    this.screen = document.getElementById('screen');
    this.space  = '\u00A0';
};


rogue.move = function (row, col) {
    this.curses.curscr._cury = row;
    this.curses.curscr._curx = col;
    this.curses.screen_dirty = 1;
};


rogue.clear = function () {
    this.screen.textContent = '';

    this.curses.cur_row = 0;
    this.curses.cur_col = 0;

    this.move(0, 0);
    this.clear_buffers();
};


rogue.clear_buffers = function () {
    var i,
        j;

    this.curses.screen_dirty = 0;

    for (i = 0; i < this.DROWS; i += 1) {
        this.curses.lines_dirty[i] = 0;

        for (j = 0; j < this.DCOLS; j += 1) {
            this.curses.terminal[i][j] = this.space;
            this.curses.buffer[i][j]   = this.space;
        }
    }
};
