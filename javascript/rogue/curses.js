var rogue = rogue || {};


rogue.init_curses_locals = function () {
    var i;

    this.curses_locals = {};
    
    this.curses_locals.terminal = new Array(this.DROWS);
    for (i = 0; i < this.DROWS; i += 1) {
        this.curses_locals.terminal[i] = new Array(this.DCOLS);
    }

    this.curses_locals.buffer = new Array(this.DROWS);
    for (i = 0; i < this.DROWS; i += 1) {
        this.curses_locals.buffer[i] = new Array(this.DCOLS);
    }

    this.curses_locals.lines_dirty = new Array(this.DROWS);
    
    this.curses_locals.curscr = {};

    this.screen = document.getElementById('screen');
    this.space  = '\u00A0';
};


rogue.initscr = function () {
    this.clear();
    // FIXME: enable cursor
};


rogue.move = function (row, col) {
    this.curses_locals.curscr._cury = row;
    this.curses_locals.curscr._curx = col;
    this.curses_locals.screen_dirty = 1;
};


rogue.clear = function () {
    this.screen.textContent = '';

    this.curses_locals.cur_row = 0;
    this.curses_locals.cur_col = 0;

    this.move(0, 0);
    this.clear_buffers();
};


rogue.clear_buffers = function () {
    var i,
        j;

    this.curses_locals.screen_dirty = 0;

    for (i = 0; i < this.DROWS; i += 1) {
        this.curses_locals.lines_dirty[i] = 0;

        for (j = 0; j < this.DCOLS; j += 1) {
            this.curses_locals.terminal[i][j] = this.space;
            this.curses_locals.buffer[i][j]   = this.space;
        }
    }
};
