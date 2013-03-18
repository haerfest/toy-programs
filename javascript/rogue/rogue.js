var rogue = rogue || {};


rogue.header = function () {
    var i;
    
    this.DROWS = 24;
    this.DCOLS = 80;
    this.NO_ROOM = -1;
    this.R_NOTHING = 1;

    this.dungeon = new Array(this.DROWS);
    for (i = 0; i < this.DCOLS; i += 1) {
        this.dungeon[i] = new Array(this.DCOLS);
    }

    this.rogue = {};
};

rogue.init = function () {
    this.init_curses();
    this.init_random();
    this.init_level();
};


window.onload = function () {
    rogue.header();
    rogue.init();
    rogue.main();
};
