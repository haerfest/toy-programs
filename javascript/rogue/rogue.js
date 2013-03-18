var rogue = rogue || {};


rogue.header = function () {
    var i;
    
    this.DROWS = 24;
    this.DCOLS = 80;

    this.MAXROOMS =  9;
    this.BIG_ROOM = 10;
    this.NO_ROOM  = -1;
    
    this.R_NOTHING = 1;

    this.rogue = {};
    this.rooms = [];
    
    this.dungeon = new Array(this.DROWS);
    for (i = 0; i < this.DCOLS; i += 1) {
        this.dungeon[i] = new Array(this.DCOLS);
    }
};


rogue.init_locals = function () {
    this.object_init_locals();
    this.curses_init_locals();
    this.random_init_locals();
    this.inventory_init_locals();
    this.init_init_locals();
    this.level_init_locals();
};


window.onload = function () {
    rogue.header();
    rogue.init_locals();
    rogue.init();
    rogue.main();
};
