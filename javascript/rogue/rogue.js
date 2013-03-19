var rogue = rogue || {};


rogue.header = function () {
    var i;

    this.POTIONS = 14;
    
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
    this.init_object_locals();
    this.init_curses_locals();
    this.init_random_locals();
    this.init_inventory_locals();
    this.init_init_locals();
    this.init_level_locals();
};


window.onload = function () {
    rogue.header();
    rogue.init_locals();
    rogue.init();
    rogue.main();
};
