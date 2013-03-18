var rogue = rogue || {};


rogue.init_init_locals = function () {
    this.init_locals = {
        
    };
    
};


rogue.init = function () {
    var seed;
    
    this.initscr();
    this.init_locals.init_curses = 1;

    seed = this.md_gseed();
    this.srrandom(seed);

    this.mix_colors();
    
    // TODO
};
