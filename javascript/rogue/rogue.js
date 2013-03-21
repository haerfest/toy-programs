var rogue = rogue || {};


rogue.header = function () {
    var i;

    this.NOTHING  = 0;
    this.OBJECT   = 01;
    this.MONSTER  = 02;
    this.STAIRS   = 04;
    this.HORWALL  = 010;
    this.VERTWALL = 020;
    this.DOOR     = 040;
    this.FLOOR    = 0100;
    this.TUNNEL   = 0200;
    this.TRAP     = 0400;
    this.HIDDEN   = 01000;

    this.ARMOR       = 01;
    this.WEAPON      = 02;
    this.SCROL       = 04;
    this.POTION      = 010;
    this.GOLD        = 020;
    this.FOOD        = 040;
    this.WAND        = 0100;
    this.RING        = 0200;
    this.AMULET      = 0400;
    this.ALL_OBJECTS = 0777;

    this.POTIONS = 14;

    this.NO_TRAP           = -1;
    this.TRAP_DOOR         = 0;
    this.BEAR_TRAP         = 1;
    this.TELE_TRAP         = 2;
    this.DART_TRAP         = 3;
    this.SLEEPING_GAS_TRAP = 4;
    this.RUST_TRAP         = 5;
    this.TRAPS             = 6;

    this.STEALTH_FACTOR = 3;
    this.R_TELE_PERCENT = 8;

    this.UNIDENTIFIED = 0;
    this.IDENTIFIED   = 01;
    this.CALLED       = 02;

    this.DROWS            = 24;
    this.DCOLS            = 80;
    this.NMESSAGES        = 5;
    this.MAX_TITLE_LENGTH = 30;
    this.MAXSYLLABLES     = 40;
    this.MAX_METAL        = 14;
    this.WAND_MATERIALS   = 30;
    this.GEMS             = 14;

    this.GOLD_PERCENT = 46;

    this.MAX_OPT_LEN = 40;

    this.INIT_ARMOR      = null;
    this.INIT_WEAPON     = null;
    this.INIT_LEFT_RING  = null;
    this.INIT_RIGHT_RING = null;
    this.INIT_HP_CURR    = 12;
    this.INIT_HP_MAX     = 12;
    this.INIT_STR_CURR   = 16;
    this.INIT_STR_MAX    = 16;
    this.INIT_EXP        = 1;
    this.INIT_EXP_POINTS = 0;
    this.INIT_PACK       = [];
    this.INIT_GOLD       = 0;
    this.INIT_CHAR       = '@';
    this.INIT_MOVES      = 1250;

    this.MAXROOMS =  9;
    this.BIG_ROOM = 10;
    this.NO_ROOM  = -1;

    this.PASSAGE = -3;

    this.AMULET_LEVEL = 26;

    this.R_NOTHING = 01;
    this.R_ROOM    = 02;
    this.R_MAZE    = 04;
    this.R_DEADEND = 010;
    this.R_CROSS   = 020;

    this.MAX_EXP_LEVEL = 21;
    this.MAX_EXP       = 10000001;
    this.MAX_GOLD      = 999999;
    this.MAX_ARMOR     = 99;
    this.MAX_HP        = 999;
    this.MAX_STRENGTH  = 99;
    this.LAST_DUNGEON  = 99;

    this.STAT_LEVEL    = 01;
    this.STAT_GOLD     = 02;
    this.STAT_HP       = 04;
    this.STAT_STRENGTH = 010;
    this.STAT_ARMOR    = 020;
    this.STAT_EXP      = 040;
    this.STAT_HUNGER   = 0100;
    this.STAT_LABEL    = 0200;
    this.STAT_ALL      = 0377;

    this.PARTY_TIME = 10;

    this.MAX_TRAPS = 10;

    this.HIDE_PERCENT = 12;

    this.rogue = {};
    this.rooms = [];
    this.traps = [];

    this.dungeon = [];
    for (i = 0; i < this.DCOLS; i += 1) {
        this.dungeon[i] = [];
    }

    this.UPWARD    = 0;
    this.UPRIGHT   = 1;
    this.RIGHT     = 2;
    this.DOWNRIGHT = 3;
    this.DOWN      = 4;
    this.DOWNLEFT  = 5;
    this.LEFT      = 6;
    this.UPLEFT    = 7;
    this.DIRS      = 8;

    this.ROW1 = 7;
    this.ROW2 = 15;

    this.COL1 = 26;
    this.COL2 = 52;

    this.MIN_ROW = 1;
};


rogue.init_locals = function () {
    this.init_object_locals();
    this.init_pack_locals();
    this.init_curses_locals();
    this.init_random_locals();
    this.init_inventory_locals();
    this.init_init_locals();
    this.init_room_locals();
    this.init_level_locals();
};


window.onload = function () {
    rogue.header();
    rogue.init_locals();
    rogue.init();
    rogue.main();
};
