var rogue = rogue || {};


rogue.init_level = function () {
    this.level = {
        cur_level: 0,
        max_level: 1,
        new_level_message: null,
        party_room: this.NO_ROOM,
        level_points: [
		        10,
		        20,
		        40,
		        80,
		        160,
		        320,
		        640,
		        1300,
		        2600,
		        5200,
	          10000,
	          20000,
	          40000,
	          80000,
	          160000,
	          320000,
	          1000000,
	          3333333,
	          6666666,
	          this.MAX_EXP,
	          99900000
        ],
        random_rooms: [3, 7, 5, 2, 0, 6, 1, 4, 8]
    };
};


rogue.make_level = function () {
    var i,
        j,
        must_1,
        must_2,
        must_3,
        big_room;

    if (this.level.cur_level < this.LAST_DUNGEON) {
        this.level.cur_level += 1;
    }

    if (this.level.cur_level > this.level.max_level) {
        this.level.max_level = this.level.cur_level;
    }

    must_1 = this.get_rand(0, 5);

    // TODO
};


rogue.clear_level = function () {
    var i,
        j;

    for (i = 0; i < this.MAXROOMS; i += 1) {
        this.rooms[i].is_room = this.R_NOTHING;
        for (j = 0; j < 4; j += 1) {
            this.rooms[i].doors[j].oth_room = this.NO_ROOM;
        }
    }

    for (i = 0; i < this.MAX_TRAPS; i += 1) {
        this.traps[i].trap_type = this.NO_TRAP;
    }

    for (i = 0; i < this.DROWS; i += 1) {
        for (j = 0; j < this.DCOLS; j += 1) {
            this.dungeon[i][j] = this.NOTHING;
        }
    }

    this.detect_monster = 0;
    this.see_invisible  = 0;
    this.being_held     = 0;
    this.bear_trap      = 0;
    this.party_room     = this.NO_ROOM;
    this.rogue.row      = -1;
    this.rogue.col      = -1;

    this.clear();
};
