var rogue = rogue || {};


rogue.level_init_locals = function () {
    this.level_locals = {
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

    if (this.level_locals.cur_level < this.LAST_DUNGEON) {
        this.level_locals.cur_level += 1;
    }

    if (this.level_locals.cur_level > this.level_locals.max_level) {
        this.level_locals.max_level = this.level_locals.cur_level;
    }

    must_1 = this.get_rand(0, 5);

    switch (must_1) {
    case 0:
        must_1 = 0;
        must_2 = 1;
        must_3 = 2;
        break;

    case 1:
		    must_1 = 3;
		    must_2 = 4;
		    must_3 = 5;
		    break;

	  case 2:
		    must_1 = 6;
		    must_2 = 7;
		    must_3 = 8;
		    break;

    case 3:
		    must_1 = 0;
		    must_2 = 3;
		    must_3 = 6;
		    break;

    case 4:
		    must_1 = 1;
		    must_2 = 4;
		    must_3 = 7;
		    break;

    case 5:
		    must_1 = 2;
		    must_2 = 5;
		    must_3 = 8;
		    break;
	  }

    if (this.rand_percent(8)) {
        this.level_locals.party_room = 0;
    }

    big_room = ((this.level_locals.party_room !== this.NO_ROOM) && this.rand_percent(1));
    if (big_room) {
        this.make_room(this.BIG_ROOM, 0, 0, 0);
    } else {
        for (i = 0; i < this.MAXROOMS; i += 1) {
            this.make_room(i, must_1, must_2, must_3);
        }
    }

    if (!big_room) {
        this.add_mazes();
        this.mix_random_rooms();

        for (j = 0; j < this.MAXROOMS; j += 1) {
            i = this.level_locals.random_rooms[j];

            if (i < this.MAXROOMS - 1) {
                this.connect_rooms(i, i + 1);
            }

            if (i < this.MAXROOMS - 3) {
                this.connect_rooms(i, i + 3);
            }

            if (i < this.MAXROOMS - 2) {
                if (this.rooms[i + 1].is_room & this.R_NOTHING) {
                    if (this.connect_rooms(i, i + 2)) {
                        this.rooms[i + 1].is_room = this.R_CROSS;
                    }
                }
            }
            
            if (i < this.MAXROOMS - 6) {
                if (this.rooms[i + 3].is_room & this.R_NOTHING) {
                    if (this.connect_rooms(i, i + 6)) {
                        this.rooms[i + 3].is_room = this.R_CROSS;
                    }
                }
            }

            if (this.is_all_connected()) {
                break;
                }
        }

        this.fill_out_level();
    }

    if (!this.has_amulet() && (this.level_locals.cur_level >= this.AMULET_LEVEL)) {
        this.put_amulet();
    }
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
