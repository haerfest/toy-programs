var rogue = rogue || {};


rogue.init_level_locals = function () {
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

    big_room = (this.level_locals.party_room !== this.NO_ROOM &&
                this.rand_percent(1));
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

            if (this.is_all_connected()) { // FIXME
                break;
            }
        }

        this.fill_out_level(); // FIXME
    }

    if (!this.has_amulet() &&
        this.level_locals.cur_level >= this.AMULET_LEVEL) {
        this.put_amulet(); // FIXME
    }
};


rogue.make_room = function(rn, r1, r2, r3) {
    var isBigRoom    = (rn === this.BIGROOM),
        doBuildWalls = true,
        left_col,
        right_col,
        top_row,
        bottom_row,
        width,
        height,
        row_offset,
        col_offset,
        i,
        j,
        ch;

    switch (rn) {
    case 0:
        left_col   = 0;
        right_col  = this.COL1 - 1;
        top_row    = this.MIN_ROW;
        bottom_row = this.ROW1 - 1;
        break;

    case 1:
        left_col   = this.COL1 + 1;
        right_col  = this.COL2 - 1;
        top_row    = this.MIN_ROW;
        bottom_row = this.ROW1 - 1;
        break;

    case 2:
        left_col   = this.COL2 + 1;
        right_col  = this.DCOLS - 1;
        top_row    = this.MIN_ROW;
        bottom_row = this.ROW1 - 1;
        break;

    case 3:
        left_col   = 0;
        right_col  = this.COL1 - 1;
        top_row    = this.ROW1 + 1;
        bottom_row = this.ROW2 - 1;
        break;

    case 4:
        left_col   = this.COL1 + 1;
        right_col  = this.COL2 - 1;
        top_row    = this.ROW1 + 1;
        bottom_row = this.ROW2 - 1;
        break;

    case 5:
        left_col   = this.COL2+1;
        right_col  = this.DCOLS-1;
        top_row    = this.ROW1+1;
        bottom_row = this.ROW2-1;
        break;

    case 6:
        left_col   = 0;
        right_col  = this.COL1 - 1;
        top_row    = this.ROW2 + 1;
        bottom_row = this.DROWS - 2;
        break;

    case 7:
        left_col   = this.COL1 + 1;
        right_col  = this.COL2 - 1;
        top_row    = this.ROW2 + 1;
        bottom_row = this.DROWS - 2;
        break;

    case 8:
        left_col   = this.COL2 + 1;
        right_col  = this.DCOLS - 1;
        top_row    = this.ROW2 + 1;
        bottom_row = this.DROWS - 2;
        break;

    case this.BIG_ROOM:
        top_row    = this.get_rand(this.MIN_ROW, this.MIN_ROW + 5);
        bottom_row = this.get_rand(this.DROWS - 7, this.DROWS - 2);
        left_col   = this.get_rand(0, 10);;
        right_col  = this.get_rand(this.DCOLS - 11, this.DCOLS - 1);
        rn         = 0;
        break;
    }

    if (!isBigRoom) {
        height = this.get_rand(4, bottom_row - top_row + 1);
        width  = this.get_rand(7, right_col - left_col - 2);

        row_offset = this.get_rand(0, (bottom_row - top_row) - height + 1);
        col_offset = this.get_rand(0, (right_col - left_col) - width + 1);

        top_row    += row_offset;
        bottom_row  = top_row + height - 1;

        left_col  += col_offset;
        right_col  = left_col + width - 1;

        if (rn != r1 && rn != r2 && rn != r3 && this.rand_percent(40)) {
            doBuildWalls = false;
        }
    }

    if (doBuildWalls) {
        this.rooms[rn].is_room = this.R_ROOM;

        for (i = top_row; i <= bottom_row; i++) {
            for (j = left_col; j <= right_col; j++) {
                if (i === top_row || i === bottom_row) {
                    ch = this.HORWALL;
                } else if ((i !== top_row && i !== bottom_row) &&
                           (j === left_col || j === right_col)) {
                    ch = this.VERTWALL;
                } else {
                    ch = this.FLOOR;
                }
                this.dungeon[i][j] = ch;
            }
        }
    }

    this.rooms[rn].top_row    = top_row;
    this.rooms[rn].bottom_row = bottom_row;
    this.rooms[rn].left_col   = left_col;
    this.rooms[rn].right_col  = right_col;
};


rogue.connect_rooms = function (room1, room2) {
    var row1,
        col1,
        row2,
        col2,
        dir;

    if (!(this.rooms[room1].is_room & (this.R_ROOM | this.R_MAZE)) ||
        !(this.rooms[room2].is_room & (this.R_ROOM | this.R_MAZE))) {
        return 0;
    }

    if (this.same_row(room1, room2) &&
        this.rooms[room1].left_col > this.rooms[room2].right_col) {
        [row1, col1] = this.put_door(this.rooms[room1], this.LEFT);
        [row2, col2] = this.put_door(this.rooms[room2], this.RIGHT);
    } else if (this.same_row(room1, room2) &&
               (this.rooms[room2].left_col > this.rooms[room1].right_col)) {
        [row1, col1] = this.put_door(this.rooms[room1], this.RIGHT);
        [row2, col2] = this.put_door(this.rooms[room2], this.LEFT);
    } else if (this.same_col(room1, room2) &&
               (this.rooms[room1].top_row > this.rooms[room2].bottom_row)) {
        [row1, col1] = this.put_door(this.rooms[room1], this.UPWARD);
        [row2, col2] = this.put_door(this.rooms[room2], this.DOWN);
    } else if (this.same_col(room1, room2) &&
               (this.rooms[room2].top_row > this.rooms[room1].bottom_row)) {
        [row1, col1] = this.put_door(this.rooms[room1], this.DOWN);
        [row2, col2] = this.put_door(this.rooms[room2], this.UPWARD);
    } else {
        return 0;
    }

    do {
        this.draw_simple_passage(row1, col1, row2, col2, dir);
    } while (this.rand_percent(4));

    this.rooms[room1].doors[Math.floor(dir / 2)].oth_room = room2;
    this.rooms[room1].doors[Math.floor(dir / 2)].oth_row  = row2;
    this.rooms[room1].doors[Math.floor(dir / 2)].oth_col  = col2;

    this.rooms[room2].doors[Math.floor(((dir + 4) % this.DIRS) / 2)].oth_room = room1;
    this.rooms[room2].doors[Math.floor(((dir + 4) % this.DIRS) / 2)].oth_row  = row1;
    this.rooms[room2].doors[Math.floor(((dir + 4) % this.DIRS) / 2)].oth_col  = col1;

    return 1;
};


rogue.clear_level = function () {
    var i,
        j;

    for (i = 0; i < this.MAXROOMS; i += 1) {
        this.rooms[i] = {
            is_room: this.R_NOTHING,
            doors: [
                { oth_room: this.NO_ROOM },
                { oth_room: this.NO_ROOM },
                { oth_room: this.NO_ROOM },
                { oth_room: this.NO_ROOM }
            ]
        };
    }

    for (i = 0; i < this.MAX_TRAPS; i += 1) {
        this.traps[i] = {
            trap_type: this.NO_TRAP
        };
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


rogue.put_door = function(rm, dir, row, col) {
    var wall_width = (rm.is_room & this.R_MAZE ? 0 : 1);

    switch (dir) {
    case this.UPWARD:
    case this.DOWN:
        row = (dir === this.UPWARD ? rm.top_row : rm.bottom_row);
        do {
            col = this.get_rand(rm.left_col + wall_width,
                                rm.right_col - wall_width);
        } while (!this.dungeon[row][col] & (this.HORWALL | this.TUNNEL));
        break;

    case this.RIGHT:
    case this.LEFT:
        col = (dir === this.LEFT ? rm.left_col : rm.right_col);
        do {
            row = this.get_rand(rm.top_row + wall_width,
                                rm.bottom_row - wall_width);
        } while (!this.dungeon[row][col] & (this.VERTWALL | this.TUNNEL));
        break;
    }

    if (rm.is_room & this.R_ROOM) {
        this.dungeon[row][col] = this.DOOR;
    }

    if (this.level_locals.cur_level > 2 && this.rand_percent(this.HIDE_PERCENT)) {
        this.dungeon[row][col] |= this.HIDDEN;
    }

    rm.doors[Math.floor(dir / 2)].door_row = row;
    rm.doors[Math.floor(dir / 2)].door_col = col;

    return [row, col];
};


rogue.draw_simple_passage = function(row1, col1, row2, col2, dir) {
    var i,
        middle,
        t;

    if (dir === this.LEFT || dir === this.RIGHT) {
        if (col1 > col2) {
            [row1, row2] = [row2, row1];
            [col1, col2] = [col2, col1];
        }

        middle = this.get_rand(col1 + 1, col2 - 1);

        for (i = col1 + 1; i !== middle; i += 1) {
            this.dungeon[row1][i] = this.TUNNEL;
        }

        for (i = row1; i !== row2; i += (row1 > row2 ? -1 : 1)) {
            this.dungeon[i][middle] = this.TUNNEL;
        }

        for (i = middle; i !== col2; i += 1) {
            this.dungeon[row2][i] = this.TUNNEL;
        }
    } else  {
        if (row1 > row2) {
            [row1, row2] = [row2, row1];
            [col1, col2] = [col2, col1];
        }

        middle = this.get_rand(row1 + 1, row2 - 1);

        for (i = row1 + 1; i !== middle; i += 1) {
            this.dungeon[i][col1] = this.TUNNEL;
        }

        for (i = col1; i !== col2; i += (col1 > col2 ? -1 : 1)) {
            this.dungeon[middle][i] = this.TUNNEL;
        }

        for (i = middle; i !== row2; i += 1) {
            this.dungeon[i][col2] = this.TUNNEL;
        }
    }

    if (this.rand_percent(this.HIDE_PERCENT)) {
        this.hide_boxed_passage(row1, col1, row2, col2, 1);
    }
};


rogue.same_row = function (room1, room2) {
    return (Math.floor(room1 / 3) === Math.floor(room2 / 3));
};


rogue.same_col = function (room1, room2) {
    return (Math.floor(room1 % 3) === Math.floor(room2 % 3));
};


rogue.add_mazes = function () {
    var i,
        j,
        start,
        maze_percent;

    if (this.level_locals.cur_level > 1) {
        start = this.get_rand(0, this.MAXROOMS - 1);
        maze_percent = this.level_locals.cur_level * 5 / 4;

        if (this.level_locals.cur_level > 15) {
            maze_percent += this.level_locals.cur_level;
        }

        for (i = 0; i < this.MAXROOMS; i += 1) {
            j = (start + i) % this.MAXROOMS;

            if (this.rooms[j].is_room & this.R_NOTHING) {
                if (this.rand_percent(maze_percent)) {
                    this.rooms[j].is_room = this.R_MAZE;

                    this.make_maze(this.get_rand(this.rooms[j].top_row + 1, this.rooms[j].bottom_row - 1),
                                   this.get_rand(this.rooms[j].left_col + 1, this.rooms[j].right_col - 1),
                                   this.rooms[j].top_row,
                                   this.rooms[j].bottom_row,
                                   this.rooms[j].left_col,
                                   this.rooms[j].right_col);

                    this.hide_boxed_passage(this.rooms[j].top_row,
                                            this.rooms[j].left_col,
                                            this.rooms[j].bottom_row,
                                            this.rooms[j].right_col,
                                            this.get_rand(0, 2));
                }
            }
        }
    }
};


rogue.make_maze = function (r, c, tr, br, lc, rc) {
    var dirs = [this.UPWARD, this.DOWN, this.LEFT, this.RIGHT],
        i,
        t,
        t1,
        t2;

    this.dungeon[r][c] = this.TUNNEL;

    if (this.rand_percent(20)) {
        for (i = 0; i < 10; i += 1) {
            t1 = this.get_rand(0, 3);
            t2 = this.get_rand(0, 3);

            [dirs[t1], dirs[t2]] = [dirs[t2], dirs[t1]];
        }
    }

    for (i = 0; i < 4; i += 1) {
        switch (dirs[i]) {
        case this.UPWARD:
            if (r - 1 >= tr &&
                this.dungeon[r-1][c]   !== this.TUNNEL &&
                this.dungeon[r-1][c-1] !== this.TUNNEL &&
                this.dungeon[r-1][c+1] !== this.TUNNEL &&
                this.dungeon[r-2][c]   !== this.TUNNEL) {
                this.make_maze(r - 1, c, tr, br, lc, rc);
            }
            break;

        case this.DOWN:
            if (r + 1 <= br &&
                this.dungeon[r+1][c]   !== this.TUNNEL &&
                this.dungeon[r+1][c-1] !== this.TUNNEL &&
                this.dungeon[r+1][c+1] !== this.TUNNEL &&
                this.dungeon[r+2][c]   !== this.TUNNEL) {
                this.make_maze(r + 1, c, tr, br, lc, rc);
            }
            break;

        case this.LEFT:
            if (c - 1 >= lc &&
                this.dungeon[r][c-1]   !== this.TUNNEL &&
                this.dungeon[r-1][c-1] !== this.TUNNEL &&
                this.dungeon[r+1][c-1] !== this.TUNNEL &&
                this.dungeon[r][c-2]   !== this.TUNNEL) {
                this.make_maze(r, c - 1, tr, br, lc, rc);
            }
            break;

        case this.RIGHT:
            if (c + 1 <= rc &&
                this.dungeon[r][c+1]   !== this.TUNNEL &&
                this.dungeon[r-1][c+1] !== this.TUNNEL &&
                this.dungeon[r+1][c+1] !== this.TUNNEL &&
                this.dungeon[r][c+2]   !== this.TUNNEL) {
                this.make_maze(r, c + 1, tr, br, lc, rc);
            }
            break;
        }
    }
};


rogue.hide_boxed_passage = function (row1, col1, row2, col2, n) {
    var i,
        j,
        t,
        row,
        col,
        row_cut,
        col_cut,
        h,
        w;

    if (this.level_locals.cur_level > 2) {
        if (row1 > row2) {
            [row1, row2] = [row2, row1];
        }
        if (col1 > col2) {
            [col1, col2] = [col2, col1];
        }

        h = row2 - row1;
        w = col2 - col1;

        if (w >= 5 || h >= 5) {
            row_cut = (h >= 2 ? 1 : 0);
            col_cut = (w >= 2 ? 1 : 0);

            for (i = 0; i < n; i += 1) {
                for (j = 0; j < 10; j += 1) {
                    row = this.get_rand(row1 + row_cut, row2 - row_cut);
                    col = this.get_rand(col1 + col_cut, col2 - col_cut);

                    if (this.dungeon[row][col] === this.TUNNEL) {
                        this.dungeon[row][col] |= this.HIDDEN;
                        break;
                    }
                }
            }
        }
    }
};


rogue.mix_random_rooms = function () {
    var i,
        t,
        x,
        y;

    for (i = 0; i < 3 * this.MAXROOMS; i += 1) {
        do {
            x = this.get_rand(0, this.MAXROOMS - 1);
            y = this.get_rand(0, this.MAXROOMS - 1);
        } while (x === y);

        [this.level_locals.random_room[x], this.level_locals.random_rooms[y]] = [this.level_locals.random_rooms[y], this.level_locals.random_rooms[x]];
    }
};
