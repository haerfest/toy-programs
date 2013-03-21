var rogue = rogue || {};


rogue.init_room_locals = function () {
    this.room_locals = {
        rooms_visited: []
    };
};


rogue.is_all_connected = function () {
    var i,
        starting_room;

    for (i = 0; i < this.MAXROOMS; i += 1) {
        this.room_locals.rooms_visited[i] = false;
        if (this.rooms[i].is_room & (this.R_ROOM | this.R_MAZE)) {
            starting_room = i;
        }
    }

    this.visit_rooms(starting_room);

    for (i = 0; i < this.MAXROOMS; i += 1) {
        if (this.rooms[i].is_room & (this.R_ROOM | this.R_MAZE) &&
            !this.room_locals.rooms_visited[i]) {
            return false;
        }
    }

    return true;
};


rogue.visit_rooms = function (rn) {
    var i,
        oth_rn;

    this.room_locals.rooms_visited[rn] = true;

    for (i = 0; i < 4; i += 1) {
        oth_rn = this.rooms[rn].doors[i].oth_room;
        if (oth_rn >= 0 && !this.room_locals.rooms_visited[oth_rn]) {
            this.visit_rooms(oth_rn);
        }
    }
};
