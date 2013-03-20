var rogue = rogue || {};


rogue.init_object_locals = function () {
    this.rogue = {
        armor:       this.INIT_ARMOR,
        weapon:      this.INIT_WEAPON,
        left_ring:   this.INIT_LEFT_RING,
        right_ring:  this.INIT_RIGHT_RING,
        hp_current:  this.INIT_HP_CURR,
        hp_max:      this.INIT_HP_MAX,
        str_current: this.INIT_STR_CURR,
        str_max:     this.INIT_STR_MAX,
        pack:        this.INIT_PACK,
        gold:        this.INIT_GOLD,
        exp:         this.INIT_EXP,
        exp_points:  this.INIT_EXP_POINTS,
        row:         0,
        col:         0,
        fchar:       this.INIT_CHAR,
        moves_left:  this.INIT_MOVES
    };

    this.object_locals = {
        id_potions: [
            { value: 100, title: 'blue \0                           ', real: 'of increase strength ', id_status: 0},
            { value: 250, title: 'red \0                            ', real: 'of restore strength ', id_status: 0},
            { value: 100, title: 'green \0                          ', real: 'of healing ', id_status: 0},
            { value: 200, title: 'grey \0                           ', real: 'of extra healing ', id_status: 0},
            { value: 10,  title: 'brown \0                          ', real: 'of poison ', id_status: 0},
            { value: 300, title: 'clear \0                          ', real: 'of raise level ', id_status: 0},
            { value: 10,  title: 'pink \0                           ', real: 'of blindness ', id_status: 0},
            { value: 25,  title: 'white \0                          ', real: 'of hallucination ', id_status: 0},
            { value: 100, title: 'purple \0                         ', real: 'of detect monster ', id_status: 0},
            { value: 100, title: 'black \0                          ', real: 'of detect things ', id_status: 0},
            { value: 10,  title: 'yellow \0                         ', real: 'of confusion ', id_status: 0},
            { value: 80,  title: 'plaid \0                          ', real: 'of levitation ', id_status: 0},
            { value: 150, title: 'burgundy \0                       ', real: 'of haste self ', id_status: 0},
            { value: 145, title: 'beige \0                          ', real: 'of see invisible ', id_status: 0}
        ]
    };
};
