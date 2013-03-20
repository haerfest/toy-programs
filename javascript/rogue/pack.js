var rogue = rogue || {};


rogue.init_pack_locals = function () {
};


rogue.mask_pack = function(pack, mask) {
    var i;

    for (i in pack) {
        if (pack[i].what_is & mask) {
            return true;
        }
    }

    return false;
};


rogue.has_amulet = function () {
    return this.mask_pack(this.rogue.pack, this.AMULET);
};
