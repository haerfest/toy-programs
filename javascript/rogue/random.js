var rogue = rogue || {};


rogue.init_random = function () {
    this.random = {
        rntb: [
	          3,
            0x9a319039, 0x32d9c024, 0x9b663182, 0x5da1f342, 
	          0xde3b81e0, 0xdf0a6fb5, 0xf103bc02, 0x48f340fb, 0x7449e56b,
	          0xbeb1dbb0, 0xab5c5918, 0x946554fd, 0x8c2e680f, 0xeb3d799f,
	          0xb11ee0b7, 0x2d436b86, 0xda672e2a, 0x1588ca88, 0xe369735d,
	          0x904f35f7, 0xd7158fd6, 0x6fa6f051, 0x616e6b96, 0xac94efdc, 
	          0x36413f93, 0xc622c298, 0xf5a42ab8, 0x8a88d77b, 0xf5ad9d0e,
	          0x8999220b, 0x27fb47b9
        ],
        fptr: 4,
        rptr: 1,
        state: 1,
        rand_type: 3,
        rand_deg: 31,
        rand_sep: 3,
        end_ptr: 32
    };
};


rogue.rrandom = function () {
    var i;

    if (this.random.rand_type === 0) {
        i = (this.random.rntb[this.random.state] * 1103515245 + 12345) & 0x7fffffff;
        this.random.rntb[this.random.state] = i;
    } else {
        this.random.rntb[this.random.fptr] += this.random.rntb[this.random.rptr];
        i = (this.random.rntb[this.random.fptr] >> 1) & 0x7fffffff;
        this.random.fptr += 1;
        if (this.random.fptr >= this.random.end_ptr) {
            this.random.fptr = this.random.state;
            this.random.rptr += 1;
        } else {
            this.random.rptr += 1;
            if (this.random.rptr >= this.random.end_ptr) {
                this.random.rptr = this.random.state;
            }
        }
    }

    return i;
};


rogue.get_rand = function (x, y) {
    var r,
        t,
        lr;

    if (x > y) {
        t = y;
        y = x;
        x = t;
    }

    lr = rrandom();
    lr = lr & 0x00003fff;
    r  = lr;
    r  = (r % ((y - x) + 1)) + x;

    return r;
}
