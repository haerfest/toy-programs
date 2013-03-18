var rogue = rogue || {};


rogue.random_init_locals = function () {
    this.random_locals = {
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


rogue. srrandom = function (x) {
    var i;

    this.random_locals.rntb[this.random_locals.state] = x;
    if (this.random_locals.rand_type !== 0) {
        for (i = 0; i < this.random_locals.rand_deg; i += 1) {
            this.random_locals.rntb[this.random_locals.state + i] = 1103515245 * this.random_locals.rntb[this.random_locals.state + i - 1] + 12345;
        }

        this.random_locals.fptr = this.random_locals.rntb[this.random_locals.state + this.random_locals.rand_sep];
        this.random_locals.rptr = this.random_locals.rntb[this.random_locals.state];

        for (i = 0; i < 10 * this.random_locals.rand_deg; i += 1) {
            this.rrandom();
        }
    }
};


rogue.rrandom = function () {
    var i;

    if (this.random_locals.rand_type === 0) {
        i = (this.random_locals.rntb[this.random_locals.state] * 1103515245 + 12345) & 0x7fffffff;
        this.random_locals.rntb[this.random_locals.state] = i;
    } else {
        this.random_locals.rntb[this.random_locals.fptr] += this.random_locals.rntb[this.random_locals.rptr];
        i = (this.random_locals.rntb[this.random_locals.fptr] >> 1) & 0x7fffffff;
        this.random_locals.fptr += 1;
        if (this.random_locals.fptr >= this.random_locals.end_ptr) {
            this.random_locals.fptr = this.random_locals.state;
            this.random_locals.rptr += 1;
        } else {
            this.random_locals.rptr += 1;
            if (this.random_locals.rptr >= this.random_locals.end_ptr) {
                this.random_locals.rptr = this.random_locals.state;
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

    lr = this.rrandom();
    lr = lr & 0x00003fff;
    r  = lr;
    r  = (r % ((y - x) + 1)) + x;

    return r;
}


rogue.rand_percent = function (percentage) {
    return (this.get_rand(1, 100) <= percentage);
};
