var rogue = rogue || {};


rogue.init_inventory_locals = function () {
    this.inventory_locals = {
        is_wood: new Array(this.WANDS),
        press_space: ' --press space to continue--',
        wand_materials: [
	          'steel ',
	          'bronze ',
	          'gold ',
	          'silver ',
	          'copper ',
	          'nickel ',
	          'cobalt ',
	          'tin ',
	          'iron ',
	          'magnesium ',
	          'chrome ',
	          'carbon ',
	          'platinum ',
	          'silicon ',
	          'titanium ',
	          'teak ',
	          'oak ',
	          'cherry ',
	          'birch ',
	          'pine ',
	          'cedar ',
	          'redwood ',
	          'balsa ',
	          'ivory ',
	          'walnut ',
	          'maple ',
	          'mahogany ',
	          'elm ',
	          'palm ',
	          'wooden '
        ],
        gems: [
 	          'diamond ',
	          'stibotantalite ',
	          'lapi-lazuli ',
	          'ruby ',
	          'emerald ',
	          'sapphire ',
	          'amethyst ',
	          'quartz ',
	          'tiger-eye ',
	          'opal ',
	          'agate ',
	          'turquoise ',
	          'pearl ',
	          'garnet '
        ],
        syllables: [
	          'blech ',
	          'foo ',
	          'barf ',
	          'rech ',
	          'bar ',
	          'blech ',
	          'quo ',
	          'bloto ',
	          'oh ',
	          'caca ',
	          'blorp ',
	          'erp ',
	          'festr ',
	          'rot ',
	          'slie ',
	          'snorf ',
	          'iky ',
	          'yuky ',
	          'ooze ',
	          'ah ',
	          'bahl ',
	          'zep ',
	          'druhl ',
	          'flem ',
	          'behil ',
	          'arek ',
	          'mep ',
	          'zihr ',
	          'grit ',
	          'kona ',
	          'kini ',
	          'ichi ',
	          'tims ',
	          'ogr ',
	          'oo ',
	          'ighr ',
	          'coph ',
	          'swerr ',
	          'mihln ',
	          'poxi '
        ],
        COMS: 48,
        com_id_tab: [
 	          { com_char: '?', com_desc: '?       prints help' },
            { com_char:	'r', com_desc: 'r       read scroll' },
            { com_char: '/', com_desc: '/       identify object' },
	          { com_char: 'e', com_desc: 'e       eat food' },
	          { com_char: 'h', com_desc: 'h       left ' },
	          { com_char: 'w', com_desc: 'w       wield a weapon' },
	          { com_char: 'j', com_desc: 'j       down' },
	          { com_char: 'W', com_desc: 'W       wear armor' },
	          { com_char: 'k', com_desc: 'k       up' },
	          { com_char: 'T', com_desc: 'T       take armor off' },
	          { com_char: 'l', com_desc: 'l       right' },
	          { com_char: 'P', com_desc: 'P       put on ring' },
	          { com_char: 'y', com_desc: 'y       up & left' },
	          { com_char: 'R', com_desc: 'R       remove ring' },
	          { com_char: 'u', com_desc: 'u       up & right' },
	          { com_char: 'd', com_desc: 'd       drop object' },
	          { com_char: 'b', com_desc: 'b       down & left' },
	          { com_char: 'c', com_desc: 'c       call object' },
	          { com_char: 'n', com_desc: 'n       down & right' },
	          { com_char: null, com_desc: '<SHIFT><dir>: run that way' },
	          { com_char: ')', com_desc: ')       print current weapon' },
	          { com_char: null, com_desc: '<CTRL><dir>: run till adjacent' },
	          { com_char: ']', com_desc: ']       print current armor' },
	          { com_char: 'f', com_desc: 'f<dir>  fight till death or near death' },
	          { com_char: '=', com_desc: '=       print current rings' },
	          { com_char: 't', com_desc: 't<dir>  throw something' },
	          { com_char: '\001', com_desc: '^A      print Hp-raise average' },
	          { com_char: 'm', com_desc: 'm<dir>  move onto without picking up' },
	          { com_char: 'z', com_desc: 'z<dir>  zap a wand in a direction' },
	          { com_char: 'o', com_desc: 'o       examine/set options' },
	          { com_char: '^', com_desc: '^<dir>  identify trap type' },
	          { com_char: '\022', com_desc: '^R      redraw screen' },
	          { com_char: '&', com_desc: '&       save screen into \'rogue.screen\'' },
	          { com_char: 's', com_desc: 's       search for trap/secret door' },
	          { com_char: '\020', com_desc: '^P      repeat last message' },
	          { com_char: '>', com_desc: '>       go down a staircase' },
	          { com_char: '\033', com_desc: '^[      cancel command' },
	          { com_char: '<', com_desc: '<       go up a staircase' },
	          { com_char: 'S', com_desc: 'S       save game' },
	          { com_char: '.', com_desc: '.       rest for a turn' },
	          { com_char: 'Q', com_desc: 'Q       quit' },
	          { com_char: ',', com_desc: ',       pick something up' },
	          { com_char: '!', com_desc: '!       shell escape' },
	          { com_char: 'i', com_desc: 'i       inventory' },
	          { com_char: 'F', com_desc: 'F<dir>  fight till either of you dies' },
	          { com_char: 'I', com_desc: 'I       inventory single item' },
	          { com_char: 'v', com_desc: 'v       print version number' },
	          { com_char: 'q', com_desc: 'q       quaff potion' }
        ]
    };
};


rogue.mix_colors = function () {
    var i,
        j,
        k,
        t;

    for (i = 0; i <= 32; i += 1) {
        j = this.get_rand(0, this.POTIONS - 1);
        k = this.get_rand(0, this.POTIONS - 1);
        
        t = this.object_locals.id_potions[j].title;
        this.object_locals.id_potions[j].title = this.object_locals.id_potions[k].title;
        this.object_locals.id_potions[k].title = t;
    }
};
