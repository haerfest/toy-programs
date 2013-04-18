/* Before you run this:
 *
 * % npm install sqlite3
 */

var path    = require('path');
var sqlite3 = require('sqlite3');

var program_name  = path.basename(process.argv[1]);
var database_file = process.argv[2];

if (!database_file) {
    process.stderr.write('Usage: ' + program_name + ' <database.sqlite3>\n');
    process.exit(1);
}

var db = new sqlite3.Database(database_file, sqlite3.OPEN_READONLY);
var query = "SELECT COUNT(*) FROM pdsiitevent WHERE trigger_source_cause_enum = ?";
var histogram = {};

db.get(query, 'CAUSE_ALPR', function (err, row) { histogram[

statement.each(
    function (err, row) {
        if (err) {
            process.stderr.write('Error: ' + err + '\n');
            process.exit(1);
        }

        var cause = row.trigger_source_cause_enum;

        histogram[cause] = histogram[cause] || 0;
        histogram[cause] += 1;
    },
    function () {
        console.log(histogram);
    }
);

statement.finalize();

db.close();

