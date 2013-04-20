var http    = require("http"),
    fs      = require("fs"),
    path    = require("path"),
    sqlite3 = require("sqlite3"),
    express = require("express");

var port     = 1337,
    database = process.argv[2];

//
// We need to have a database.
//
if (!database) {
    var program_name = path.basename(process.argv[1]);
    process.stderr.write("Usage: node " + program_name + " <sqlite3 database file>\n");
    process.exit(1);
}

var db  = new sqlite3.Database(database),
    app = express();


function returnFile (filename, res, type) {
    fs.readFile(filename, function (err, data) {
        if (err) {
            res.writeHead(404, {"Content-Type": "text/plain"});
            res.end("404 File not found");
        } else {
            res.writeHead(200, {"Content-Type": type});
            res.end(data);
        }
    });
}


app.get("/", function (req, res) {
    returnFile("index.html", res, "text/html");
});

app.get("/index.html", function (req, res) {
    returnFile(req.url.substr(1), res, "text/html");
});

app.get("/style.css", function (req, res) {
    returnFile(req.url.substr(1), res, "text/css");
});

app.get("/script.js", function (req, res) {
    returnFile(req.url.substr(1), res, "text/javascript");
});


function returnImages (req, res, json, asset_management_ids) {
    if (asset_management_ids.length === 0) {
        res.writeHead(200, {"Content-Type": "text/html"});
        res.end(JSON.stringify(json));
    } else {
        //
        // We should fetch the last record before #rec_time_micros, but as that's very slow, we'll pick
        // the first one after #rec_time_micros. We do no order as it is ordered by time.
        //
        db.get("select id from pdsiitevent where asset_management_id = ? and rec_time_micros >= ? limit 1",
               asset_management_ids[0],
               req.query.rec_time_micros,
               function (err, row) {
                   if (!err && row) {
                       json.push({"asset_management_id": asset_management_ids[0],
                                  "id": row.id});
                   }
                   
                   returnImages(req, res, json, asset_management_ids.slice(1));
               });
    }
}

function returnTracks (req, res, json, asset_management_ids) {
   if (asset_management_ids.length === 0) {
        res.writeHead(200, {"Content-Type": "text/html"});
        res.end(JSON.stringify(json));
    } else {
        //
        // We should fetch the last record before #rec_time_micros, but as that's very slow, we'll pick
        // the first one after #rec_time_micros. We do no order as it is ordered by time.
        //
        db.get("select time,left_x,left_y,right_x,right_y from track_position where pdstrackerevent_id = 2 and time > 0;",
               asset_management_ids[0],
               req.query.rec_time_micros,
               function (err, row) {
                   if (!err && row) {
                       json.push({"asset_management_id": asset_management_ids[0],
                                  "id": row.id});
                   }
                   
                   returnTracks(req, res, json, asset_management_ids.slice(1));
               });
    }
}

app.get("/api/get-image/:id", function (req, res) {
    db.get("select image from pdsiitevent where id = ?", req.param("id"), function (err, row) {
        if (!err && row) {
            res.writeHead(200, {"Content-Type": "image/jpeg"});
            res.end(row.image, "binary");
        }
    });
});

app.get("/api/get-images-at-time", function (req, res) {
    db.all("select distinct asset_management_id from pdsiitevent order by asset_management_id",
           function (err, rows) {
               var asset_management_ids = rows.map(function (r) { return r.asset_management_id; });
               returnImages(req, res, [], asset_management_ids);
           });
});

app.get("/api/get-track/:id",
        function (req, res) {
            db.all("select time as time, (left_x + right_x) / 200 as x, (left_y + right_y) / 200 as y from track_position where pdstrackerevent_id = ? and time > 0",
                   req.param("id"),
                   function (err, rows) {
                       res.json(200, rows);
                   });
        });

app.get("/api/get-tracks",
        function (req, res) {
            //db.all("select time, pdstrackerevent_id, (left_x + right_x) / 200 as center_x, (left_y + right_y) / 200 as center_y from track_position where time > 0 order by time asc",
            db.all("select id, start_time, start_pos_x / 100 as start_x, start_pos_y / 100 as start_y, end_time, end_pos_x / 100 as end_x, end_pos_y / 100 as end_y from pdstrackerevent",
                   function (err, rows) {
                       res.json(200, rows);
                   });
        });

app.get("/api/get-tracks-at-time", function (req, res) {
    db.all("select distinct asset_management_id from pdsiitevent order by asset_management_id",
           function (err, rows) {
               var asset_management_ids = rows.map(function (r) { return r.asset_management_id; });
               //returnTracks(req, res, [], asset_management_ids);


           });
});

app.get("/api/get-time-range", function (req, res) {
    db.get("select min(rec_time_micros) as minimum, max(rec_time_micros) as maximum from (select rec_time_micros from pdsiitevent union select rec_time_micros from pdstrackerevent)", function (err, row) {
        res.writeHead(200, {"Content-Type": "text/json"});
        res.end(JSON.stringify(row));
    });
});

app.get("/api/find-regnum", function (req, res) {
    db.all("select rec_time_micros from pdsiitevent where licence_plate = ?", req.query.regnum, function (err, rows) {
        res.writeHead(200, {"Content-Type": "text/json"});
        res.end(JSON.stringify(rows));        
    });
});

app.listen(port);
console.log("Server listening at port " + port);