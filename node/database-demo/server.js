var http    = require("http");
var fs      = require("fs");
var sqlite3 = require("sqlite3");
var express = require("express");

var port     = 1337;
var database = "/home/wouter/projects/vre/database/data-se01x0153mlc010-mpxbulk-1304080700/data-se01x0153mlc010-mpxbulk-1304080700.sqlite3";

var db  = new sqlite3.Database(database);
var app = express();


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

app.get("/api/get-image/:id", function (req, res) {
    db.get("select image from pdsiitevent where id = ?", req.param("id"), function (err, row) {
        if (!err && row) {
            res.writeHead(200, {"Content-Type": "image/jpeg"});
            res.end(row.image, "binary");
        }
    });
});

app.get("/api/get-events-at-time", function (req, res) {
    db.all("select distinct asset_management_id from pdsiitevent order by asset_management_id",
           function (err, rows) {
               var asset_management_ids = rows.map(function (r) { return r.asset_management_id; });
               returnImages(req, res, [], asset_management_ids);
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