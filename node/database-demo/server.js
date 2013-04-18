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

app.get("/api/find-regnum", function (req, res) {
    var param = req.query.regnum;
    db.all("select strftime('%Y-%m-%d %H:%S:%f', rec_time_micros / 1E6, 'unixepoch') as title, rec_time_micros from pdsiitevent where licence_plate = ?", param, function (err, rows) {
        res.writeHead(200, {"Content-Type": "text/json"});
        res.end(JSON.stringify(rows));
    });
});


function returnImages (req, res, json, asset_management_ids) {
    if (asset_management_ids.length === 0) {
        res.writeHead(200, {"Content-Type": "text/html"});
        res.end(JSON.stringify(json));
    } else {
        db.get("select image from pdsiitevent where asset_management_id = ? and rec_time_micros <= ? order by rec_time_micros desc limit 1",
               asset_management_ids[0],
               req.query.rec_time_micros,
               function (err, row) {
                   if (!err) {
                       var base64 = new Buffer(row.image).toString("base64");
                       json.push({"asset_management_id": asset_management_ids[0],
                                  "image": base64});
                   }
                   
                   returnImages(req, res, json, asset_management_ids.slice(1));
               });
    }
}


app.get("/api/get-events-at-time", function (req, res) {
    db.all("select distinct asset_management_id from pdsiitevent order by asset_management_id",
           function (err, rows) {
               var asset_management_ids = rows.map(function (r) { return r.asset_management_id; });
               returnImages(req, res, [], asset_management_ids);
           });
});

app.get("/api/next-event", function (req, res) {
    var param = req.query.rec_time_micros;
    db.each("select strftime('%Y-%m-%d %H:%S:%f', rec_time_micros / 1E6, 'unixepoch') as title, rec_time_micros from pdsiitevent where rec_time_micros > ? order by rec_time_micros asc limit 1", param, function (err, row) {
        res.writeHead(200, {"Content-Type": "text/json"});
        res.end(JSON.stringify(row));
    });
});

app.get("/api/prev-event", function (req, res) {
    var param = req.query.rec_time_micros;
    db.each("select strftime('%Y-%m-%d %H:%S:%f', rec_time_micros / 1E6, 'unixepoch') as title, rec_time_micros from pdsiitevent where rec_time_micros < ? order by rec_time_micros desc limit 1", param, function (err, row) {
        res.writeHead(200, {"Content-Type": "text/json"});
        res.end(JSON.stringify(row));
    });
});

app.get("/api/get-time-range", function (req, res) {
    db.get("select min(rec_time_micros) as minimum, max(rec_time_micros) as maximum from (select rec_time_micros from pdsiitevent union select rec_time_micros from pdstrackerevent)", function (err, row) {
        res.writeHead(200, {"Content-Type": "text/json"});
        res.end(JSON.stringify(row));
    });
});

app.listen(port);
console.log("Server listening at port " + port);