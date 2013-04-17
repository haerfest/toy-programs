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

app.get("/api/event-at-time", function (req, res) {
    var param = req.query.rec_time_micros;
    db.get("select image from pdsiitevent where rec_time_micros = ?", param, function (err, row) {
        var base64 = new Buffer(row.image).toString("base64")
        res.writeHead(200, {"Content-Type": "text/html"});
        res.end(base64);
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

app.listen(port);
console.log("Server listening at port " + port);