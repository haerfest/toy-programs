var http    = require('http');
var fs      = require('fs');
var sqlite3 = require('sqlite3');
var express = require('express');

var port     = 1337;
var database = '/home/wouter/projects/vre/database/data-se01x0153mlc010-mpxbulk-1304080700/data-se01x0153mlc010-mpxbulk-1304080700.sqlite3';

var db  = new sqlite3.Database(database);
var app = express();


function returnFile (filename, res, type) {
    fs.readFile(filename, function (err, data) {
        if (err) {
            res.writeHead(404, {'Content-Type': 'text/plain'});
            res.end('404 File not found');
        } else {
            res.writeHead(200, {'Content-Type': type});
            res.end(data);
        }
    });
}


app.get('/', function (req, res) {
    returnFile('index.html', res, 'text/html');
});

app.get('/index.html', function (req, res) {
    returnFile(req.url.substr(1), res, 'text/html');
});

app.get('/script.js', function (req, res) {
    returnFile(req.url.substr(1), res, 'text/javascript');
});

app.get('/api/find-regnum', function (req, res) {
    db.all('select rec_time_micros from pdsiitevent where licence_plate = ?', req.query.regnum, function (err, rows) {
        var json = JSON.stringify(rows.map(function (row) {
            return row.rec_time_micros;
        }));

        res.writeHead(200, {'Content-Type': 'text/json'});
        res.end(json);
    });
});

app.get('/api/event-at-time', function (req, res) {
    db.get('select image from pdsiitevent where rec_time_micros = ?', req.query.rec_time_micros, function (err, row) {
        var base64 = new Buffer(row.image).toString('base64')
        res.writeHead(200, {'Content-Type': 'image/html'});
        res.end(base64);
    });
});

app.listen(port);
console.log('Server listening at port ' + port);