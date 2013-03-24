var sys      = require('sys'),
    http     = require('http'),
    fs       = require('fs'),
    url      = require('url'),
    sqlite3  = require('sqlite3').verbose(),
    database = 'database.sqlite3',
    port     = 8080,
    ip       = '127.0.0.1';


http.createServer(function (req, res) {

    var returnError = function (statusCode, body) {
        res.writeHead(statusCode, {'Content-type': 'text/plain'});
        if (body) {
            res.write(body);
        }
        res.end();
    };

    var getMimeType = function (page) {
        var mimeType = 'text/plain';
        
        if (page.match(/\.html?$/)) {
            mimeType = 'text/html';
        } else if (page.match(/\.css$/)) {
            mimeType = 'text/css';
        } else if (page.match(/\.js$/)) {
            mimeType = 'text/javascript';
        };

        return mimeType;
    };
    
    var serveFile = function (page) {
        var filename = page.substr(1);

        fs.readFile(filename, function (err, data) {
            if (err) {
                returnError(404, err.message);
                return;
            }
            
            res.writeHead(200, {'Content-type': getMimeType(page)});
            res.write(data);
            res.end();
        });
    };

    var getFileList = function () {
        fs.readdir('.', function (err, files) {
            if (err) {
                returnError(500);
                return;
            }
            
            res.writeHead(200, {'Content-type': 'application/json'});
            res.write(JSON.stringify(files));
            res.end();
        });
    };

    var getNameList = function () {
        var db = new sqlite3.Database(database, sqlite3.OPEN_READONLY, function (err) {
            if (err) {
                returnError(500);
                return;
            }
            
            db.all('SELECT name FROM names;', function (err, rows) {
                var names = rows.map(
                    function (row) {
                        return row.name;
                    }
                );
                res.writeHead(200, {'Content-type': 'application/json'});
                res.write(JSON.stringify(names));
                res.end();
            });
        });
    };

    var serveApi = function (api) {
        switch (api.toLowerCase()) {
        case 'getfilelist':
            getFileList();
            break;

        case 'getnamelist':
            getNameList();
            break;

        default:
            returnError(501);
            break;
        }
    };
    
    var page = url.parse(req.url).pathname;
    console.log(page);

    if (page === '/') {
        serveFile('/index.html');
    } else if (page.match(/^\/api\//)) {
        serveApi(page.substr(5));
    } else {
        serveFile(page);
    }

}).listen(port, ip);

console.log('Server listening at ' + ip + ':' + port);

