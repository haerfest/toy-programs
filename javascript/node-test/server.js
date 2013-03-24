var sys   = require('sys'),
    http  = require('http'),
    fs    = require('fs'),
    url   = require('url'),
    port  = 8080,
    ip    = '127.0.0.1';


http.createServer(function (req, res) {

    var serveFile = function (page) {
        var filename = page.substr(1);

        fs.readFile(filename, function (err, data) {
            var status = 200;
            var body   = data;

            if (err) {
                status = 404;
                body   = '<h1>' + err.name + ': ' + err.message + '</h1>';
            }
            
            res.writeHead(status, {'Content-type': 'text/html'});
            res.write(body);
            res.end();
        });
    };

    var getFileList = function () {
        fs.readdir('.', function (err, files) {
            var status = 200;
            var body   = files;

            if (err) {
                status = 500;
                body   = '';
            }
            
            res.writeHead(status, {'Content-type': 'text/html'});
            res.write(JSON.stringify(body));
            res.end();
        });
    };
    
    var serveApi = function (api) {
        switch (api.toLowerCase()) {
        case 'getfilelist':
            getFileList();
            break;

        default:
            res.writeHead(501, {'Content-type': 'text/html'});
            res.end();
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

