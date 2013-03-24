var sys   = require('sys'),
    http  = require('http'),
    fs    = require('fs'),
    port  = 8080,
    ip    = '127.0.0.1',
    index;

fs.readFile('index.html', function (err, data) {
    if (err) {
        throw err;
    }
    index = data;
});

http.createServer(function (req, res) {
    res.writeHead(200, {'Content-type': 'text/html'});
    res.write(index);
    res.end();
}).listen(port, ip);

console.log('Server listening at ' + ip + ':' + port);

