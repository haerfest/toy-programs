var http = require('http'),
    port = 8080,
    ip   = '127.0.0.1';

http.createServer(function (req, res) {
    res.writeHead(200, {'Content-type': 'text/plain'});
    res.end('Hello, world!');
}).listen(port, ip);

console.log('Server listening at ' + ip + ':' + port);

