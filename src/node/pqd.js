var restify = require('restify');
var server = restify.createServer();

server.use(restify.bodyParser({ mapParams: false }));

/*
  Hiding the *queues* variable in a closure
*/
var getQueue = (function (queues) {
  return function (name) {
    if (! queues[name])
      queues[name] = {
        name: name,
        first: null,
        last: null,
        count: 0
      };
    return queues[name];
  };
})({});

var push = function (queue, value) {
  if (value) {
    var node = {
      next: null,
      value: value
    };
    if (queue.count === 0)
      queue.first = node;
    else
      queue.last.next = node;
    queue.last = node;
    queue.count++;
  }
};

var pop = function (queue) {
  var node = queue.first;
  if (node) {
    queue.first = node.next;
    queue.count--;
  }
  return node;
};

var makeQueueResponse = function (queue) {
  return {
    queue: queue.name,
    count: queue.count
  };
};

server.post('/:queue', function (req, res, next) {
  var queue = getQueue(req.params.queue);
  push(queue, req.body);
  res.send(makeQueueResponse(queue));
  return next();
});

server.get('/:queue', function (req, res, next) {
  var queue = getQueue(req.params.queue),
      node = pop(queue),
      resObj = makeQueueResponse(queue);
  resObj.eof = node === null;
  resObj.value = node ? node.value : undefined;
  res.send(resObj);
  return next();
});

server.get('/:queue/count', function (req, res, next) {
  res.send(makeQueueResponse(getQueue(req.params.queue)));
  return next();
});

server.listen(8080, function() {
  console.log('Poly Queue Daemon listening at', server.url);
});