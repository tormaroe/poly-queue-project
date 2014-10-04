import jester, json, tables

type
  PNode = ref TNode
  TNode = tuple[next, prev: PNode, val: string]

  PQueue = ref TQueue
  TQueue = object
    queue: string
    count: int
    first: PNode
    last: PNode

proc newNode(val: string, previous: PNode): PNode =
  new(result)
  result.prev = previous
  result.val = val

proc newQueue(key: string): PQueue =
  new(result)
  result.queue = key
  result.count = 0

var queues = initTable[string, PQueue]()

proc getQueue(key: string): PQueue =
  if not queues.hasKey(key):
    queues[key] = newQueue(key)
  result = queues[key] 

proc push(q: PQueue, val: string) =
  var node = newNode(val, previous = q.last)
  if q.count == 0:
    q.first = node
  else:
    node.prev.next = node
  q.last = node
  q.count = q.count + 1
  
proc pop(q: PQueue): PNode =
  result = q.first
  if result != nil:
    q.first = result.next
    q.count = q.count - 1
    if q.first != nil:
      q.first.prev = nil

proc `%`(q: PQueue): PJsonNode =
  result = %[("queue", %q.queue), ("count", %q.count)]

proc `%`(q: PQueue, val: string, eof: bool): PJsonNode =
  result = %[("queue", %q.queue), ("count", %q.count), 
             ("value", %val), ("eof", %eof)]

post "/@queue":
  let queue = getQueue(@"queue")
  queue.push(request.body)
  resp($ %queue, "application/json")

get "/@queue":
  let queue = getQueue(@"queue")
  let node = queue.pop()
  var val = ""
  if node != nil: val = node.val
  let jsonResponse = queue.`%`(val, node == nil)
  resp($jsonResponse, "application/json")

get "/@queue/count":
  let queue = getQueue(@"queue")
  resp($ %queue, "application/json")

run()