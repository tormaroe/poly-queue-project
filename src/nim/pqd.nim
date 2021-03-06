import jester, json, tables, locks

type
  PNode = ref TNode
  TNode = tuple[next: PNode, val: string]

  PQueue = ref TQueue
  TQueue = tuple[queue: string, count: int, first, last: PNode]

proc newNode(val: string): PNode =
  new(result)
  result.val = val

proc newQueue(key: string): PQueue =
  new(result)
  result.queue = key
  result.count = 0

var 
  queues = initTable[string, PQueue]()
  L: TLock

proc getQueue(key: string): PQueue =
  acquire(L)
  if not queues.hasKey(key):
    queues[key] = newQueue(key)
  release(L)
  result = queues[key] 

proc push(q: PQueue, val: string) =
  acquire(L)
  let node = newNode(val)
  if q.count == 0:
    q.first = node
  else:
    q.last.next = node
  q.last = node
  q.count = q.count + 1
  release(L)
  
proc pop(q: PQueue): PNode =
  acquire(L)
  result = q.first
  if result != nil:
    q.first = result.next
    q.count = q.count - 1
  release(L)

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

initLock(L)

run()