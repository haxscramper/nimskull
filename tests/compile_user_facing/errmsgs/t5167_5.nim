discard """
cmd: "nim check $file"
action: reject
nimout: '''
t5167_5.nim(11, 16) Error: expression has no type: system
t5167_5.nim(12, 18) Error: invalid expression: T = system
t5167_5.nim(23, 9) Error: 't' has unspecified generic parameters
'''
"""
# issue #11942
discard newSeq[system]()
discard newSeq[T = system]()

# issue #5167
template t[B]() =
  echo "foo1"

macro m[T]: untyped = nil

proc bar(x: proc (x: int)) =
  echo "bar"

let x = t
bar t

let y = m
bar m
