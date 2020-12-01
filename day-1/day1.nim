import sequtils
import strutils

let entireFile = readFile("input.txt")
let lines = entireFile.strip().split({'\n'})

let numbers = map(lines, proc(x: string): int = parseInt(x))

for x in numbers:
  for y in numbers:
    if x + y == 2020:
      echo "The answer to part 1 is ", x * y

for x in numbers:
  for y in numbers:
    for z in numbers:
      if x + y + z == 2020:
        echo "The answer to part 2 is ", x * y * z
