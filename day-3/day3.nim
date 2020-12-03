import strutils

let entireFile = readFile("input.txt")
let lines = entireFile.strip().split({'\n'})

proc countTrees(lines: openArray[string], dx, dy: int): int =
    result = 0
    var y = 0
    var x = 0
    let width = len(lines[0])
    while y < len(lines):
        if lines[y][x] == '#':
            result += 1
        y += dy
        x = (x + dx) %% width

proc part1(lines: openArray[string]): int =
    return lines.countTrees(dx = 3, dy = 1)

proc part2(lines: openArray[string]): int =
    let cases = [
        (1,1),
        (3,1),
        (5,1),
        (7,1),
        (1,2)
    ]
    result = 1
    for (dx, dy) in cases:
        result *= lines.countTrees(dx=dx, dy=dy)



echo "The answer to part 1 is ", part1(lines)

echo "The answer to part 2 is ", part2(lines)
