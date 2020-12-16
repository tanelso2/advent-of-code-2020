import strutils
import sequtils
import sets

let entireFile = readFile("input.txt")
let lines = entireFile.strip().split("\n")

proc getSeatNumber(line: string): int =
    proc convertChars(c: char): char =
        result = case c:
            of 'B':
                '1'
            of 'F':
                '0'
            of 'R':
                '1'
            of 'L':
                '0'
            else:
                '0'
                
    let binString = cast[string](line.map(convertChars))
    return parseBinInt(binString)

echo "Testing ", getSeatNumber("FBFBBFFRLR")

let seatNumbers = lines.map(getSeatNumber)

echo "The answer to part 1 is ", max(seatNumbers)

proc part2(): int =
    for i in 80 .. 926:
        if not (i in seatNumbers):
            return i

echo "The answer to part2 is ", part2()