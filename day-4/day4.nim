import re
import sets
import sequtils
import strutils
import sugar
import tables

let entireFile = readFile("input.txt")

let sections = entireFile.strip().split("\n\n")

type
    passport = Table[string, string]

proc parseSection(section: string): passport =
    for line in section.split({'\n'}):
        let fields = line.strip().splitWhitespace()
        for field in fields:
            let x = field.split(":")
            result[x[0]] = x[1]

let passports = sections.map(parseSection)

proc part1Validate(p: passport): bool =
    let requiredFields = toHashSet(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])
    let foundFields = toHashSet(toSeq(p.keys))
    return requiredFields <= foundFields

let part1ValidPassports = passports.filter(part1Validate)
echo "The answer to part 1 is ", part1ValidPassports.len

proc part2Validate(p: passport): bool =
    for (k,v) in p.pairs:
        # Return false whenever something fails, continue otherwise
        case k:
            of "byr":
                let intVal = parseInt(v)
                if intVal < 1920 or intVal > 2002:
                    echo "failing byr ", v
                    return false
            of "iyr":
                let intVal = parseInt(v)
                if intVal < 2010 or intVal > 2020:
                    echo "failing iyr ", v
                    return false
            of "eyr":
                let intVal = parseInt(v)
                if intVal < 2020 or intVal > 2030:
                    echo "failing eyr ", v
                    return false
            of "hgt":
                if v.endsWith("cm"):
                    let intVal = parseInt(v[0..^3])
                    if intVal < 150 or intVal > 193:
                        echo "failing hgt ", v
                        return false
                elif v.endsWith("in"):
                    let intVal = parseInt(v[0..^3])
                    if intVal < 59 or intVal > 76:
                        echo "failing hgt ", v
                        return false
                else:
                    echo "failing hgt ", v
                    return false
            of "hcl":
                if not (v =~ re"#[0..9|a..f]*"):
                    echo "failing hcl ", v
                    return false
            of "ecl":
                let validOptions = toHashSet(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
                if not validOptions.contains(v):
                    echo "failing ecl ", v
                    return false
            of "pid":
                if len(v) != 9:
                    echo "failing pid ", v
                    return false
                if not v.allCharsInSet(Digits):
                    echo "failing pid ", v
                    return false
            else:
                echo "ignoring ", k
                discard
    return true

let part2ValidPassports = passports.filter(part1Validate).filter(part2Validate)

echo "The answer to part2 is ", len(part2ValidPassports)