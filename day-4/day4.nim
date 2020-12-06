import sets
import sequtils
import strutils
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
        case k:
            of "byr":
                # do check, return false if fails
                echo v
    return false

let part2ValidPassports = passports.filter(part1Validate).filter(part2Validate)

