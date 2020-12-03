import sequtils
import strutils

let entireFile = readFile("input.txt")
let lines = entireFile.strip().split({'\n'})

type PasswordVerifier = tuple
    targetCharacter: char
    min: int
    max: int
    password: string

proc parseLine(line: string): PasswordVerifier =
    let s = line.split(seps=Whitespace)
    let numRange: string = s[0]
    let targetChar: char = s[1][0]
    let password = s[2]
    let x = numRange.split({'-'}).map(parseInt)
    let min = x[0]
    let max = x[1]
    return (
        targetCharacter: targetChar,
        min: min,
        max: max,
        password: password
    )

let passwords = lines.map(parseLine)

proc part1Validate(p: PasswordVerifier): bool =
    var count = 0
    for c in p.password:
        if c == p.targetCharacter:
            count += 1
    return count >= p.min and count <= p.max 

let part1ValidPasswords = passwords.filter(part1Validate)

echo "The answer to part 1 is ", len(part1ValidPasswords)

proc part2Validate(p: PasswordVerifier): bool =
    let minChar = p.password[p.min-1]
    let maxChar = p.password[p.max-1]
    return minChar == p.targetCharacter xor maxChar == p.targetCharacter

let part2ValidPasswords = passwords.filter(part2Validate)

echo "The answer to part 2 is ", len(part2ValidPasswords)