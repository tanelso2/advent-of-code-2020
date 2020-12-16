import sets
import strutils
import sequtils
import sugar

let entireFile = readFile("input.txt")
let groups = entireFile.strip().split("\n\n")

proc getQuestionSet(group: string, op: (HashSet[char], HashSet[char]) -> HashSet[char]): HashSet[char] =
    let lines = group.splitLines
    var ret = lines[0].toHashSet()
    for line in lines:
        let s = line.toHashSet()
        ret = op(ret, s)
    return ret

let questionSets = groups.map((x) => x.getQuestionSet(union))

proc getSum(qs: seq[HashSet[char]]): int =
    var sum = 0
    for s in qs:
        sum += s.card
    return sum

echo "The answer to part1 is ", getSum(questionSets)

let part2QuestionSets = groups.map((x) => x.getQuestionSet(intersection))

echo "The answer to part 2 is ", getSum(part2QuestionSets)