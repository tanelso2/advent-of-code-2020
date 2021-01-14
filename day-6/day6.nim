import sets
import strutils
import sequtils
import sugar

let entireFile = readFile("input.txt")
let groups = entireFile.strip().split("\n\n")

type
    charSet = HashSet[char]

proc getQuestionSet(group: string, op: (charSet, charSet) -> charSet): charSet =
    let lines = group.splitLines
    return lines.map((x) => x.toHashSet).foldl(op(a, b))

let questionSets = groups.map((x) => x.getQuestionSet(union))

proc getSum(qs: seq[charSet]): int =
    return qs.foldl(a + b.card, 0)

echo "The answer to part1 is ", getSum(questionSets)

let part2QuestionSets = groups.map((x) => x.getQuestionSet(intersection))

echo "The answer to part 2 is ", getSum(part2QuestionSets)