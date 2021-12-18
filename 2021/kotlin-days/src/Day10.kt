import java.util.ArrayDeque

sealed class Result
data class Corrupted(val char: Char) : Result()
data class Incomplete(val stack: ArrayDeque<Char>) : Result()
object Ok : Result()

val corruptionPoints = mapOf(
    ')' to 3,
    ']' to 57,
    '}' to 1197,
    '>' to 25137
)

val completionPoints = mapOf(
    ')' to 1L,
    ']' to 2L,
    '}' to 3L,
    '>' to 4L
)

fun Incomplete.points(): Long = stack.map { ch -> completionPoints[ch] as Long }.fold(0L) { acc, p ->
    5 * acc + p
}

fun Corrupted.points(): Int = corruptionPoints[char] as Int

fun parse(line: String, stack: ArrayDeque<Char>): Result {
    // valid
    if (line.isEmpty() && stack.isEmpty()) return Ok
    // incomplete
    if (line.isEmpty()) return Incomplete(stack)
    return when(val next = line[0]) {
        // closing
        stack.peek() -> {
            stack.pop()
            parse(line.drop(1), stack)
        }
        // opening
        '('          -> {
            stack.push(')')
            parse(line.drop(1), stack)
        }
        '['          -> {
            stack.push(']')
            parse(line.drop(1), stack)
        }
        '<'          -> {
            stack.push('>')
            parse(line.drop(1), stack)
        }
        '{'          -> {
            stack.push('}')
            parse(line.drop(1), stack)
        }
        // corrupted
        else         -> Corrupted(next)
    }
}

fun main() {
    fun part1(input: List<String>): Int {
        return input.map { parse(it, ArrayDeque<Char>()) }
            .filterIsInstance<Corrupted>()
            .map { it.points() }
            .sum()
    }

    fun part2(input: List<String>): Any {
        val totals = input.map { parse(it, ArrayDeque<Char>()) }
            .filterIsInstance<Incomplete>()
            .map { it.points() }
            .sorted()
        println(totals)
        return totals[totals.size / 2]
    }

    val input = readInput("Day10")
    println(part1(input))
    println(part2(input))
}
