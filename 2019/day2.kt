val ADD = 1
val MUL = 2
val HALT = 99

object Day2 {
    val input = File("src/main/resources/day2-input.txt").readText()
        .split(",")
        .map { it.toInt() }

    fun run(initial: List<Int>, noun: Int, verb: Int): Int {
        val program = initial.toIntArray()
        program[1] = noun
        program[2] = verb
        (program.indices step 4).forEach { ip ->
            when (program[ip]) {
                ADD -> program[program[ip + 3]] = program[program[ip + 1]] + program[program[ip + 2]]
                MUL -> program[program[ip + 3]] = program[program[ip + 1]] * program[program[ip + 2]]
                HALT -> return program[0]
            }
        }
        throw Exception("Missing HALT cmd")
    }

    fun run2(program: List<Int>): Int {
        var answer = 0
        (0..99).forEach { noun ->
            (0..99).forEach { verb ->
                val result = run(program, noun, verb)
                // println("$noun $verb - $result")
                if (result == 19690720) {
                    return (noun * 100) + verb
                }
            }
        }
        return answer
    }

    val part1Solution = run(input, 12, 2)
    val part2Solution = run2(input)

    fun printAnswers() {
        println(part1Solution)
        println(part2Solution)
    }
}