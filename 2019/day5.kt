import java.io.File

fun readFromFile(path: String) = File(path).readText()
    .trim()
    .split(",")
    .map { it.toInt() }
    .toIntArray()

val ADD = 1
val MUL = 2
val INPUT = 3
val OUTPUT = 4
val JUMP_IF_TRUE = 5
val JUMP_IF_FALSE = 6
val LESS_THAN = 7
val EQUALS = 8
val HALT = 99

class Day5(val prog: IntArray, val input: Int = 0) {

    companion object {
        fun of(path: String, input: Int): Day5  {
            val program = readFromFile(path)
            return Day5(program, input)
        }
    }

    fun getOpValue(prog: IntArray, addr: Int, mode: Int): Int {
        return when(mode) {
            0 -> prog[addr]
            1 -> addr
            2 -> prog[addr]
            else -> throw Exception("Invalid mode")
        }
    }

    fun run(): Int {
        var pointer = 0
        val output = mutableListOf<Int>()
        var currentOp = 0

        do {

            val opCode = prog[pointer]
            if (opCode == HALT) break

            val modes = opCode / 100
            val mode1 = modes % 10
            val mode2 = (modes / 10) % 10
            val mode3 = (modes / 100) % 10

            val op1 = getOpValue(prog, pointer + 1, mode1)
            val op2 = getOpValue(prog, pointer + 2, mode2)
            val op3 = getOpValue(prog, pointer + 3, mode3)

            currentOp = opCode % 100

            when(currentOp) {
                ADD -> {
                    prog[op3] = prog[op1] + prog[op2]
                    pointer += 4
                }
                MUL -> {
                    prog[op3] = prog[op1] * prog[op2]
                    pointer += 4
                }
                INPUT -> {
                    prog[op1] = input
                    pointer += 2
                }
                OUTPUT -> {
                    output += prog[op1]
                    pointer += 2
                }
                JUMP_IF_TRUE -> {
                    if (prog[op1] != 0) {
                        pointer = prog[op2]
                    } else {
                        pointer += 3
                    }
                }
                JUMP_IF_FALSE -> {
                    if (prog[op1] == 0) {
                        pointer = prog[op2]
                    } else {
                        pointer += 3
                    }
                }
                LESS_THAN -> {
                    prog[op3] = if (prog[op1] < prog[op2]) 1 else 0
                    pointer += 4
                }
                EQUALS -> {
                    prog[op3] = if (prog[op1] == prog[op2]) 1 else 0
                    pointer += 4
                }
                else -> throw Exception("bad opcode: ${opCode % 100}")
            }

        } while (currentOp != HALT)

        val diagnosticCode = output.last()
        println("Diagnostic code: ${diagnosticCode}")
        return diagnosticCode
    }
}
