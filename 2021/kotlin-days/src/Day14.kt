fun main() {
    // left slow method for part 1
    fun part1(input: List<String>): Int {
        val template = input.first().windowed(2, 1, true)
        val substitutions = input.drop(2).fold(mutableMapOf<String, String>()) { map, i ->
            val (sub, letter) = i.split(" -> ")
            map[sub] = letter
            map
        }

        val result = (1..10).fold(template) { acc, _i ->
            acc.flatMap {
                if (substitutions.contains(it)) {
                    val sub = substitutions[it] as String
                    listOf(it[0] + sub, sub + it[1])
                } else {
                    listOf(it)
                }
            }
        }

        val polymer = result.map { it.first() }.joinToString("")
        val letterCounts = polymer.groupingBy { it } .eachCount().values.sorted()
        return letterCounts.last() - letterCounts.first()
    }

    fun part2(input: List<String>): Any {
        val template = input.first().windowed(2, 1, true).fold(mutableMapOf<String, Long>()) { acc, combo ->
            acc[combo] = (acc[combo] ?: 0).toLong() + 1L
            acc
        }
        val substitutions = input.drop(2).fold(mutableMapOf<String, String>()) { map, i ->
            val (sub, letter) = i.split(" -> ")
            map[sub] = letter
            map
        }

        val result = (1..40).fold(template) { temp, _ ->
            temp.toList().fold(mutableMapOf()) { acc, (pair, count) ->
                substitutions[pair]?.let { sub ->
                    val leftKey = "${pair[0]}$sub"
                    val rightKey = "$sub${pair[1]}"
                    acc[leftKey] = (acc[leftKey] ?: 0L) + count
                    acc[rightKey] = (acc[rightKey] ?: 0L) + count
                } ?: run {
                    acc[pair] = count
                }
                acc
            }
        }

        val totals = result.toList().fold(mutableMapOf<Char, Long>()) { acc, (pair, count) ->
            acc[pair[0]] = acc[pair[0]]?.let { it + count } ?: count
            acc
        }.values.sorted()
        return totals.last() - totals.first()
    }

    val input = readInput("Day14")
    println(part1(input))
    println(part2(input))
}