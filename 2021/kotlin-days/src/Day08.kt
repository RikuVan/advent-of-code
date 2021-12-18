// lengths of unique num of segments
val singles = listOf(2, 3, 4, 7)


data class EncodedDisplay(val digits: List<List<String>>, val outputValue: List<String>)

fun String.parseDigits() = substring(0, 58).split(" ")
fun String.parseOutputValue() = substring(61).split(" ").filter { it.isNotBlank() }

fun main() {
    fun part1(input: List<String>): Int {
        return input.map {
            it.parseOutputValue()
        }.map { it ->
            it.filter {
                singles.contains(it.length)
            }.size
        }.sum()
    }

    fun part2(input: List<String>): Any {
        val encodedValues = input.map {
            val digits = it.parseDigits().map {
                it.split("").filter { it.isNotBlank() }.sorted()
            }
            val values = it.parseOutputValue().map { it.split("").filter { it.isNotBlank() }.sorted().joinToString("") }
            EncodedDisplay(digits, values)
        }

        val allValues = encodedValues.map { (digits, values) ->
            val one = digits.find { it.size === 2 }
            val four = digits.find { it.size === 4 }
            val seven = digits.find { it.size === 3 }
            val eight = digits.find { it.size === 7 }
            // 0, 6, 9
            val digitsWithSix = digits.filter { it.size === 6 }
            // 2, 3, 5
            val digitsWithFive = digits.filter { it.size === 5 }
            val nine = digitsWithSix?.find { digit ->
                seven!!.all { digit.contains(it) } && four!!.all { digit.contains(it) }
            }
            val zero = digitsWithSix.filter { it !== nine }.find { digit -> one!!.all { digit.contains(it) } }
            val six = digitsWithSix.find { digit -> digit !== nine && digit !== zero }
            val three = digitsWithFive.find { digit -> digit.all { nine!!.contains(it) } && !digit!!.all { six!!.contains(it) } }
            val five = digitsWithFive.find { digit -> digit.all { nine!!.contains(it) } && digit !== three }
            val two = digitsWithFive.find { digit -> digit !== five && digit !== three }

            val decodedDigits =
                listOf(zero, one, two, three, four, five, six, seven, eight, nine).map { it!!.joinToString("") }
            decodedDigits.indexOf(values[0]) * 1000 +decodedDigits.indexOf(values[1]) * 100 +decodedDigits.indexOf(values[2]) * 10 +decodedDigits.indexOf(values[3])
        }

        return allValues.sum()
    }

    val input = readInput("Day08")
    println(part1(input))
    println(part2(input))
}
