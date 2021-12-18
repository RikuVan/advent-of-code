
object Day4 {
    val passwords = "171309-643603"
    val limits = passwords.split("-").map { it.toInt() }.zipWithNext().first()
    // kind of ugly first try
    val numLegitpasswordsPart1 = (limits.first..limits.second).filter { password ->
        var idx = 5
        var hasPair = false
        var isAscending = true
        val parts = password.toString().map { it.toString().toInt() }
        while(idx > 0) {
            if (parts[idx] == parts[idx - 1]) {
                hasPair = true
            }
            if (parts[idx] < parts[idx - 1]) {
                isAscending = false
            }
            idx--
        }
        hasPair && isAscending
    }.size

    // could refactor original part 1
    val numLegitpasswordsPart2 = (limits.first..limits.second).filter { password ->
        var idx = 5
        var isAscending = true
        val parts = password.toString().map { it.toString().toInt() }
        while(idx > 0) {
            if (parts[idx] < parts[idx - 1]) {
                isAscending = false
            }
            idx--
        }
        val hasPair = parts.groupBy { it }.any { it.value.size == 2 }
        hasPair && isAscending
    }.size

    fun printAnswer() {
        println(numLegitpasswordsPart1)
        println(numLegitpasswordsPart2)
    }
}
