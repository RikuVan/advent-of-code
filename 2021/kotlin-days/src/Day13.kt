import kotlin.math.abs

sealed class Fold {
    companion object {
        fun of(value: String): Fold {
            val (fold, axis) = value.drop(11).split("=")
            return when(fold) {
                "x" -> Left(axis.toInt())
                else -> Up(axis.toInt())
            }
        }
    }
}
data class Left(val axis: Int) : Fold()
data class Up(val axis: Int) : Fold()

fun folder(dots: Set<Point>, fold: Fold): MutableSet<Point> {
    return dots.map { (x, y) -> when(fold) {
        is Up -> Point(x, fold.axis - abs(y - fold.axis))
        is Left -> Point(fold.axis - abs(x - fold.axis), y)
    } }.toMutableSet()
}

fun parseDotsAndFolds(input: List<String>): Pair<MutableSet<Point>, List<Fold>> {
    return input.fold(Pair(mutableSetOf<Point>(), mutableListOf<Fold>())) { (dots, folds), line ->
        when {
            line.isBlank() -> Unit
            line.startsWith("fold") -> folds += Fold.of(line)
            else -> {
                val (y, x) = line.split(",")
                // leaving with reversed y, x
                dots += Point(y.toInt(), x.toInt())
            }
        }
        Pair(dots, folds)
    }

}

fun printCode(points: Set<Point>) {
    val width = points.map { it.y }.maxOf { it }
    val height = points.map { it.x }.maxOf { it }
    println(width)
    println(height)
    for (x in 0..width) {
        for (y in 0..height) {
            points.find { it== Point(y, x) }?.let {
                print("#")
            } ?: print(" ")
        }
        println("")
    }
}

fun main() {
    fun part1(input: List<String>): Int {
        val (dots, folds) = parseDotsAndFolds(input)
        return folds.take(1).fold(dots) { acc, fold -> folder(acc.toSet(), fold) }.size
    }

    fun part2(input: List<String>): Unit {
        val (dots, folds) = parseDotsAndFolds(input)
        val code = folds.fold(dots) { acc, fold -> folder(acc.toSet(), fold) }
        printCode(code.toSet())
    }

    val input = readInput("Day13")
    println(part1(input))
    part2(input)
}