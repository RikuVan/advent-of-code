data class Point(val x: Int, val y: Int)

fun List<List<Int>>.includes(point: Point): Boolean {
    val (x, y) = point
    return x >= 0 && x <= size - 1 && y >= 0 && y <= this[0].size - 1
}

fun List<List<Int>>.get(point: Point) = try {
    this[point.x][point.y]
} catch (e: Exception) {
    9
}

fun Point.neighbors() =
    listOf(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1)).filter { p -> p.x >= 0 && p.y >= 0 }

fun lowPoints(input: List<List<Int>>): List<Point> {
    val lowPoints = mutableListOf<Point>()
    input.forEachIndexed { i, row ->
        row.forEachIndexed { k, n ->
            // left
            if (k !== 0 && row[k - 1] <= n) return@forEachIndexed
            // right
            if (k !== row.size - 1 && row[k + 1] <= n) return@forEachIndexed
            // up
            if (i !== 0 && input[i - 1][k] <= n) return@forEachIndexed
            // down
            if (i !== input.size - 1 && input[i + 1][k] <= n) return@forEachIndexed
            lowPoints += Point(i, k)
        }
    }
    return lowPoints.toList()
}

fun basin(point: Point, grid: List<List<Int>>, seen: MutableSet<Point>): MutableSet<Point> {
    if (!grid.includes(point) || grid.get(point) == 9) {
        return seen
    }
    seen.add(point)
    return point.neighbors().fold(seen) { s, p ->
        if (!s.contains(p)) {
            basin(p, grid, s)
        } else {
            s
        }
    }
}


fun main() {
    fun part1(input: List<List<Int>>): Int {
        val points = lowPoints(input)
        return points.sumOf { (x, y) -> input[x][y] + 1 }
    }

    fun part2(input: List<List<Int>>): Any {
        return lowPoints(input).map { basin(it, input, mutableSetOf<Point>()) }.map {
            it.size
        }.sorted().reversed().take(3).reduce { acc, i -> acc * i }
    }

    val input = readInput("Day09").map { line -> line.toList().map { it.digitToInt() } }
    println(part1(input))
    println(part2(input))
}
