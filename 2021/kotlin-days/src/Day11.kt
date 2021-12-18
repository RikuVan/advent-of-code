typealias Grid =  MutableMap<Point, Int>

fun Point.allNeighbors() =
    mutableListOf(
        Point(x -1, y - 1), Point(x - 1, y), Point(x - 1, y + 1),
        Point(x, y - 1), /*     Point     */ Point(x, y + 1),
        Point(x + 1, y - 1), Point(x + 1, y), Point(x + 1, y + 1)
    )

fun List<String>.toGrid(): Grid =
    foldIndexed(mutableMapOf<Point, Int>()) { row, map, line  ->
        line.mapIndexed { col, ch ->
            map[Point(row, col)] = "$ch".toInt()
        }
        map
    }

fun flash(grid: Grid): Int {
    val flashers = grid.filterValues { it > 9 }.keys
    if (flashers.isEmpty()) return grid.values.count { it == 0 }
    flashers.forEach { grid[it] = 0 }

    flashers
        .flatMap { it.allNeighbors() }
        .filter { it in grid && grid[it] != 0 }
        .forEach { grid[it] = grid.getValue(it) + 1 }

    return flash(grid)
}

fun step(grid: Grid): Int {
    grid.forEach { (point, energy) -> grid[point] = energy + 1 }
    return flash(grid)
}

fun inspect(grid: Grid) {
    grid.keys.forEach{ point ->
        print(grid[point])
        if (point.y === 9) {
            print("\n")
        }
    }
    println()
}

fun main() {
    fun part1(input: List<String>): Int {
        val grid = input.toGrid()
        var total = 0
        repeat(100) {
            total += step(grid)
        }
        return total
    }

    fun part2(input: List<String>): Int {
        val grid = input.toGrid()
        var allFlashed: Int? = null
        var step = 0
        while(allFlashed == null) {
            step++
            if(step(grid) == grid.size) {
                allFlashed = step
            }
        }
        return allFlashed
    }

    val input = readInput("Day11")
    println(part1(input))
    println(part2(input))
}
