fun parseGraph(input: List<String>): Map<Point, Int> {
    val graph = mutableMapOf<Point, Int>()
    input.forEachIndexed { row, line ->
        line.forEachIndexed { col, n ->
            graph[Point(row, col)] = "$n".toInt()
        }
    }
    return graph.toMap()
}

fun expandedGrid(grid: Map<Point, Int>): Map<Point, Int> {
    val height = grid.keys.maxOf { it.x } + 1
    val width = grid.keys.maxOf { it.y } + 1
    val expanded = mutableMapOf<Point, Int>()
    for (y in 0 until (height * 5)) {
        for (x in 0 until (width * 5)) {
            val risk = grid.getValue(Point(x % width, y % height)) + x / width + y / height
            expanded[Point(x, y)] = if (risk > 9) risk - 9 else risk
        }
    }
    return expanded
}

@JvmInline
value class Weight(val value: Int)

typealias WeightedPoint = Pair<Point, Weight>

fun addToQueue(point: Point, weight: Weight, queue: List<Pair<Point, Weight>>): List<WeightedPoint> {
    var currentIndex = 0
    var placeAt: Int? = null
    if (queue.isEmpty()) return listOf(Pair(point, weight))
    do {
        val currentWeight = queue[currentIndex].second
        when {
            weight.value <= currentWeight.value -> {
                placeAt = currentIndex
            }
            currentIndex == queue.size - 1      -> {
                placeAt = queue.size
            }
        }
        currentIndex++
    } while (placeAt == null)
    return queue.slice(0 until placeAt) + listOf(Pair(point, weight)) + queue.slice(placeAt until queue.size)
}


tailrec fun shortest(
    graph: Map<Point, Int>,
    distances: MutableMap<Point, Int>,
    queue: List<WeightedPoint>,
    target: Point
): Int {
    val shortest = queue.first().first
    var newQueue = queue.drop(1)
    if (shortest == target) return distances[shortest]!!
    val neighbors = shortest?.neighbors() as List<Point>
    neighbors.toList().forEach { point ->
        if (graph[point] !== null) {
            val distanceFromSource = distances[shortest]!! + graph[point]!!
            if (distanceFromSource < distances[point] ?: Int.MAX_VALUE) {
                distances[point] = distanceFromSource
                newQueue = addToQueue(point, Weight(distanceFromSource), newQueue)
            }
        }
    }
    return shortest(graph, distances, newQueue, target)
}

val start = listOf(Pair(Point(0, 0), Weight(0)))

fun main() {

    fun part1(input: List<String>): Int {
        val graph = parseGraph(input)
        return shortest(graph, mutableMapOf(Point(0, 0) to 0), start, graph.keys.last())
    }

    fun part2(input: List<String>): Int {
        val graph = expandedGrid(parseGraph(input))
        return shortest(graph, mutableMapOf(Point(0, 0) to 0), start, graph.keys.last())
    }

    val input = readInput("Day15")
    println(part1(input))
    println(part2(input))
}