typealias Caves = Map<String, List<String>>

fun String.isUpperCase(): Boolean = all { it.isUpperCase() }

typealias VisitCheck = (cave: String, path: List<String>) -> Boolean

val part1VisitCheck: VisitCheck =  { cave, path -> cave.isUpperCase() || cave !in path }
val part2VisitCheck: VisitCheck = { cave, path ->
    when {
        cave == "start" -> false
        cave.isUpperCase() -> true
        cave !in path -> true
        else -> path.filter { !it.isUpperCase() }.groupBy { it }
            .none { it.value.size == 2 }
    } }

fun walk(
    path: List<String>,
    caves: Caves,
    mayVisit: (String, List<String>) -> Boolean = part1VisitCheck
): List<List<String>> {
    val cave = path.last()
    return when(cave) {
        "end" -> {
            // println("ok path: $path")
            listOf(path)
        }
        else  -> caves[cave]!!.filter {
            mayVisit(it, path)
        }.flatMap {
            walk(path + it, caves, mayVisit)
        }
    }
}

fun cavesEdges(input: List<String>): Caves {
    return input.map { it.split("-") }
        .flatMap { (head, tail) ->
            listOf(
                head to tail,
                tail to head
            )
        }
        .groupBy({ it.first }, { it.second })
}


fun main() {
    fun part1(input: List<String>): Int {
        return walk(listOf("start"), cavesEdges(input)).size
    }

    fun part2(input: List<String>): Int {
        return walk(listOf("start"), cavesEdges(input), part2VisitCheck).size
    }

    val input = readInput("Day12")
    println(part1(input))
    println(part2(input))
}