import java.io.File
import kotlin.math.abs

data class Coordinates(val col: Int, val row: Int) {
    operator fun plus(direction: Coordinates) =
        Coordinates(col + direction.col, row + direction.row)
}

fun Coordinates.distanceFrom(other: Coordinates) = abs(this.col - other.col) + abs(this.row + other.row)

val directions = mapOf(
    'L' to Coordinates(-1, 0),
    'R' to Coordinates(1, 0),
    'U' to Coordinates(0, 1),
    'D' to Coordinates(0, -1)
)

object Day3 {
    val wires = File("src/main/resources/day3-input.txt").useLines { it.toList() }

    val coordinates = wires.map { it.parseWire() }

    val intersections = coordinates[0].intersect(coordinates[1]).filter { int -> int != Coordinates(0, 0) }

    val partOneDistance = intersections.map { it.distanceFrom(Coordinates(0, 0)) }.min()

    val partToMinSteps = intersections.map { i -> coordinates[0].indexOf(i) + coordinates[1].indexOf(i) }.min()

    fun printAnswer() {
        println(partOneDistance)
        println(partToMinSteps)
    }
}

fun String.parseinstruction(): Pair<Char, Int> {
    val dir = this[0]
    val num = this.drop(1).toInt()
    return Pair(dir, num)
}

fun String.parseWire(): List<Coordinates> {
    return this.split(",")
        .map { instr -> instr.parseinstruction() }
        .fold(mutableListOf(Coordinates(0, 0)), { coords, instr ->
            val direction = directions.get(instr.first) as Coordinates
            repeat(instr.second, { coords.add(coords.last() + direction) })
            coords
        })
}
