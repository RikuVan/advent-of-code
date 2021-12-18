data class Mass(val value: Int)
val Mass.requiredFuel get() = this.value / 3 - 2

object Day1 {
    val input = File("src/main/resources/day1-input.txt").useLines { it.toList() }
    val fuels = input.map { str -> Mass(str.toInt()).requiredFuel }
    val part1 = fuels.takeWhile { it > 0 }.sum()
    val part2 = fuels.map { item -> generateSequence(item) { x -> Mass(x).requiredFuel.takeIf { it > 0 } }.sum() }.sum()

    fun printAnswers() {
        println(part1)
        println(part2)
    }
}