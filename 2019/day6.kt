import java.io.File
import java.lang.Exception

fun addOrbiters(p: Planet?, newP: Planet) {
    if (p is Planet) {
        p.orbiters.add(newP)
        if (p.parent is Planet) addOrbiters(p.parent, newP)
    }
}

data class Planet(val code: String) {
    var orbiters: MutableList<Planet> = mutableListOf()
    var parent: Planet? = null
    var parents = 0

    fun addChild(planet: Planet) {
        orbiters.add(planet)
        planet.parent = this
        addOrbiters(parent, planet)
    }

    override operator fun equals(other: Any?): Boolean {
        if (other is Planet) {
            return code == other.code
        }
        return false
    }
}

val Planet.isCOM get() = this.code == "COM"

fun String.toPlanet(): List<Planet> = this.split(")").map { Planet(it) }

object Day6 {
    val planetPairs = File("src/main/resources/day6-input.txt").readLines().map { it.toPlanet() }

    fun getAllOrbits(current: Planet, pairs: List<List<Planet>>, parents: Int) {
        current.parents = parents
        for (i in pairs) {
            if (i[0].code == current.code) {
                current.addChild(i[1])
                getAllOrbits(i[1], pairs, parents + 1)
            }
        }
    }

    fun createUniverse(): Planet {
        val copy = planetPairs.toMutableList()
        val comPair = planetPairs.find { it[0].isCOM }
        val root = comPair!![0]
        val current = comPair[1]
        root.addChild(current)
        getAllOrbits(current, copy, 1)
        return root
    }

    val universe = createUniverse()
    val numOfOrbits = universe.orbiters.map {it.parents }.sum()

    val youParents = universe.orbiters.filter { it.orbiters.contains(Planet("YOU")) }
    val sanParents = universe.orbiters.filter { it.orbiters.contains(Planet("SAN")) }
    val moves = (youParents + sanParents).filter { !(youParents.contains(it) && sanParents.contains(it)) }.size


    fun printAnswers() {
        println("part 1 answer: $numOfOrbits")
        println("part 2 answer: $moves")
    }
}