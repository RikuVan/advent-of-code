import java.io.File

val WIDTH = 25
val HEIGHT = 6

val String.zeroLength get() = split("").filter { it == "0" }.size
fun Char.merge(other: Char) = when (this) {
    '2' -> other
    else -> this
}

object Day8 {
    var layers = File("src/main/resources/day8-input.txt").readText().chunked(WIDTH * HEIGHT)
    val layerToCheck =
        layers.reduce { layer, lastLayer -> if (lastLayer.zeroLength < layer.zeroLength) lastLayer else layer }
    
    val checksum = layerToCheck.groupBy { it }
        .filter { (k) -> k == '1' || k == '2' }
        .map { (k, v) -> v.size }
        .reduce { x, y -> x * y }

    val image = layers.reduce { acc, layer ->
        acc.zip(layer)
            .map { (f, s) -> f.merge(s) }
            .joinToString("")
            .replace('0', ' ')
    }.chunked(WIDTH)

    fun printAnswers() {
        println(checksum)
        for (i in image) {
            println(i)
        }
    }
}