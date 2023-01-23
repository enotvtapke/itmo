import parser.Parser
import java.io.File

fun main() {
//    val res = Parser("var ar: Array<Map<Int,String>>").parse()
    val res = Parser("var ar: Map<Array<Int>, Mock<String>>").parse()
//    val res = Parser("var ar: Array<Int>").parse()

    println(res)
    File("./parseTree.dot").writeText(res.toString())
}