fun main(args: Array<String>) {
    val n = 10
    var x = 0
    var y = 1
    print("First $n terms: ")
    for (i in 1..n) {
        print("$x, ")
        val sum = x + y
        x = y
        y = sum
    }
    println()
    print("Hello from Kotlin!")
}