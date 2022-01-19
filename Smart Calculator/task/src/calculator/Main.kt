package calculator

fun main() {
    var output = ""
    while (output != "Bye!") {
        output = ""
        val input = readLine()!!.trim()
        when {
            input == "" -> continue
            input == "/exit" -> output = "Bye!"
            input == "/help" -> output = """Long help message.""".trimIndent()
            input.first() == '/' -> output = "Unknown command"
            input.contains("=") -> {  // assigment
                val (name, expression) = input.split("=", limit = 2)
                try { Variables.setVariable(name.trim(), calc(expression.trim())!!) }
                catch (e: MyErrorException) { println(e.message) }
            }
            else -> try { output = calc(input)!!.toString() } // expression
            catch (e: MyErrorException) { println(e.message) }
        }
        if (output != "") println(output)
    }
}

fun calc(expression: String): MyBigNumber? {
    val parser = ExpressionParser(expression)
    val calculator = Calculator()
    parser.forEach { case ->
        when (case) {
            'N' -> calculator.setNumberValue(parser.numValue)
            'V' -> calculator.setNumberValue(Variables.getValue(parser.strValue))
            else -> calculator.process(case)
        }
    }
    calculator.process('=')
    return calculator.getLastResult()
}

class MyErrorException(errorMessage: String): Exception(errorMessage)

class ExpressionParser(private val expression: String): Iterator<Char> {
    private val regexes = mapOf(
        'S' to "^-?[a-zA-Z0-9]+\\s*".toRegex(),
        '(' to "^\\(\\s*".toRegex(),
        ')' to "^\\)\\s*".toRegex(),
        '+' to "^[+-]+\\s*".toRegex(),
        '*' to "^\\*\\s*".toRegex(),
        '/' to "^/\\s*".toRegex(),
        '^' to "^\\^\\s*".toRegex(),
        'N' to "^-?\\d+\\s*\$".toRegex(),
        'V' to "^-?[a-zA-Z]+\\s*$".toRegex())

    private var pos = 0
    var numValue = MyBigNumber()
    var strValue = ""

    override fun hasNext(): Boolean =  pos < expression.length

    override fun next(): Char {
        regexes.forEach { regx ->
            val text = regx.value.find(expression.substring(pos))?.value
            if (text != null ){
                pos += text.length
                when (regx.key) {
                    'S' -> when {
                        text.trim().matches(regexes['N']!!) -> { numValue = MyBigNumber(text.trim()); return 'N' }
                        text.trim().matches(regexes['V']!!) -> { strValue = text.trim(); return 'V' }
                        else -> throw MyErrorException ("Invalid identifier")
                    }
                    '+' -> if (text.filter { it == '-' }.count() % 2 == 1) return '-'
                }
                return regx.key
            }
        }
        throw MyErrorException ("Invalid expression")
    }
}


object Variables {
    var regex = "^[a-zA-Z]+$".toRegex()
    private val map = mutableMapOf<String, MyBigNumber>()

    fun getValue(name: String): MyBigNumber {
        var sign = 1
        var nname = name
        if (name.startsWith('-')) {
            sign = -1
            nname = name.substring(1)
        }
        if (!regex.matches(nname)) throw MyErrorException("Invalid identifier")
        if (!map.containsKey(nname)) throw MyErrorException("Unknown variable")

        return MyBigNumber(map[name]!!.getPolinom(), map[name]!!.getSign() * sign)
    }

    fun setVariable(name: String, value: MyBigNumber) {
        if (!regex.matches(name)) throw MyErrorException("Invalid identifier")
        map[name] = value
    }
}


class Calculator {
    private val pow = fun(b: MyBigNumber, a: MyBigNumber) = b.pow(a)

    private val div = fun(b: Int, a: Int): Int {
        if (b == 0 ) throw MyErrorException("Division by zero")
        return a / b
    }
    private val funcOf = mapOf<Char, (MyBigNumber, MyBigNumber) -> MyBigNumber >(
        '+' to { b: MyBigNumber, a: MyBigNumber -> a.plus(b) },
        '-' to { b: MyBigNumber, a: MyBigNumber -> a.minus(b) },
        '*' to { b: MyBigNumber, a: MyBigNumber -> a.mul(b) },
        '/' to { b: MyBigNumber, a: MyBigNumber -> a.div(b).first },
        '^' to { b: MyBigNumber, a: MyBigNumber -> a.pow(b) })

    private val priority = mapOf('+' to 1, '-' to 1, '*' to 2, '/' to 2, '^' to 4,
        '(' to Integer.MIN_VALUE, '_' to Integer.MIN_VALUE)

    private val nStack = SimpleStack<MyBigNumber>()
    private val opStack = SimpleStack<Char>('_')
    private val seqChecker = SequenceChecker()

    fun setNumberValue(value: MyBigNumber) {
        if(!seqChecker.accept('N')) throw MyErrorException("Invalid expression")
        nStack.push(value)
    }

    fun getLastResult() = nStack.top()

    fun process(command: Char) {
        if (!seqChecker.accept(command)) throw MyErrorException("Invalid expression")
        when (command) {
            '(' -> opStack.push(command)
            ')' -> {
                while (opStack.top() != '(')
                    nStack.push(funcOf[opStack.pop()]!!(nStack.pop()!!, nStack.pop()!!))
                opStack.pop()
            }
            '=' -> while (opStack.top() != '_')
                nStack.push(funcOf[opStack.pop()]!!(nStack.pop()!!, nStack.pop()!!))
            in "+-*/^" -> { while (priority[opStack.top()]!! >= priority[command]!!)
                nStack.push(funcOf[opStack.pop()]!!(nStack.pop()!!, nStack.pop()!!))
                opStack.push(command)
            }
            else -> throw MyErrorException("Unknown command $command")
        }
    }
}


class SimpleStack<T>(val bootom: T? = null) {
    private val list = mutableListOf<T>()
    fun push(arg: T) { list.add(arg) }
    fun top(): T? = if (list.isNotEmpty()) list.last() else bootom
    fun pop(): T? {
        if (list.isNotEmpty()) {
            val res = list.last()
            list.removeAt(list.lastIndex)
            return res
        }
        return bootom
    }
}


class SequenceChecker {
    private var leftBrackets = 0
    private var lastCommand = '_'

    fun accept(command: Char): Boolean {
        var res: Boolean = when (command) {
            'N' -> lastCommand in "+-*/^(_"
            in "+-*/^" -> lastCommand in "NV)"
            '(' -> { leftBrackets++; lastCommand in "+-*/^(_" }
            ')' -> { leftBrackets--; lastCommand in "NV)" && leftBrackets >= 0 }
            '=' -> lastCommand in "NV)" && leftBrackets == 0
            else -> true
        }
        if (res) lastCommand = command
        return res
    }
}


class MyBigNumber (

    private var polinom: MutableList <UInt> = mutableListOf<UInt> (0u),
    private var sign: Int = if ( polinom.any { it != 0u } ) 1 else 0) {

    companion object { // My implementation of unsigned polynomial arithmetic

        val RADIX = UInt.MAX_VALUE.toULong() + 1uL
        // BigNumber.MAXVALUE == 4294967296 ^ 2147483647 - 1   (MAX SIZE 16GB)

        private fun MutableList<UInt>.compareTo ( other: MutableList<UInt> ): Int {
            val bySize = indexOfLast { it != 0u }.compareTo (other.indexOfLast { it != 0u })
            if (bySize != 0) return bySize
            for ( index in lastIndex downTo 0 ) {
                val byElements = get(index).compareTo (other.getn(index))
                if (byElements != 0) return byElements
            }
            return 0
        }

        private fun copyOf ( mutableList: MutableList<UInt> ) = mutableList.toMutableList()

        private fun MutableList<UInt>.getn (index: Int) = getOrElse(index) { 0u }

        private fun MutableList<UInt>.setn (index: Int, item: UInt): MutableList<UInt> {
            while (index > lastIndex) add (0u)
            set(index, item)
            return this
        }

        private fun MutableList<UInt>.rise (times: Int = 1): MutableList<UInt> {
            if (any { it != 0u }) for (i in 1..times) add(0, 0u)
            return this
        }

        private fun MutableList<UInt>.trim (): MutableList<UInt>{
            while (this.size > 1 && last() == 0u) removeAt(this.lastIndex)
            return this
        }

        private fun MutableList<UInt>.plus (uInt: UInt = 1u): MutableList<UInt> {
            var index = 0
            var sum = uInt.toULong()
            while (sum > 0u) {
                sum += getn(index)
                setn(index,(sum % RADIX).toUInt())
                sum /= RADIX
                index++
            }
            return this
        }

        private fun MutableList<UInt>.plus (otherList: MutableList<UInt>): MutableList<UInt> {
            if (otherList.any{ it != 0u }) {
                var uLSum = 0uL
                for (index in 0..maxOf(lastIndex, otherList.lastIndex)) {
                    uLSum = uLSum / RADIX + getn(index).toULong() + otherList.getn(index).toULong()
                    setn(index, (uLSum % RADIX).toUInt())
                    uLSum -= get(index)
                }
                if (uLSum > 0uL) add((uLSum / RADIX).toUInt())
            }
            return this
        }

        private fun MutableList<UInt>.diff (uInt: UInt): MutableList<UInt>{
            if (first() >= uInt) this[0] -= uInt
            else diff(mutableListOf(uInt))
            return trim()
        }

        private fun MutableList<UInt>.diff (otherList: MutableList<UInt>): MutableList<UInt>{
            val compare = compareTo(otherList)
            var over = 0u
            for (index in 0..maxOf(lastIndex, otherList.lastIndex)) {
                var diff = if (compare > 0) RADIX - over + getn(index) - otherList.getn(index)
                else RADIX - over + otherList.getn(index) - getn(index)
                over = 1u - (diff / RADIX).toUInt()
                setn(index, (diff % RADIX).toUInt())
            }
            return trim()
        }

        private fun MutableList<UInt>.mul (uInt: UInt): MutableList<UInt> {
            var mul = 0uL
            for (index in indices) {
                mul += uInt.toULong() * get(index)
                set (index, (mul % RADIX).toUInt())
                mul /= RADIX
            }
            if (mul > 0uL) add(mul.toUInt())
            return trim()
        }

        private fun MutableList<UInt>.div (uInt: UInt): Pair<MutableList<UInt>, UInt> {
            var remainder = 0uL
            for (index in lastIndex downTo 0) {
                remainder = remainder * RADIX + get(index)
                set(index, (remainder / uInt).toUInt())
                remainder %= uInt
            }
            return Pair(trim(), remainder.toUInt())
        }

    }

    constructor (string: String) : this() {
        val start = string.indexOfFirst { it !in "-0" }
        if (start >= 0)
            for (index in start until string.length) {
                polinom.mul(10u).plus(string[index].digitToInt().toUInt())
            } else polinom.add(0u)
        sign = if (polinom.all { it == 0u }) 0 else if (string.startsWith('-')) -1 else 1
    }

    fun getSign() = sign

    fun getPolinom() = copyOf(polinom)

    fun plus (other: MyBigNumber): MyBigNumber = when {
        sign == 0 -> MyBigNumber(copyOf(other.polinom), other.sign)
        other.sign == 0 -> MyBigNumber(copyOf(polinom), sign)
        sign * other.sign > 0 -> MyBigNumber(copyOf(polinom).plus(other.polinom), sign)
        else -> MyBigNumber(copyOf(polinom).diff(other.polinom), sign * polinom.compareTo(other.polinom))
    }

    fun minus (other: MyBigNumber) = plus(MyBigNumber(other.polinom, other.sign * -1))

    fun mul (other: MyBigNumber): MyBigNumber {
        val newSignFactor = sign * other.sign
        if (newSignFactor == 0) return MyBigNumber()
        val newPolinom = mutableListOf(0u)
        for (index in other.polinom.indices) newPolinom.plus(copyOf(polinom).mul(other.polinom[index]).rise(index))
        return MyBigNumber(newPolinom.trim(), newSignFactor)
    }

    fun div (other: MyBigNumber): Pair<MyBigNumber, MyBigNumber> {
        var resultSign = sign / other.sign  // can thow ZeroDivision exception
        if (resultSign == 0) return Pair(MyBigNumber(), MyBigNumber())
        if (polinom.compareTo(other.polinom) < 0) return Pair(MyBigNumber(), MyBigNumber(copyOf(polinom), sign))

        val remainder = copyOf(polinom)
        val divider = copyOf(other.polinom)
        val result = mutableListOf(0u)
        var rank = remainder.size - divider.size
        var dirrection = 1

        while (remainder.compareTo(divider) > 0) {
            val remainderHead = RADIX * remainder.getn(divider.size + rank) +
                    remainder.getn(divider.lastIndex + rank)
            var approxFactor = (remainderHead / divider.last()).toUInt()
            if (approxFactor <= 0u) {
                rank--
                continue
            }
            if (dirrection > 0) result.plus(mutableListOf(approxFactor).rise(rank))
            else result.diff(mutableListOf(approxFactor).rise(rank))

            val subtrahend = copyOf(divider).mul(approxFactor).rise(rank)
            dirrection *= remainder.compareTo(subtrahend)
            remainder.diff(subtrahend)
        }
        if (dirrection < 0) {
            remainder.diff(divider)
            result.diff(mutableListOf(1u))
        }
        return Pair(MyBigNumber(result, resultSign), MyBigNumber(remainder, sign))
    }

    fun pow (other: MyBigNumber): MyBigNumber {
        if (other.sign < 0 ) return MyBigNumber()
        if (!other.polinom.any { it != 0u}) return MyBigNumber(mutableListOf(1u), 1)
        var power = copyOf(other.polinom)
        var result = copyOf(polinom)
        power.diff(1u)
        while ( power.any{ it != 0u }) {
            val newResult = mutableListOf(0u)
            for (index in polinom.indices)
                newResult.plus(result.mul(polinom[index]).rise(index))
            result = newResult
            power.diff(1u)
        }
        return MyBigNumber(result, if (other.polinom.first() % 2u == 0u) 1 else sign)
    }

    override fun toString(): String {
        val stringBuilder = StringBuilder()
        val mutableList = polinom.toMutableList()
        while (mutableList.any { it != 0u } ) stringBuilder.append(mutableList.div(10u).second)
        if (stringBuilder.isEmpty()) stringBuilder.append("0")
        if (sign < 0) stringBuilder.append("-")
        return stringBuilder.toString().reversed()
    }

}