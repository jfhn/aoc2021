import java.io.File

fun main() {
	val input = File("input.txt").readLines().filter { !it.isBlank() }

	val bingoInput      = input[0].split(",").map { it.toInt() };
	val numBingoFields = (input.size - 1) / 5;

	val bingoFields = Array<BingoField>(numBingoFields) { BingoField() }

	for (i in 0 .. numBingoFields - 1) {
		for (j in 0..4) {
			val fields = "[0-9]+".toRegex()
				.findAll(input[i * 5 + j + 1])
				.map { it.value.toInt() }
				.toList()
			bingoFields[i].addNext(fields)
		}
	}

	println("Part 1: " + solve1(bingoInput, bingoFields))
	println("Part 2: " + solve2(bingoInput, bingoFields))
}

class BingoField {
	// Index with x * 5 + y
	val fields = IntArray(25)

	private var writeIdx = 0
	private var marked   = BooleanArray(25) { false }

	fun addNext(vs: List<Int>) {
		vs.forEach { fields[writeIdx++] = it }
	}

	fun mark(num: Int) {
		val idx = fields.indexOf(num)
		if (idx != -1) {
			marked[idx] = true
		}
	}

	fun hasWon(): Boolean {
		// Rows
		return marked[0]  && marked[1]  && marked[2]  && marked[3]  && marked[4] ||
		marked[5]  && marked[6]  && marked[7]  && marked[8]  && marked[9]  ||
		marked[10] && marked[11] && marked[12] && marked[13] && marked[14] ||
		marked[15] && marked[16] && marked[17] && marked[18] && marked[19] ||
		marked[20] && marked[21] && marked[22] && marked[23] && marked[24] ||

		// Columns
		marked[0] && marked[5] && marked[10] && marked[15] && marked[20] ||
		marked[1] && marked[6] && marked[11] && marked[16] && marked[21] ||
		marked[2] && marked[7] && marked[12] && marked[17] && marked[22] ||
		marked[3] && marked[8] && marked[13] && marked[18] && marked[23] ||
		marked[4] && marked[9] && marked[14] && marked[19] && marked[24]
	}

	fun sumOfUnmarked(): Int {
		var sum = 0
		for (i in 0..24) {
			if (!marked[i]) {
				sum += fields[i]
			}
		}
		return sum
	}

	override fun toString(): String {
		var str = StringBuilder()
		str.append("Field:\n")
		for (i in 0..4) {
			for (j in 0..4) {
				str.append(fields[i * 5 + j])
				str.append(" ")
			}
			str.append("\n")
		}
		return str.toString()
	}
}

fun solve1(input: List<Int>, fields: Array<BingoField>): Int {
	input.forEach { input ->
		fields.forEach {
			it.mark(input)
			if (it.hasWon()) {
				return it.sumOfUnmarked() * input;
			}
		}
	}

	return -1
}

fun solve2(input: List<Int>, fields: Array<BingoField>): Int {
	var counter = 0

	input.forEach { input ->
		fields.forEach {
			if (!it.hasWon()) {
				it.mark(input)
				if (it.hasWon()) {
					counter++;
				}
				if (counter == fields.size - 1) {
					return it.sumOfUnmarked() * input;
				}
			}
		}
	}

	return -1
}
