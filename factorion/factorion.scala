object Calculator extends App {
	val factors = scala.collection.mutable.Map[Int, Int]()

	// Turns the number into an array of digits
	def getDigits(number :Int):Array[Int] = {
		var total = number
		var n = total.toString.length
		var digits = new Array[Int](n)

		for (x <- 1 until n+1) {
			digits(x-1) = total%10
			total = (total - total%10) / 10
		}

		return digits
	}

	def isFactorion(number :Int):Boolean = {
		var digits = getDigits(number)
		digits = digits.map( x => factors(x) )
		var total = 0
		digits foreach (total += _)
		if (number == total) {
			return true
		}

		return false
	}

	// Precalculate factorials
	factors(0) = 1
	for (i <- 1 until 10) {
		factors(i) = factors(i-1) * i
	}

	// Calculate factorions
	var start = System.nanoTime
	for (i <- 0 until 2500001) {
		if (isFactorion(i)) {
			println(i)
		}
	}
	var end = System.nanoTime
	println("-- "+ (end-start)/1000000+ " ms")

}