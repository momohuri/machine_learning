
object batch_gradient_descent {
  def algorithm(trainingData: Map[Int, Int]): Float = {
    cost_function(trainingData, hypothesis)

    //    val b = Array(1, 2)

  }

  def cost_function(trainingData: Map[Int, Int], hypothesis: (Int) => Int): Float = {
    val sumOfSqrtDifference = trainingData.foldLeft(0) { case (sum, (key, value)) =>
      sum + Math.pow((hypothesis(key) - value).toDouble, 2).toInt
    }
    (1 / (2 * trainingData.size).toFloat) * sumOfSqrtDifference
  }

  def hypothesis(x: Int): Int = {
    2 * x + 1
  }


}

val trainingData = Map(0 -> 1, 2 -> 5, 5 -> 12)
val result = batch_gradient_descent.algorithm(trainingData)
print(result)