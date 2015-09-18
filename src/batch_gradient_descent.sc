
object batch_gradient_descent {
  var O0: Double = 0
  var O1: Double = 0

  val learningRate = 0.001

  def with_one_variable(trainingData: Map[Int, Int]): (Double, Double, Double, Int) = {
    var previousResult: Double = cost_function(trainingData)
    var currentResult = 0.0
    var iterations = 0
    while (previousResult != currentResult) {
      iterations += 1
      previousResult = cost_function(trainingData)
      val sumOfDifference = trainingData.foldLeft(0.0) { case (sum, (key, value)) =>
        sum + hypothesis(key) - value
      }
      val sumOfDifferenceMultipliedByKey = trainingData.foldLeft(0.0) { case (sum, (key, value)) =>
        sum + (hypothesis(key) - value).toInt * key
      }

      O0 = O0 - learningRate * (1 / trainingData.size.toDouble) * sumOfDifference
      O1 = O1 - learningRate * (1 / trainingData.size.toDouble) * sumOfDifferenceMultipliedByKey


      currentResult = cost_function(trainingData)
    }
    (O0, O1, cost_function(trainingData), iterations)
  }

  def cost_function(trainingData: Map[Int, Int]): Double = {
    val sumOfSqrDifference = trainingData.foldLeft(0.0) { case (sum, (key, value)) =>
      sum + Math.pow(hypothesis(key) - value, 2)
    }
    (1 / (2 * trainingData.size.toDouble)) * sumOfSqrDifference
  }

  def hypothesis(x: Int): Double = {
    O0 + O1 * x
  }


}

val trainingData = Map(0 -> 1, 2 -> 5, 5 -> 12)
val result = batch_gradient_descent.with_one_variable(trainingData)
print(result)