
object batch_gradient_descent {

  val learningRate = 0.001


  def with_I_variable(trainingData: TrainingData): Map[String, Any] = {
    var thetaI: Vector[Double] = trainingData.trainingSet(0)._1.map { x => 0.0 }
    //our equation
    def hypothesis(xI: Vector[Double]): Double = {
      xI.zipWithIndex.foldLeft(.0) { case (sum, (x, i)) =>
        sum + x * thetaI(i)
      }
    }

    // function that calculate de cost, close to 0 is better
    def cost_function(trainingData: TrainingData): Double = {
      val sumOfSqrDifference = trainingData.trainingSet.foldLeft(0.0) { case (sum, (key, value)) =>
        sum + Math.pow(hypothesis(key) - value, 2)
      }
      (1.0 / (2.0 * trainingData.trainingSet.length)) * sumOfSqrDifference
    }

    hypothesis(trainingData.trainingSet(0)._1)

    var previousResult: Double = cost_function(trainingData)
    var currentResult = 0.0
    var iterations = 0



    //start iterating
    while (previousResult != currentResult && iterations < 10000000) {
      iterations += 1
      previousResult = cost_function(trainingData)
      //two loops like that is not really good...
      //make all the caulculation at the same time (no theta update before this part is done)
      val sumOfDifferenceMultipliedByKey = (0 to trainingData.xISize).map(j => {
        trainingData.trainingSet.map { case (xI, yI) =>
          (hypothesis(xI) - yI) * xI(j)
        }.sum
      })

      //update all the theta vector
      thetaI = thetaI.zipWithIndex.map { case (theta, i) =>
        theta - learningRate * (1.0 / trainingData.trainingSet.length) * sumOfDifferenceMultipliedByKey(i)
      }
      //calculate the new cost rate and start again
      currentResult = cost_function(trainingData)
    }

    //mean normalize the theta
    thetaI = thetaI.zipWithIndex.map { case (theta, i) =>
      if (i != thetaI.size - 1) thetaI(i) / trainingData.maxXi(i)
      else theta
    }

    //    print((thetaI.head - trainingData.avgsXi(0)) / (trainingData.maxXi(0) - trainingData.minXi(0)))

    Map("ThetaI" -> thetaI.mkString(","),
      "cost" -> currentResult,
      "iterations" -> iterations)

  }
}

case class TrainingData(trainingData: Array[(Vector[Double], Double)]) {
  val xISize = trainingData(0)._1.length

  var avgsXi = new Array[Double](xISize)
  val maxXi = new Array[Double](xISize)
  val minXi = new Array[Double](xISize)

  private val Ys = trainingData.map { case (xI, y) => y }
  private val avgY = Ys.sum / Ys.length

  trainingData.foreach { case (xI, yI) =>
    xI.zipWithIndex.foreach { case (x, i) =>
      avgsXi(i) += x
      if (x > maxXi(i)) maxXi(i) = x
      if (x < minXi(i)) minXi(i) = x
    }
  }

  avgsXi = avgsXi.map(x => x / trainingData.length)

  //   we had 1 for theta0 because it s the constant and apply mean normalization
  val trainingSet = trainingData.map { case (xI, yI) =>
    //    (xI.zipWithIndex.map { case (x, i) => (x - avgsXi(i)) / (maxXi(i) - minXi(i)) } :+ 1.0, yI)
    (xI.zipWithIndex.map { case (x, i) => x / maxXi(i) } :+ 1.0, yI)
  }

}

val training = new TrainingData(Array(Vector(2.0,2.0) -> 13.0, Vector(3.0,2.0) -> 18.0))

val result = batch_gradient_descent.with_I_variable(training)

print(result)
