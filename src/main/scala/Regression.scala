import Dataset.*

import scala.annotation.tailrec

object Regression {
  
  // Functia de calculare a erorii pt gds
  def calculate_error(estimate_vector: List[List[Double]], Y: List[List[Double]]): Matrix = {
    Matrix.apply(
      estimate_vector zip Y map (x => List((x._1.head - x._2.head).abs))
    )

  }


  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {
    // Obtin datasetul mare
    val dataset = Dataset.apply(dataset_file)
    // Iau doar coloanele care ma intereseaza
    val selectedColumns = dataset.selectColumns(value_column +: attribute_columns)
    // Realizez split-ul
    val (ds_train, ds_test) = selectedColumns.split(test_percentage)
    // In x iau coloanele fara cea de valoare si adaug inca o coloana de 1
    val (x, test) = (Matrix.apply(ds_train.selectColumns(attribute_columns)).++(1.0),
                Matrix.apply(ds_test))

    val ds_estimate_column = ds_train.selectColumn(value_column)

    val estimate_column = Matrix.apply(ds_estimate_column)

    @tailrec
    def gds(gradient_descent_steps: Int, W: Matrix): Matrix = {
      val estimate = x * W
      val error = estimate - estimate_column
      val gradient = (x.transpose * error).data.get.map(z => z.map(_ / error.height.get))
      if (gradient_descent_steps > 0) {
        gds(gradient_descent_steps - 1,
          W - Matrix.apply(gradient.map(x => x.map(_ * alpha))))
      } else {
        W
      }
    }

    // List.fill(x.data.head.length)(List(0.0)) produce o lista de liste de un singur element 0.0
    val W = gds(gradient_descent_steps,
      Matrix.apply(List.fill(x.data.get.head.length)(List(0.0)))) // Estimarea coeficientilor
    val error = calculate_error((test * W).data.get, estimate_column.data.get)
      .data.get.map(x => x.head).sum / test.data.get.length
    (W, error)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}