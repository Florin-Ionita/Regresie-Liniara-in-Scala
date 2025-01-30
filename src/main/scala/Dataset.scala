import scala.io.Source
import Helpers.print_matrix

import scala.annotation.tailrec

class Dataset(m: List[List[String]]) {

  val data: List[List[String]] = m

  override def toString: String = print_matrix(data) // functie luata din helper

  def selectColumn(col: String): Dataset = {
    val index = data.head.indexOf(col)
    new Dataset(data.map(line => List(line(index))))
  }

  def selectColumns(cols: List[String]): Dataset = {
    // Aplica pe fiecare element din cols selectColumn si dupa
    // Folosind reduce concateneaza listele intre ele cu toate elementele
    new Dataset(cols
      .map(selectColumn(_).data)
      .reduce((a, b) => a.zip(b).map(x => x._1 ::: x._2)))
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sort = data.tail.sortBy(line => line.head)
    
    @tailrec
    def split_helper(train: List[List[String]],
                     test: List[List[String]],
                     acc: Int,
                     data: List[List[String]])
    : (List[List[String]], List[List[String]]) = {
      if (data.isEmpty) (train, test)
      else if (acc < 1/percentage - 1) {
        split_helper(train :+ data.head, test, acc + 1, data.tail)
      } else {
        split_helper(train, test :+ data.head, 0, data.tail)
      }
    }
    
    val (train, test) = split_helper(List(),
                            List(),
                            0,
                            sort)
    (new Dataset(data.head :: train),
      new Dataset(data.head :: test))
  }

  def size: Int = data.length
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {

  def apply(csv_filename: String): Dataset = {
    new Dataset(Source
      .fromFile(csv_filename)
      .getLines()
      .toList
      .map(_.split(",").toList))
  }

  def apply(ds: List[List[String]]): Dataset = {
    new Dataset(ds)
  }
}