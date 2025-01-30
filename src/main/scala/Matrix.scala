import Helpers.print_matrix

import scala.annotation.targetName

type Mat = List[List[Double]]

class Matrix(m: Option[Mat]) {

  def transpose: Matrix = {
    def transpose_helper(m: Mat): Mat =
      m match {
        case Nil :: _ => Nil
        case m => m.map(_.head) :: transpose_helper(m.map(_.tail))
      }
    data match {
      case None => new Matrix(None)
      case Some(m) => new Matrix(Some(transpose_helper(m)))
    }

  }

  def map(f: Double => Double): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(m) => new Matrix(Some(m.map(_.map(f))))
    }
  }

  @targetName("-")
  def -(other: Matrix): Matrix = {
    // Functie ajutatoare ca sa verific daca se pot scade doua matrici
    def sameSize(other: Option[Mat]): Boolean = {
      (height, width, other.get.length, other.get.head.length) match {
        case (Some(h1), Some(w1), h2, w2) => h1 == h2 && w1 == w2
        case _ => false
      }
    }

    (data, other.data) match {
      case (None, None) => new Matrix(None)
      case (None, _) => new Matrix(None)
      case (_, None) => new Matrix(None)
      case (a, b) if sameSize(b) => new Matrix(Some(
        a.get.zip(b.get) // Va rezulta List[(List[double],List[Double]),...]
          .map(x => x._1.zip(x._2) // Aici va rezulta List[(Double, Double),...]
            .map(y => y._1 - y._2)) // Si aici doar face scaderea si reducerea la un element
      )
      )
      case _ => new Matrix(None)
    }
  }
  
  @targetName("*")
  def *(other: Matrix): Matrix = {
    def op(b:Mat, line: List[Double]): List[Double] = {
      b
        .map(c => c.zip(line)
          .map(x => x._1 * x._2)
          .reduce((x, y) => x + y)
        )
    }
    (data, other.data) match {
      case (None, None) => new Matrix(None)
      case (None, _) => new Matrix(None)
      case (_, None) => new Matrix(None)
      // Numarul de coloane din prima matrice trebuie sa fie egal cu numarul de linii din a doua matrice
      case (a, b) if a.get.head.length == b.get.length => new Matrix(Some(
        a.get.map(line => op(b.get.transpose, line))
      )
      )
      case _ => new Matrix(None)
    }
  }
  @targetName("++")
  def ++(x: Double): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(m) => new Matrix(Some(m.map(line => line :+ x)))
    }
  }

  def data: Option[Mat] = m
  def height: Option[Int] = {
    data match {
      case Some(x) => Some(x.length)
      case None => None
    }
  }
  def width: Option[Int] = {
    data match {
      case Some(x) => Some(x.head.length)
      case None => None
    }

  }
  override def toString: String = {
    data match {
      case None => "None"
      case _ => data.map(_.mkString(",")).mkString("\n")
    }
  }
}

object Matrix {
  def apply(data: Mat): Matrix = {
    data match {
      case Nil => new Matrix(None)
      case _ => new Matrix(Some(data))
    }
  }
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = {
    val m = dataset
      .data
      .tail // Ca sa nu iau headerul cu numele coloanelor
      .map(_.map(_.toDouble))
    new Matrix(Option(m))
  }
}
