object Helpers {
  def print_matrix(data: List[List[String]]): String = {
    // Met1: 
    data.map(_.mkString(",")).mkString("\n")
    // Met 2
    // Am incercat dar nu mi-a reusit
    //    data
    //      // Iau toate elementele de pe linie si le unesc cu ,
    //      .map(line =>
    //        line.foldLeft("")((total, field) => total.+(field).+(",")))
    //      // Iau toate liniile si le unesc cu \n
    //      .andThen(dataset =>
    //        dataset.foldRight("": String)((line, total) => total.+(line).+("\n")))
    //      // In final este un List de un singur string deci il convertesc
    //      .toString()
  }

}
