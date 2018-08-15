import scala.io.Source

trait FileUtil {

  def getFileData(filePath: String): Seq[String] = {
    val bufferSource = Source.fromResource(filePath)
    val fileRows = bufferSource
      .getLines()
      .filter(_.nonEmpty)
      .toSeq
    fileRows.map(_.replace("\"", ""))
  }

  def getAirports: Seq[Airport] = {
    val airportsInfo = getFileData("airports.csv")
    airportsInfo.tail.map{row =>
      val data = row.trim.split(",")
      val id: Int = data.lift(0).map(_.toInt).getOrElse(throw new RuntimeException(s"Unable to extract airport ID from $row"))
      val ident: String = data.lift(1).getOrElse(throw new RuntimeException(s"Unable to extract airport identifier from $row"))
      val airport_type: String = data.lift(2).getOrElse(throw new RuntimeException(s"Unable to extract airport type from $row"))
      val name: String = data.lift(3).getOrElse(throw new RuntimeException(s"Unable to extract airport name from $row"))
      val iso_country: String = data.lift(8).getOrElse(throw new RuntimeException(s"Unable to extract airport country of location from $row"))
      Airport(id, ident, airport_type, name, iso_country)
    }
  }

  def getCountries: Seq[Country] = {
    val airportsInfo = getFileData("countries.csv")
    airportsInfo.tail.map{row =>
      val data = row.trim.split(",")
      val id: Int = data.lift(0).map(_.toInt).getOrElse(throw new RuntimeException(s"Unable to extract country ID from $row"))
      val code: String = data.lift(1).getOrElse(throw new RuntimeException(s"Unable to extract country Code from $row"))
      val name: String = data.lift(2).getOrElse(throw new RuntimeException(s"Unable to extract country Name from $row"))
      Country(id, code, name)
    }
  }

  def getRunways: Seq[Runway] = {
    val airportsInfo = getFileData("runways.csv")
    airportsInfo.tail.map{row =>
      val data = row.trim.split(",")
      val id: Int = data.lift(0).map(_.toInt).getOrElse(throw new RuntimeException(s"Unable to extract runway ID from $row"))
      val airportRef: Int = data.lift(1).map(_.toInt).getOrElse(throw new RuntimeException(s"Unable to extract runways airport ref from $row"))
      val airportIdent: String = data.lift(2).getOrElse(throw new RuntimeException(s"Unable to extract runways airport identifier Name from $row"))
      val length: Option[Int] = data.lift(3).filter(l => !l.isEmpty).map(_.toInt)
      val width: Option[Int] = data.lift(4).filter(w => !w.isEmpty).map(_.toInt)
      val surface: String = data.lift(5).getOrElse(throw new RuntimeException(s"Unable to extract runways surface from $row"))
      val runwayIdent: Option[String] = data.lift(8).filter(_.nonEmpty)
      Runway(id, airportRef, airportIdent, length, width, surface, runwayIdent)
    }
  }
}
