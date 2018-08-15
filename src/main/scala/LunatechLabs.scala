

class LunatechLabs extends FileUtil {

  lazy val airports = getAirports.groupBy(a => a.country)
  lazy val countries = getCountries
  lazy val runways = getRunways

  def getRunwayTypes(airportId: Int): Seq[String] = {
    val runway = runways.filter(_.airportRef == airportId)
    runway.map(_.surface.toUpperCase).distinct
  }

  def getTopRunwayIdentifications: Seq[String] = {
    val runwayIdent = runways.groupBy(_.leIdent)
      .mapValues(_.size)
      .toSeq
      .filter(_._1.isDefined)
      .sortBy(_._2)
    val topRunwayIdent = runwayIdent.takeRight(10)
    topRunwayIdent.flatMap(_._1)
  }

  def getRunway(airports: Seq[Airport]): Seq[String] = (for{
      airport <- airports
      airportId = airport.id
      runways = getRunwayTypes(airportId)
    }yield {
      runways
    }).flatten.distinct

  def generateReport(selectedCountries: Seq[(String, Int)]): Seq[Report] = {
    selectedCountries.map{v =>
      val countryCode = v._1
      val airportCount = v._2
      val airportsInCountry = airports.get(countryCode).toSeq.flatten
      val airportIds = getRunway(airportsInCountry)
      Report(countryCode, airportCount, airportIds)
    }
  }

  def getReport: Seq[Report] = {
    val airportCount = airports.mapValues(_.size).toSeq.sortBy(_._2)
    val countriesWithMaxAirports = airportCount.takeRight(10)
    val countriesWithMinAirports = airportCount.take(10)
    val selectedCountries = countriesWithMaxAirports ++ countriesWithMinAirports
    generateReport(selectedCountries)
  }

  def getCountry(inputString: String): Option[Country] = {
    val cleanInput = inputString.trim.toUpperCase
    cleanInput.length match {
      case l if l == 2 => countries.find(country => country.code.toUpperCase == cleanInput)
      case l if l > 3 => countries.find(country => country.name.toUpperCase.contains(cleanInput))
      case l if cleanInput == "UAE" => countries.find(country => country.code.toUpperCase == "AE")
      case _ => None
    }
  }

  def getAirportsDetail(country: Option[Country]): Seq[OutputAirports] = {
    val countryInfo = country.getOrElse(return Seq())

    val countryName = countryInfo.name
    val countryCode = countryInfo.code

    val allAirports = airports.get(countryCode).toSeq.flatten

    allAirports.map{airport =>
      val runwaysOnAirport = runways.filter(_.airportRef == airport.id)
      val runwayIdent = runwaysOnAirport.flatMap(_.leIdent)
      val runwayType = runwaysOnAirport.map(_.surface).distinct
      OutputAirports(countryName, airport.name, runwayIdent, runwayType)
    }

  }
}
case class OutputAirports(country: String, airport: String, runways: Seq[String], runwayTypes: Seq[String]){
  override def toString: String = s"Country: $country, Airport: $airport, Runways: ${runways.mkString(",")}, Runway Types: ${runwayTypes.mkString(",")}"
}
