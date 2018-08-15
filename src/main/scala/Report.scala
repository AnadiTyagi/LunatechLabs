case class Report (country: String, count:Int, runwayType: Seq[String]) {
  override def toString: String = s"Country: $country, Number of Airports: $count, Types of Runways: ${runwayType.mkString(",")}"
}
