import org.scalatest.{FlatSpec, Matchers}

class LunatechSpec extends FlatSpec with Matchers {

  val lunatechLabs = new LunatechLabs


  "Lunatech" should "return correct country" in {

    val expectedCountryByCode = Some(Country(302755,"US","United States"))
    val expectedCountryByName = Some(Country(302634,"IN","India"))
    val expectedCountryUAE = Some(Country(302618,"AE","United Arab Emirates"))


    val countryByCode = lunatechLabs.getCountry("Us")
    val countryByName = lunatechLabs.getCountry(" india ")
    val countryUAE = lunatechLabs.getCountry("uae")
    val countryRandom = lunatechLabs.getCountry("Randon Input")

    countryByCode should be(expectedCountryByCode)
    countryByName should be(expectedCountryByName)
    countryUAE should be(expectedCountryUAE)
    countryRandom should be(None)
  }
  it should "get airport detail by country correctly" in {
    val expectedOutput = Seq(OutputAirports("Moldova","Balti International Airport",Seq("15"),Seq("CON")),
    OutputAirports("Moldova","Mărculeşti International Airport",Seq("07"),Seq("CON")),
    OutputAirports("Moldova","Cahul International Airport",Seq(),Seq()),
    OutputAirports("Moldova","Chadyr Lunga Airport",Seq("2"),Seq("grass")),
    OutputAirports("Moldova","Chişinău International Airport",Seq("08"),Seq("CON")),
    OutputAirports("Moldova","Tiraspol Airport",Seq(),Seq()),
    OutputAirports("Moldova","Bălţi City Airport",Seq(),Seq()),
    OutputAirports("Moldova","Vadul lui Voda Airfield",Seq(),Seq()))

    val country = Some(Country(302704,"MD","Moldova"))
    val airports = lunatechLabs.getAirportsDetail(country)

    airports should be(expectedOutput)
  }

  it should "generate report correctly" in {
    val expectedReport = Seq(Report("MD",8,List("CON", "GRASS")), Report("CK",9,List("COR", "ASP", "UNK", "CON")))
    val countries = Seq(("MD", 8), ("CK", 9))
    val reports = lunatechLabs.generateReport(countries)
    reports should be(expectedReport)
  }

  it should "get runways correctly" in {
    val expectedRunways = Seq("ASPH-G")
    val airport = Seq(Airport(6523, "00A", "heliport", "Total Rf Heliport", "US"))
    val runways = lunatechLabs.getRunway(airport)
    runways should be(expectedRunways)
  }

  it should "extract top runway identifiers correctly" in {
    val expectedTopRunwayIdent = Seq("15", "13", "08", "14", "12", "16", "17", "09", "18", "H1")
    val topRunwayIdent = lunatechLabs.getTopRunwayIdentifications
    topRunwayIdent should be(expectedTopRunwayIdent)
  }
}