import scala.io.StdIn

/**
  * Created by: anadi
  * Date: 8/15/18
  * Description: 
  **/
trait RenderInput {

  val lunatechLabs: LunatechLabs

  val validInputs = Seq("Q", "R", "E")

  def getValidInput: String = {
    val input = StdIn.readLine("Select Q for query and R for Report and E for Exit:")
    if (!validInputs.contains(input.toUpperCase)) { getValidInput }
    else { input }
  }

  def getQueryData: Seq[OutputAirports] = {
    val country = StdIn.readLine("Enter country code or name:")
    val countryDetails = lunatechLabs.getCountry(country)
    if(countryDetails.isEmpty) { getQueryData }
    else { lunatechLabs.getAirportsDetail(countryDetails) }
  }

  def getReport: Seq[Report] = {
    lunatechLabs.getReport
  }

  def getTopRunwayIdentifications: Seq[String] = {
    lunatechLabs.getTopRunwayIdentifications
  }

}
