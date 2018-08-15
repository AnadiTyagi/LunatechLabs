/**
  * Created by: anadi
  * Date: 8/14/18
  * Description: 
  **/
object LunatechTaskMain extends App with RenderInput {

  override val lunatechLabs: LunatechLabs = new LunatechLabs

  val input = getValidInput

  if (input == "Q") {
    val queryData = getQueryData
    queryData.map(println)
  }
  if(input == "R") {
    val report = getReport
    val runwayIdentification = getTopRunwayIdentifications.mkString(",")
    report.map(println)
    println(s"Top Runway Identifiers: $runwayIdentification")
  }

}
