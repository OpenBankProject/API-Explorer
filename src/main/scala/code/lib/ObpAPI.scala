package code.lib

import java.io._
import java.net.{HttpURLConnection, URL}
import java.text.SimpleDateFormat
import java.util.Date

import code.lib.ObpJson._
import code.util.Helper
import code.util.Helper.MdcLoggable
import code.util.cache.Caching
import net.liftweb.common.{Box, Failure, Full, _}
import net.liftweb.http.{RequestVar, S, SessionVar}
import net.liftweb.json.JsonAST.{JBool, JValue}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.util.Helpers.{intToTimeSpanBuilder => _, _}

import scala.xml.NodeSeq
import com.tesobe.CacheKeyFromArguments
import scala.collection.immutable.{List, Nil}
import scala.concurrent.duration._
import scala.language.postfixOps
import java.util.UUID.randomUUID
import net.liftweb.common._


case class Header(key: String, value: String)




object ObpAPI extends Loggable {

  val obpPrefix : String = "/obp"


  implicit val formats = DefaultFormats
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  
  val defaultProvider = Helper.getPropsValue("defaultAuthProvider").getOrElse("")
  
  val userNotFoundError = "user (\\S+) at provider (\\S+) not found".r
  
  /**
   * The request vars ensure that for one page load, the same API call isn't
   * made multiple times
   */
  object allBanksVar extends RequestVar[Box[BanksJson]] (Empty)

  def allBanks : Box[BanksJson]= {
    allBanksVar.get match {
      case Full(a) => Full(a)
      case _ => allBanksVar.set(ObpGet(s"$obpPrefix/v3.1.0/banks").flatMap(_.extractOpt[BanksJson]))
        allBanksVar.get
    }
  }

  def currentUser : Box[CurrentUserJson]= if(isLoggedIn){
    ObpGet(s"$obpPrefix/v2.0.0/users/current").flatMap(_.extractOpt[CurrentUserJson])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")

  // Wrapper for looking at OAuth headers.
  def isLoggedIn : Boolean = {
    OAuthClient.loggedIn
  }


  
  trait SortDirection {
    val value : String
  }
  object ASC extends SortDirection { val value = "ASC" }
  object DESC extends SortDirection { val value = "DESC" }



  /**
    * @return Json for transactions of a particular bank account Uses 3.0.0 call and format.
    */
  def transactions300(bankId: String, accountId: String, viewId: String, limit: Option[Int],
                      offset: Option[Int], fromDate: Option[Date], toDate: Option[Date], sortDirection: Option[SortDirection]) : Box[TransactionsJsonV300]= if(isLoggedIn){

    val headers : List[Header] = limit.map(l => Header("obp_limit", l.toString)).toList ::: offset.map(o => Header("obp_offset", o.toString)).toList :::
      fromDate.map(f => Header("obp_from_date", dateFormat.format(f))).toList ::: toDate.map(t => Header("obp_to_date", dateFormat.format(t))).toList :::
      sortDirection.map(s => Header("obp_sort_direction", s.value)).toList ::: Nil

    ObpGet(s"$obpPrefix/v3.0.0/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) +
      "/transactions", headers).flatMap(x => x.extractOpt[TransactionsJsonV300])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")







  def publicAccounts(bankId : String) : Box[BarebonesAccountsJson] = {
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + "/accounts/public").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def publicAccounts : Box[BarebonesAccountsJson] = {
    ObpGet(s"$obpPrefix/v3.1.0/accounts/public").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def privateAccounts(bankId : String) : Box[BarebonesAccountsJson] = if(isLoggedIn){
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + "/accounts/private").flatMap(_.extractOpt[BarebonesAccountsJson])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")

  def privateAccounts : Box[BarebonesAccountsJson] = if(isLoggedIn){
    ObpGet(s"$obpPrefix/v1.2.1/accounts/private").flatMap(_.extractOpt[BarebonesAccountsJson])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")
  
  @deprecated("This method will mix public and private, not clear for Apps.","2018-02-18")
  def allAccountsAtOneBank(bankId : String) : Box[BarebonesAccountsJson] = if(isLoggedIn){
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + "/accounts").flatMap(_.extractOpt[BarebonesAccountsJson])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")

  // Similar to getViews below
  def getViewsForBankAccount(bankId: String, accountId: String) = if(isLoggedIn){
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + bankId + "/accounts/" + accountId + "/views").flatMap(_.extractOpt[ViewsJson])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")

  def getAccount(bankId: String, accountId: String, viewId: String) : Box[AccountJson] = if(isLoggedIn) {
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/account").flatMap(x => x.extractOpt[AccountJson])
  } else Failure("OBP-20001: User not logged in. Authentication is required!")

  def getCounterparties(bankId: String, accountId: String, viewId: String): Box[DirectOtherAccountsJson] = if(isLoggedIn) {
    val counterparties  = ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/other_accounts").flatMap(x => x.extractOpt[DirectOtherAccountsJson])
    counterparties
  } else Failure("OBP-20001: User not logged in. Authentication is required!")

  def getExplictCounterparties(bankId: String, accountId: String, viewId: String): Box[ExplictCounterpartiesJson] = if(isLoggedIn){
     ObpGet(s"$obpPrefix/v2.2.0/banks/" + urlEncode(bankId) + "/accounts/" + urlEncode(accountId) + "/" + urlEncode(viewId) + "/counterparties").flatMap(x => x.extractOpt[ExplictCounterpartiesJson])
  }else Failure("OBP-20001: User not logged in. Authentication is required!")

  def getEntitlementsV300 : Box[EntitlementsJson] = if(isLoggedIn){
    ObpGet(s"$obpPrefix/v3.0.0/my/entitlements").flatMap(_.extractOpt[EntitlementsJson]) 
  } else Failure("OBP-20001: User not logged in. Authentication is required!")


  def getEntitlementRequestsV300 : Box[EntitlementRequestsJson] = if(isLoggedIn){
    val result = ObpGet(s"$obpPrefix/v3.0.0/my/entitlement-requests").flatMap(_.extractOpt[EntitlementRequestsJson])
    logger.debug(s"We got this result for EntitlementRequestsJson: ${result}")
    result
  } else Failure("OBP-20001: User not logged in. Authentication is required!")



  /**
   * The request vars ensure that for one page load, the same API call isn't made multiple times
   */
  object allResoucesVar extends SessionVar[Box[ResourceDocsJson]] (Empty)
  
  
  // Returns Json containing Resource Docs
  def getResourceDocsJson(apiVersion : String) : List[ResourceDocJson] = {

    //Note: ?content=true&content=false
    // if there are two content parameters there, only the first one is valid for the api call. 
    // so requestParams have the high priority 
    val requestParams = List("tags", "language", "functions", "content")
        .map(paramName => (paramName, S.param(paramName)))
        .collect{
          case (paramName, Full(paramValue)) if(paramValue.trim.size > 0) => s"$paramName=$paramValue"
        }
        .mkString("?", "&", "")

    val staticResourcesDocs= getStaticResourceDocs(apiVersion,requestParams) 
    
    if(requestParams.contains("content=static")) {
      staticResourcesDocs
    } else if (requestParams.contains("content=dynamic")){
      getDynamicResourceDocs(apiVersion,requestParams)
    } else{
      val dynamicResourcesDocs= getDynamicResourceDocs(apiVersion,requestParams)
      staticResourcesDocs ++ dynamicResourcesDocs 
    }
  }
  
  //  static resourceDocs can be cached for a long time, only be changed when new deployment.
  val getStaticResourceDocsJsonTTL: FiniteDuration = 365 days
  def getStaticResourceDocs(apiVersion : String, requestParams: String): List[ResourceDocJson] =  {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(getStaticResourceDocsJsonTTL) {
        ObpGet(s"$obpPrefix/v3.1.0/resource-docs/$apiVersion/obp$requestParams&content=static").map(extractResourceDocsJson).map(_.resource_docs).head
      }
    }
  }
  
  def getDynamicResourceDocs(apiVersion : String, requestParams: String) =  
    ObpGet(s"$obpPrefix/v3.1.0/resource-docs/$apiVersion/obp$requestParams&content=dynamic").map(extractResourceDocsJson).map(_.resource_docs).head

  /**
   * extract ResourceDocsJson and output details of error if extract json to case class fail
   * @param jValue
   * @return extracted ResourceDocsJson from JValue
   */
  private def extractResourceDocsJson(jValue: JValue): ResourceDocsJson = {
    val arr = (jValue \ "resource_docs").asInstanceOf[JArray].arr
    val resourceDocJsons = arr.map { it =>
      try {
        it.extract[ResourceDocJson]
      } catch {
        case e: Throwable =>
          import net.liftweb.json
          logger.error(s"parse json to ResourceDocJson fail, json: ${json.compactRender(it)}", e)
          throw e
      }
    }
    ResourceDocsJson(resourceDocJsons)
  }


  //  this is one month
  val getGlossaryItemsJsonTTL: FiniteDuration = 30 days
  
  def getGlossaryItemsJson : Box[GlossaryItemsJsonV300] = {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(getGlossaryItemsJsonTTL) {
        ObpGet(s"$obpPrefix/v3.0.0/api/glossary").flatMap(_.extractOpt[GlossaryItemsJsonV300])
      }
    }
  }

  //  this is one month
  val getMessageDocsJsonTTL: FiniteDuration = 30 days
  
  def getMessageDocsJson(connector: String) : Box[MessageDocsJsonV220] = {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(getMessageDocsJsonTTL) {
  
        ObpGet(s"$obpPrefix/v2.2.0/message-docs/$connector").flatMap(_.extractOpt[MessageDocsJsonV220])
      }
    }
  }

}

case class ObpError(error :String)

object OBPRequest extends MdcLoggable {
  implicit val formats = DefaultFormats
  //returns a tuple of the status code,  response body and list of headers
  def apply(apiPath : String, jsonBody : Option[JValue], method : String, headers : List[Header]) : Box[(Int, String, List[String])] = {
    logger.debug(s"before $apiPath call:")
    val statusAndBody = tryo {
      val credentials = OAuthClient.getAuthorizedCredential
      val apiUrl = OAuthClient.currentApiBaseUrl

      val convertedApiPath = apiPath
        .replaceAll("UKv2.0", "v2.0")
        .replaceAll("UKv3.1", "v3.1")
        .replaceAll("BGv1.3", "v1.3")
        .replaceAll("BGv1", "v1")
        .replaceAll("OBPv", "v")

      val url = apiUrl + convertedApiPath

      logger.info(s"OBP Server Request URL: ${apiUrl}${convertedApiPath}")

      //bleh
      val request = SSLHelper.getConnection(url) //blagh!
      request.setDoOutput(true)
      request.setRequestMethod(method)
      request.setRequestProperty("Content-Type", "application/json; charset=UTF-8")
      request.setRequestProperty("Accept", "application/json")
      request.setRequestProperty("Accept-Charset", "UTF-8")

      //sign the request if we have some credentials to sign it with
      credentials.foreach(c => c.consumer.sign(request))

      headers.foreach(header => request.setRequestProperty(header.key, header.value))

      //Set the request body
      if(jsonBody.isDefined) {
        val output = request.getOutputStream()
        val writer = new BufferedWriter(new OutputStreamWriter(output, "UTF-8"))
        writer.write(compact(render(jsonBody.get)).toString)
        writer.flush()
        writer.close()
      }

      request.connect()
      val status = request.getResponseCode()
      import scala.collection.JavaConverters._
      val responseHeaders: List[(String, Set[String])] = request.getHeaderFields().asScala.mapValues(_.asScala.toSet).toList
      val adjustedResponseHeaders = responseHeaders.map(x => x._1 + ": " + x._2.mkString(", ")).sortWith(_ < _).filter(_.startsWith("null") == false)

      //get reponse body
      val inputStream = if(status >= 400) request.getErrorStream() else request.getInputStream()
      val reader = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"))
      val builder = new StringBuilder()
      var line = ""
      def readLines() {
        line = reader.readLine()
        if (line != null) {
          builder.append(line + "\n")
          readLines()
        }
      }
      readLines()
      reader.close();
      (status, builder.toString(), adjustedResponseHeaders)
    }

    val result = statusAndBody pass {
      case Failure(msg, ex, _) => {
        val sw = new StringWriter()
        val writer = new PrintWriter(sw)
        ex.foreach(_.printStackTrace(writer))
        logger.debug("Error making api call: " + msg + ", stack trace: " + sw.toString)
      }
      case _ => Unit
    }
    logger.debug(s"after $apiPath call:")
    result
  }
}

object ObpPut {
  def apply(apiPath: String, json : JValue): Box[JValue] = {
    OBPRequest(apiPath, Some(json), "PUT", Nil).flatMap {
      case(status, result, _) => APIUtils.getAPIResponseBody(status, result)
    }
  }
}
object ObpPutWithHeader {
  def apply(apiPath: String, json : JValue, headers : List[Header] = Nil): (Box[JValue], List[String]) = {
    OBPRequest(apiPath, Some(json), "PUT", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3)
    }
  }
}

object ObpPost {
  def apply(apiPath: String, json : JValue): Box[JValue] = {
    OBPRequest(apiPath, Some(json), "POST", Nil).flatMap {
      case(status, result, _) => APIUtils.getAPIResponseBody(status, result)
    }
  }
}
object ObpPostWithHeader {
  def apply(apiPath: String, json : JValue, headers : List[Header] = Nil): (Box[JValue], List[String]) = {
    val requestBody = json match {
      case JNothing | JNull => None
      case v => Option(v)
    }
    OBPRequest(apiPath, requestBody, "POST", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3)
    }
  }
}

object ObpDeleteBoolean {
  /**
   * @return True if the delete worked
   */
  def apply(apiPath: String): Boolean = {
    val worked = OBPRequest(apiPath, None, "DELETE", Nil).map {
      case(status, result, _) => APIUtils.apiResponseWorked(status, result)
    }
    worked.getOrElse(false)
  }
}



// In case we want a more raw result
// TODO
object ObpDelete {
  def apply(apiPath: String): Box[JValue] = {
    OBPRequest(apiPath, None, "DELETE", Nil).map {
      case(status, result, _) => APIUtils.apiResponseWorked(status, result)
    }
  }
}
object ObpDeleteWithHeader {
  def apply(apiPath: String, headers : List[Header] = Nil): (Box[JValue], List[String]) = {
    OBPRequest(apiPath, None, "DELETE", headers) match {
      case Full(value) => (APIUtils.deleteApiResponse(value._1, value._2), value._3)
    }
  }
}



object ObpGet {
  def apply(apiPath: String, headers : List[Header] = Nil): Box[JValue] = {
    // the bankId is blank
    if(apiPath.contains("/banks//")) {
      Empty
    } else {
      OBPRequest(apiPath, None, "GET", headers).flatMap {
        case(status, result, _) => APIUtils.getAPIResponseBody(status, result)
      }
    }
  }
}
object ObpGetWithHeader {
  def apply(apiPath: String, headers : List[Header] = Nil): (Box[JValue], List[String]) = {
    OBPRequest(apiPath, None, "GET", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3)
    }
  }
}

object APIUtils extends MdcLoggable {
  implicit val formats = DefaultFormats

  def getAPIResponseBody(responseCode : Int, body : String) : Box[JValue] = {
    responseCode match {
      case 200 | 201 | 202 |204 => 
        logger.debug("Before getAPIResponseBody(String ->JValue): ")
        val jvalue = tryo{parse(body)}
        logger.debug("After getAPIResponseBody(String -> JValue): ")
        jvalue
      case _ => {
        val failMsg = "Bad response code (" + responseCode + ") from OBP API server: " + body
        logger.warn(failMsg)
        Failure(Helper.renderJson(parse(body)))
      }
    }
  }
  def deleteApiResponse(responseCode : Int, result : String) : Box[JValue] = {
    responseCode match {
      case 200 | 201 | 202 |204 =>
        Full(JBool(true))
      case _ => val failMsg = "Bad response code (" + responseCode + ") from OBP API server: " + result
        logger.warn(failMsg)
        Failure(Helper.renderJson(parse(result)))
    }
  }
  def apiResponseWorked(responseCode : Int, result : String) : Boolean = {
    responseCode match {
      case 200 | 201 | 202 |204 => true
      case _ => false
    }
  }
}

object ObpJson {
  import net.liftweb.json._
  implicit val formats = DefaultFormats
  case class BanksJson(banks : Option[List[BankJson]]) {
    def bankJsons: List[BankJson] = {
      banks.toList.flatten
    }
  }
  case class BankJson(id: Option[String], 
    short_name: Option[String],
    full_name: Option[String],
    logo: Option[String],
    website: Option[String])

  case class CurrentUserJson(user_id: String,
                             email: String,
                             provider_id: String,
                             provider: String,
                             username: String
                            )
		  		  
  case class UserJson(id: Option[String],
    provider: Option[String],
    display_name: Option[String])

  case class AccountBalanceJson(currency: Option[String],
    amount: Option[String])		  	
	
    //simplified version of what we actually get back from the api
  case class ViewJson(
    id: Option[String],
    short_name: Option[String],
    description: Option[String],
    is_public: Option[Boolean])
            
  case class ViewsJson(views: Option[List[ViewJson]])

  case class CompleteViewJson(json: Map[String, Any]){
    val id: Option[String] = json.get("id") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val shortName: Option[String] = json.get("short_name") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val alias: Option[String] = json.get("alias") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val description: Option[String] = json.get("description") match {
      case Some(s : String) => Some(s)
      case _ => None
    }

    val isPublic: Option[Boolean] = json.get("is_public") match {
      case Some(b : Boolean) => Some(b)
      case _ => None
    }

    val booleans = json.collect{ case (s: String, b: Boolean) => (s,b)}

    val permissions = booleans.filterNot(_.key == "is_public")
  }
		  		  
  case class AccountJson(id: Option[String],
    label: Option[String],
    number: Option[String],
    owners: Option[List[UserJson]],
    `type`: Option[String],
    balance: Option[AccountBalanceJson],
    IBAN : Option[String],
    views_available: Option[List[ViewJson]])
		  			 
  case class BarebonesAccountsJson(accounts: Option[List[BarebonesAccountJson]])
  
  case class BarebonesAccountJson(id: Option[String],
    label: Option[String],
    views_available: Option[List[ViewJson]],
    bank_id: Option[String])
		  						  
  case class HolderJson(name: Option[String],
		is_alias : Option[Boolean])
		  				
  //TODO: Can this go with BankJson?
  case class LightBankJson(national_identifier: Option[String],
    name: Option[String])
  
  case class ThisAccountJson(holders: Option[List[HolderJson]],
    number: Option[String],
    kind: Option[String],
    IBAN: Option[String],
    bank: Option[LightBankJson])
  
  case class LocationJson(latitude: Option[Double],
    longitude: Option[Double],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    user: Option[UserJson])

  case class OtherAccountMetadataJson(public_alias: Option[String],
    private_alias: Option[String],
    more_info: Option[String],
    URL: Option[String],
    image_URL: Option[String],
    open_corporates_URL: Option[String],
    corporate_location: Option[LocationJson],
    physical_location: Option[LocationJson])		  					 

  //TODO: Why can't an other account have more than one holder?	  					 
  case class OtherAccountJson(id: Option[String],
    holder: Option[HolderJson],
    number: Option[String],
    kind: Option[String],
    IBAN: Option[String],
    bank: Option[LightBankJson],
    metadata: Option[OtherAccountMetadataJson])

  case class OtherAccountsJson(other_accounts: Option[List[OtherAccountJson]])

  //////////////////////////////////////
  // Subtle differences to the OtherAccount json above.
  // This what the 1.2.1 other_accounts call returns
  // These case classes copied from API JSONFactory1.2.1

  case class OtherAccountJson121(
                               id : String,
                               holder : AccountHolderJson121,
                               number : String,
                               kind : String,
                               IBAN : String,
                               swift_bic: String,
                               bank : DirectMinimalBankJSON,
                               metadata : DirectOtherAccountMetadataJSON
                               )

  case class DirectOtherAccountsJson(
                                other_accounts : List[OtherAccountJson121]
                                )

  case class ExplictCounterparty(
                                  name: String,
                                  description: String,
                                  created_by_user_id: String,
                                  this_bank_id: String,
                                  this_account_id: String,
                                  this_view_id: String,
                                  counterparty_id: String,
                                  other_bank_routing_scheme: String,
                                  other_bank_routing_address: String,
                                  other_branch_routing_scheme: String,
                                  other_branch_routing_address: String,
                                  other_account_routing_scheme: String,
                                  other_account_routing_address: String,
                                  other_account_secondary_routing_scheme: String,
                                  other_account_secondary_routing_address: String,
                                  is_beneficiary: Boolean
                                )
  
  case class ExplictCounterpartiesJson(
                                      counterparties : List[ExplictCounterparty]
                                    )

  case class AccountHolderJson121(
    name : String,
    is_alias : Boolean
    )


  case class DirectMinimalBankJSON(
  national_identifier : String,
  name : String
  )

  case class DirectOtherAccountMetadataJSON(
   public_alias : String,
   private_alias : String,
   more_info : String,
   URL : String,
   image_URL : String,
   open_corporates_URL : String,
   corporate_location : DirectLocationJSON,
   physical_location : DirectLocationJSON
   )


  case class DirectLocationJSON(
  latitude : Double,
  longitude : Double,
  date : Date,
  user : DirectUserJSON
  )


  case class DirectUserJSON(
   id : String,
   provider : String,
   display_name : String
   )


  ///////////

  case class TransactionValueJson(currency: Option[String],
    amount: Option[String])
		  					  
  case class TransactionDetailsJson(`type`: Option[String],
    description: Option[String],
    posted: Option[Date], //TODO: Check if the default date formatter is okay
    completed: Option[Date], //TODO: Check if the default date formatter is okay
    new_balance: Option[AccountBalanceJson],
    value: Option[TransactionValueJson])	  					  
		  					  
  case class TransactionCommentJson(id: Option[String],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    value: Option[String],
    user: Option[UserJson],
    reply_to: Option[String])

  case class ResourceDocCommentJsonV300(id: Option[String],
                                        text: Option[String],
                                        user: Option[MinimalUserJsonV300],
                                    date: Option[Date],
                                    reply_to_id: Option[String])


  case class TransactionTagJson(id: Option[String],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    value: Option[String],
    user: Option[UserJson])
  
  case class TransactionImageJson(id: Option[String],
    label: Option[String],
    date: Option[Date], //TODO: Check if the default date formatter is okay
    URL: Option[String],
    user: Option[UserJson])
  
  case class TransactionMetadataJson(narrative: Option[String],
    comments: Option[List[TransactionCommentJson]],
    tags: Option[List[TransactionTagJson]],
    images: Option[List[TransactionImageJson]],
    where: Option[LocationJson])
  
  case class TransactionJson(uuid: Option[String],
    id: Option[String],
    this_account: Option[ThisAccountJson],
    other_account: Option[OtherAccountJson],
    details: Option[TransactionDetailsJson],
    metadata: Option[TransactionMetadataJson]) {
    
    lazy val imageJsons : Option[List[TransactionImageJson]] = {
      metadata.flatMap(_.images)
    }
    
    lazy val tagJsons : Option[List[TransactionTagJson]] = {
      metadata.flatMap(_.tags)
    }
    
    lazy val commentJsons : Option[List[TransactionCommentJson]] = {
      metadata.flatMap(_.comments)
    }
  }
  
  case class TransactionsJson(transactions: Option[List[TransactionJson]])
  
  case class PermissionJson(user: Option[UserJson], views: Option[List[ViewJson]])
  
  case class PermissionsJson(permissions : Option[List[PermissionJson]])


  // Copied directly from 1.2.1 API
  case class TransactionsJson121(
   transactions: List[TransactionJson121]
   )

  case class TransactionJson121(
      id : String,
      this_account : ThisAccountJson121,
      other_account : OtherAccountJson121,
      details : TransactionDetailsJson121,
      metadata : TransactionMetadataJson121
      )

  case class ThisAccountJson121(
    id : String,
    holders : List[AccountHolderJson121],
    number : String,
    kind : String,
    IBAN : String,
    swift_bic: String,
    bank : MinimalBankJson121
    )


  case class MinimalBankJson121(
    national_identifier : String,
    name : String
    )


  case class TransactionDetailsJson121(
   `type` : String,
   description : String,
   posted : Date,
   completed : Date,
   new_balance : AmountOfMoneyJson121,
   value : AmountOfMoneyJson121
 )

  case class TransactionMetadataJson121(
  narrative : String,
  comments : List[TransactionCommentJson121],
  tags :  List[TransactionTagJson121],
  images :  List[TransactionImageJson121],
  where : LocationJson121
  )

  case class LocationJson121(
                           latitude : Double,
                           longitude : Double,
                           date : Date,
                           user : UserJson121
                           )

  case class AmountOfMoneyJson121(
    currency : String,
    amount : String
    )


  case class TransactionCommentJson121(
     id : String,
     value : String,
     date: Date,
     user : UserJson121
     )



  case class TransactionTagJson121(
   id : String,
   value : String,
   date : Date,
   user : UserJson121
   )


  case class TransactionImageJson121(
   id : String,
   label : String,
   URL : String,
   date : Date,
   user : UserJson121
   )

  case class UserJson121(
   id : String,
   provider : String,
   display_name : String
   )


  case class MinimalUserJsonV300(
                          user_id : String,
                          username : String,
                          provider : String
                        )





  case class EntitlementJson (
  entitlement_id :String,
  role_name: String,
  bank_id: String)


  case class EntitlementsJson (list : List[EntitlementJson])

  case class UserJsonV200(
                           user_id: String,
                           email : String,
                           provider_id: String,
                           provider : String,
                           username : String,
                           entitlements : EntitlementsJson
                         )

  case class EntitlementRequestJson(entitlement_request_id: String, user: UserJsonV200, role_name: String, bank_id: String, created: String)
  case class EntitlementRequestsJson(entitlement_requests: List[EntitlementRequestJson])








  case class Entitlement (
                               entitlementId :String,
                               roleName: String,
                               bankId: String)


  case class EntitlementRequest (
                                  entitlementRequestId :String,
                                  user: UserJsonV200,
                                  roleName: String,
                                  bankId: String,
                                  created: String)


// Extract the roles in Resource Doc to this:
  case class RoleJson (
                    role: String,
                    requires_bank_id: Boolean
                  )



  // Used to describe the OBP API calls for documentation and API discovery purposes
  case class ResourceDocJson(operation_id: String,
                             request_verb: String,
                             request_url: String,
                             summary: String, // Summary of call should be 120 characters max
                             description: String,      // Description of call in html format
                             example_request_body: JValue,  // An example request body
                             success_response_body: JValue, // Success response body
                             error_response_bodies: List[String],
                             implemented_by: ImplementedByJson,
                             tags : List[String],
                             roles: List[RoleJson],
                             is_featured: Boolean,
                             special_instructions: String,
                             specified_url: String, // This is the URL that we want people to call.
                             connector_methods: List[String]
                            )

  case class ResourceDocsJson (resource_docs : List[ResourceDocJson])
  ///////////////////////////////////////////


  // Internal representation of the ResourceDoc (may differ from the OBP API representation (for instance OBP representation does not have id)


  // Used to describe where an API call is implemented (format from API)
  case class ImplementedByJson (
   version : String, // Short hand for the version e.g. "1_4_0" means Implementations1_4_0
   function : String // The val / partial function that implements the call e.g. "getBranches
  )

  // Internal format (currently the same)
  case class ImplementedBy (
   version : String, // Short hand for the version e.g. "1_4_0" means Implementations1_4_0
   function : String // The val / partial function that implements the call e.g. "getBranche
  )



  case class Role (
                                           role: String,
                                           requiresBankId: Boolean
                                         )
// Role and indication if the current user has it.
  case class RoleInfo(
                       role: String,
                       requiresBankId: Boolean,
                       userHasEntitlement: Boolean,
                       userHasEntitlementRequest: Boolean
                  )



  case class ResourceDocPlus(id: String,
                             verb: String,
                             url: String,
                             summary: String,
                             description: NodeSeq,
                             exampleRequestBody: JValue,
                             successResponseBody: JValue,
                             errorResponseBodies: List[String],
                             implementedBy: ImplementedBy,
                             tags: List[String],
                             roleInfos: List[RoleInfo],
                             isFeatured: Boolean,
                             specialInstructions: NodeSeq,
                             connectorMethods: List[String]
  )


  case class ResourceDocs (resourceDocs : List[ResourceDocPlus])
  
}

case class GlossaryDescriptionJsonV300 (markdown: String, html: String)

case class GlossaryItemJsonV300 (title: String,
                                 description : GlossaryDescriptionJsonV300
                                )

case class GlossaryItemsJsonV300 (glossary_items: List[GlossaryItemJsonV300])

case class EndpointInfo(name: String, version: String)

case class MessageDocJsonV220(
                           process: String, // Should be unique
                           message_format: String,
                           outbound_topic: Option[String] = None,
                           inbound_topic: Option[String] = None,
                           description: String,
                           example_outbound_message: JValue,
                           example_inbound_message: JValue,
                           outboundAvroSchema: Option[JValue] = None,
                           inboundAvroSchema: Option[JValue] = None,
                           adapter_implementation : AdapterImplementationJson220,
                           dependent_endpoints: List[EndpointInfo],
                           requiredFieldInfo: Option[JValue] = None
                         )

case class MessageDocsJsonV220 (message_docs: List[MessageDocJsonV220])

case class AdapterImplementationJson220(
                                         group: String = "MISC",
                                         suggested_order : Integer = 100
                                    )



case class ThisAccountJsonV300(
                                id: String,
                                bank_routing: BankRoutingJsonV121,
                                account_routings: List[AccountRoutingJsonV121],
                                holders: List[AccountHolderJSON]
                              )
case class OtherAccountJsonV300(
                                 id: String,
                                 holder: AccountHolderJSON,
                                 bank_routing: BankRoutingJsonV121,
                                 account_routings: List[AccountRoutingJsonV121],
                                 metadata: OtherAccountMetadataJSON
                               )
case class OtherAccountsJsonV300(
                                  other_accounts: List[OtherAccountJsonV300]
                                )
case class TransactionJsonV300(
                                id: String,
                                this_account: ThisAccountJsonV300,
                                other_account: OtherAccountJsonV300,
                                details: TransactionDetailsJSON,
                                metadata: TransactionMetadataJSON
                              )
case class TransactionsJsonV300(
                                 transactions: List[TransactionJsonV300]
                               )
case class BankRoutingJsonV121(
                                scheme: String,
                                address: String
                              )
case class AccountRoutingJsonV121(
                                   scheme: String,
                                   address: String
                                 )
case class AccountHolderJSON(
                              name : String,
                              is_alias : Boolean
                            )
case class OtherAccountMetadataJSON(
                                     public_alias : String,
                                     private_alias : String,
                                     more_info : String,
                                     URL : String,
                                     image_URL : String,
                                     open_corporates_URL : String,
                                     corporate_location : LocationJSONV121,
                                     physical_location : LocationJSONV121
                                   )
case class LocationJSONV121(
                             latitude : Double,
                             longitude : Double,
                             date : Date,
                             user : UserJSONV121
                           )
case class TransactionDetailsJSON(
                                   `type` : String,
                                   description : String,
                                   posted : Date,
                                   completed : Date,
                                   new_balance : AmountOfMoneyJsonV121,
                                   value : AmountOfMoneyJsonV121
                                 )
case class TransactionMetadataJSON(
                                    narrative : String,
                                    comments : List[TransactionCommentJSON],
                                    tags :  List[TransactionTagJSON],
                                    images :  List[TransactionImageJSON],
                                    where : LocationJSONV121
                                  )
case class UserJSONV121(
                         id : String,
                         provider : String,
                         display_name : String
                       )
case class AmountOfMoneyJsonV121(
                                  currency : String,
                                  amount : String
                                )
case class TransactionCommentJSON(
                                   id : String,
                                   value : String,
                                   date: Date,
                                   user : UserJSONV121
                                 )
case class TransactionTagJSON(
                               id : String,
                               value : String,
                               date : Date,
                               user : UserJSONV121
                             )
case class TransactionImageJSON(
                                 id : String,
                                 label : String,
                                 URL : String,
                                 date : Date,
                                 user : UserJSONV121
                               )