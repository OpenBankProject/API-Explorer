package code.lib

import java.io._
import java.net.HttpURLConnection
import java.security.Security
import java.text.SimpleDateFormat
import java.util.UUID.randomUUID
import java.util.{Date, UUID}

import code.lib.ObpAPI.UnknownErrorMessage
import code.lib.ObpJson._
import code.util.{Helper, JwsUtil}
import code.util.Helper.{MdcLoggable, covertWebpageIdToObpOperationId}
import code.util.cache.Caching
import com.nimbusds.jose.crypto.bc.BouncyCastleProviderSingleton
import com.nimbusds.jose.crypto.{ECDSASigner, RSASSASigner}
import com.nimbusds.jose.jwk.{ECKey, JWK, RSAKey}
import com.nimbusds.jose.{JOSEException, JWSAlgorithm, JWSHeader, JWSSigner}
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import com.tesobe.CacheKeyFromArguments
import net.liftweb.common.{Box, Failure, Full, _}
import net.liftweb.http.{RequestVar, S, SessionVar}
import net.liftweb.json
import net.liftweb.json.JsonAST.{JBool, JValue}
import net.liftweb.json._
import net.liftweb.util.Helpers.{intToTimeSpanBuilder => _, _}
import okhttp3.MediaType

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Nil}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.xml.NodeSeq


case class Header(key: String, value: String)




object ObpAPI extends Loggable {

  val obpPrefix : String = "/obp"


  implicit val formats = DefaultFormats
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  
  val userNotFoundError = "user (\\S+) at provider (\\S+) not found".r

  final val AccountUrlPath = "/accounts/"
  final val ApiCollectionId = "api-collection-id"
  final val CacheModifier = "cache-modifier"
  final val ContentEqualStatic = "content=static"
  final val ContentEqualDynamic = "content=dynamic"
  final val UnknownErrorMessage = "Unknown Error!"
  final val OBPVersionV400 = "OBPv4.0.0"
  final val OBPVersionV500 = "OBPv5.0.0"
  final val OBPVersionV510 = "OBPv5.1.0"
  final val UKVersionV31 = "UKv3.1"
  final val UKVersionV20 = "UKv2.0"
  final val BGVersionV13 = "BGv1.3"
  final val BGVersionV1 = "BGv1"
  final val BGVersionV133 = "BGv1.3.3"
  final val BahrainOBFVersionV100 = "BAHRAIN-OBFv1.0.0"
  final val PAPIVersionV2111 = "PAPIv2.1.1.1"
  final val AUv100 = "AUv1.0.0"
  final val STETv14 = "STETv1.4"
  final val CNBV9v100 = "CNBV9v1.0.0"
  final val VersionV133 = "v1.3.3"
  final val DisplayEqualNone = "display: none"
  final val ResourceStyleCss = ".resource [style]"
  final val ResourceErrorStyleCss = ".resource-error [style]"
  final val DisplayEqualBlock = "display: block"
  final val ContentBoxHeadline = ".content-box__headline *"
  final val RolesBoxId = "@roles_box [id]"
  final val RolesBoxStyle = "@roles_box [style]"
  final val RoleItemClassCss = ".role_item"
  final val RolesRoleNameCss = "@roles__role_name"
  final val RoleStatusNameCss = "@roles__status"
  final val RolesBankIdInput = "@roles__bank_id_input"
  final val RolesRoleInput = "@roles__role_input"
  final val RolesEntitlementRequestId = "@roles__entitlement_request_response [id]"
  final val RolesRequestEntitlementButton = "@roles__request_entitlement_button"
  final val PleaseLoginToRequestThisRole = " - Please login to request this Role"
  final val YouHaveThisRole = s" - You have this Role."
  final val ContactOBPTeam = s" - You have requested this Role. Please contact Open Bank Project team to grant your this role."
  final val YouCanRequestThisRole = s" - You can request this Role."
  final val RolesEntitlementRequestButtonBox = "@roles__entitlement_request_button_box [style]"
  final val EndPointAnchorHref = ".end-point-anchor [href]"
  final val ContentBoxHeadlineId = ".content-box__headline [id]"

  final val showDynamicResourceDocs = Helper.getPropsValue("show_dynamic_resource_docs","true").toBoolean
  final val v510 = "v5.1.0"

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

  def currentUser : Box[CurrentUserJson]= 
    if(isLoggedIn) 
      ObpGet(s"$obpPrefix/v3.0.0/users/current").flatMap(_.extractOpt[CurrentUserJson]) 
    else 
      Failure("Please Login in first.")

  def getRoot : Box[JValue]= ObpGet(s"$obpPrefix/v4.0.0/root")
  
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
                      offset: Option[Int], fromDate: Option[Date], toDate: Option[Date], sortDirection: Option[SortDirection]) : Box[TransactionsJsonV300]= {

    val headers : List[Header] = limit.map(l => Header("obp_limit", l.toString)).toList ::: offset.map(o => Header("obp_offset", o.toString)).toList :::
      fromDate.map(f => Header("obp_from_date", dateFormat.format(f))).toList ::: toDate.map(t => Header("obp_to_date", dateFormat.format(t))).toList :::
      sortDirection.map(s => Header("obp_sort_direction", s.value)).toList ::: Nil

    ObpGet(s"$obpPrefix/v3.0.0/banks/" + urlEncode(bankId) + AccountUrlPath + urlEncode(accountId) + "/" + urlEncode(viewId) +
      "/transactions", headers).flatMap(x => x.extractOpt[TransactionsJsonV300])
  }







  def publicAccounts(bankId : String) : Box[BarebonesAccountsJson] = {
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + AccountUrlPath + "public").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def publicAccounts : Box[BarebonesAccountsJson] = {
    ObpGet(s"$obpPrefix/v3.1.0${AccountUrlPath}public").flatMap(_.extractOpt[BarebonesAccountsJson])
  }

  def privateAccounts(bankId : String) : Box[BarebonesAccountsJson] = {
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + AccountUrlPath + "private").flatMap(_.extractOpt[BarebonesAccountsJson])
  } 

  def privateAccounts : Box[BarebonesAccountsJson] = {
    ObpGet(s"$obpPrefix/v1.2.1${AccountUrlPath}private").flatMap(_.extractOpt[BarebonesAccountsJson])
  }
  
  @deprecated("This method will mix public and private, not clear for Apps.","2018-02-18")
  def allAccountsAtOneBank(bankId : String) : Box[BarebonesAccountsJson] = {
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + "/accounts").flatMap(_.extractOpt[BarebonesAccountsJson])
  } 

  // Similar to getViews below
  def getViewsForBankAccount(bankId: String, accountId: String) = {
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + bankId + AccountUrlPath + accountId + "/views").flatMap(_.extractOpt[ViewsJson])
  }

  def getAccount(bankId: String, accountId: String, viewId: String) : Box[AccountJson] = {
    ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + AccountUrlPath + urlEncode(accountId) + "/" + urlEncode(viewId) + "/account").flatMap(x => x.extractOpt[AccountJson])
  } 

  def getCounterparties(bankId: String, accountId: String, viewId: String): Box[DirectOtherAccountsJson] =  {
    val counterparties  = ObpGet(s"$obpPrefix/v3.1.0/banks/" + urlEncode(bankId) + AccountUrlPath + urlEncode(accountId) + "/" + urlEncode(viewId) + "/other_accounts").flatMap(x => x.extractOpt[DirectOtherAccountsJson])
    counterparties
  } 

  def getExplictCounterparties(bankId: String, accountId: String, viewId: String): Box[ExplictCounterpartiesJson] = {
     ObpGet(s"$obpPrefix/v2.2.0/banks/" + urlEncode(bankId) + AccountUrlPath + urlEncode(accountId) + "/" + urlEncode(viewId) + "/counterparties").flatMap(x => x.extractOpt[ExplictCounterpartiesJson])
  }

  def getEntitlementsV300 : Box[EntitlementsJson] = {
    if (isLoggedIn)
      ObpGet(s"$obpPrefix/v3.0.0/my/entitlements").flatMap(_.extractOpt[EntitlementsJson])
    else
      Failure("Please Login in first.")
  } 

  //Can not use the `isLoggedIn` guard here. It use for the home page, sometimes no need the authentication. 
  def getMySpaces : Box[MySpaces] = if (isLoggedIn)
    ObpGet(s"$obpPrefix/v4.0.0/my/spaces").flatMap(_.extractOpt[MySpaces])
  else
    Failure("Please Login in first.")
  

  private val jsonSchemaValidationTTL: FiniteDuration = Helper.getPropsAsIntValue("json_schema_validation.cache.ttl.seconds", 3800) seconds
  def getJsonSchemaValidations() : Box[Map[String, JObject]] = {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(jsonSchemaValidationTTL) {
        val url = s"$obpPrefix/v4.0.0/endpoints/json-schema-validations"
        ObpGet(url).flatMap {
          case JObject(JField("json_schema_validations", JArray(values @ _)) :: Nil) =>
            val operationIdToAuthTypes = values map { it =>
              val operationId = (it \ "operation_id").extract[String]
              val jsonSchema = (it \ "json_schema").asInstanceOf[JObject]
              operationId -> jsonSchema
            }
            Full(operationIdToAuthTypes.toMap)
          case v =>
            val errorMsg = s"The endpoint $url return wrong structure data, it have no 'json_schema_validations', response body: ${render(v)}"
            logger.error(errorMsg)
//            throw new IllegalStateException(errorMsg)
            Empty
        }
      }
    }
  }

  private val authenticationTypeValidationTTL: FiniteDuration = Helper.getPropsAsIntValue("authentication_type_validation.cache.ttl.seconds", 3600) seconds
  def getAuthenticationTypeValidations() : Box[Map[String, List[String]]] = {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(authenticationTypeValidationTTL) {
        val url = s"$obpPrefix/v4.0.0/endpoints/authentication-type-validations"
        ObpGet(url).flatMap {
          case JObject(JField("authentication_types_validations", JArray(values @ _)) :: Nil) =>
            val operationIdToAuthTypes = values map { it =>
              val operationId = (it \ "operation_id").extract[String]
              val allowedAuthTypes = (it \ "allowed_authentication_types").extract[List[String]]
              operationId -> allowedAuthTypes
            }
            Full(operationIdToAuthTypes.toMap)
          case v =>
            val errorMsg = s"The endpoint $url return wrong structure data, it have no 'authentication_types_validations', response body: ${render(v)}"
            logger.error(errorMsg)
//            throw new IllegalStateException(errorMsg)
            Full(Map(""->List("")))
            
        }
      }
    }
  }


  def getEntitlementRequestsV300 : Box[EntitlementRequestsJson] = {
    val result = if (isLoggedIn)
      ObpGet(s"$obpPrefix/v3.0.0/my/entitlement-requests").flatMap(_.extractOpt[EntitlementRequestsJson])
    else
      Failure("Please Login in first.")
      
    logger.debug(s"We got this result for EntitlementRequestsJson: ${result}")
    result
  } 

  def getApiCollection(apiCollectionName: String) : Box[ApiCollectionJson400] = {
    val response = ObpGet(s"$obpPrefix/v4.0.0/my/api-collections/name/$apiCollectionName").flatMap(_.extractOpt[ApiCollectionJson400])
    if (response.toString.contains("OBP-30079")) {
      createMyApiCollection("Favourites",true)
      ObpGet(s"$obpPrefix/v4.0.0/my/api-collections/name/$apiCollectionName").flatMap(_.extractOpt[ApiCollectionJson400])
    } else{
      response
    }
  } 

  def getApiCollectionEndpoints(apiCollectionName: String) : Box[ApiCollectionEndpointsJson400] = {
    if (isLoggedIn)
      ObpGet(s"$obpPrefix/v4.0.0/my/api-collections/$apiCollectionName/api-collection-endpoints").flatMap(_.extractOpt[ApiCollectionEndpointsJson400])
    else
      Failure("Please Login in first.")
  } 

  def getApiCollectionEndpointsById(apiCollectionId: String) : Box[ApiCollectionEndpointsJson400] = {
    val response = if (isLoggedIn)
      ObpGet(s"$obpPrefix/v4.0.0/api-collections/$apiCollectionId/api-collection-endpoints").flatMap(_.extractOpt[ApiCollectionEndpointsJson400])
    else
      Failure("Please Login in first.")
      
    if (isLoggedIn && response.toString.contains("OBP-30079")) {
      createMyApiCollection("Favourites",true)
      ObpGet(s"$obpPrefix/v4.0.0/my/api-collections/Favourites/api-collection-endpoints").flatMap(_.extractOpt[ApiCollectionEndpointsJson400])
    } else{
      response
    }
  }


  def getMyApiCollections = if (isLoggedIn)
    ObpGet(s"$obpPrefix/v4.0.0/my/api-collections").flatMap(_.extractOpt[ApiCollectionsJson400])
  else
    Failure("Please Login in first.")
  
  
  def createMyApiCollection (apiCollectionName: String, is_sharable:Boolean) = {
    val postSelectionEndpointJson =  PostApiCollectionJson400(apiCollectionName, is_sharable)
    ObpPost(s"$obpPrefix/v4.0.0/my/api-collections", Extraction.decompose(postSelectionEndpointJson))
  }
  
  def createMyApiCollectionEndpoint (apiCollectionName: String, webPageOperationId: String) = {
    val obpOperationId = covertWebpageIdToObpOperationId(webPageOperationId)
    val postSelectionEndpointJson =  PostSelectionEndpointJson400(obpOperationId)
    ObpPost(s"$obpPrefix/v4.0.0/my/api-collections/$apiCollectionName/api-collection-endpoints", Extraction.decompose(postSelectionEndpointJson))
  }

  def deleteMyApiCollectionEndpoint (apiCollectionName: String, webPageOperationId: String)  = {
    val obpOperationId = covertWebpageIdToObpOperationId(webPageOperationId)
    ObpDelete(s"$obpPrefix/v4.0.0/my/api-collections/$apiCollectionName/api-collection-endpoints/$obpOperationId")
  }

  @deprecated("16-11-2021","this is the Legacy props, now we introduce `webui_index_dynamic_url_text_pairs` ")
  def getApiCollectionsFromPropsLegacy: List[(String, String)] = {
    val webuiIndexDynamic1LinkUrl = Helper.getPropsValue("webui_index_dynamic_1_link_url", "")
    val webuiIndexDynamic1LinkText = Helper.getPropsValue("webui_index_dynamic_1_link_text", "")

    val webuiIndexDynamic2LinkUrl = Helper.getPropsValue("webui_index_dynamic_2_link_url", "")
    val webuiIndexDynamic2LinkText = Helper.getPropsValue("webui_index_dynamic_2_link_text", "")
    
    if (webuiIndexDynamic1LinkUrl.nonEmpty && webuiIndexDynamic1LinkText.nonEmpty && webuiIndexDynamic2LinkText.nonEmpty && webuiIndexDynamic2LinkUrl.nonEmpty){
      List(
        (webuiIndexDynamic1LinkText,webuiIndexDynamic1LinkUrl), (webuiIndexDynamic2LinkText,webuiIndexDynamic2LinkUrl)
      )
    } else if (webuiIndexDynamic1LinkUrl.nonEmpty && webuiIndexDynamic1LinkText.nonEmpty )
      List(
        (webuiIndexDynamic1LinkText,webuiIndexDynamic1LinkUrl)
      )
    else
      Nil
  }
  
  val dynamicUrlTextPairsJson: List[DynamicUrlTextPairsJson] = {
    def extractor(str: String) = try {
      val dynamicUrlTextPairs =  json.parse(str).extract[List[DynamicUrlTextPairsJson]]
      //The props value can be parse to JNothing.
      if(str.nonEmpty && dynamicUrlTextPairs == Nil)
        throw new RuntimeException(s"props [webui_index_dynamic_url_text_pairs] parse -> extract to Nil! it should be the valid class($DynamicUrlTextPairsJson) json format, current value is $str .")
      else
        dynamicUrlTextPairs
    } catch {
      case e: Throwable => // error handling, found wrong props value as early as possible.
        this.logger.error(s"props [webui_index_dynamic_url_text_pairs] value is invalid, it should be the class($DynamicUrlTextPairsJson) json format, current value is $str ." );
        throw e;
    }
    Helper.getPropsValue("webui_index_dynamic_url_text_pairs").map(extractor).getOrElse(Nil)
  }
  
  def getApiCollectionsFromProps: Box[List[(String, String)]] = {
    Full(getApiCollectionsFromPropsLegacy ++ dynamicUrlTextPairsJson.map(dynamicUrlTextPair => List((dynamicUrlTextPair.text, dynamicUrlTextPair.url))).flatten)
  }
  
  /**
   * The request vars ensure that for one page load, the same API call isn't made multiple times
   */
  object allResourcesVar extends SessionVar[Box[ResourceDocsJson]] (Empty)


  // Returns both system and dynamic resource docs:
  def getAllResourceDocsJson(apiVersion : String): Box[List[ResourceDocJson]] = {

    val apiCollectionIdParam = List(ApiCollectionId)
      .map(paramName => (paramName, S.param(paramName)))
      .collect{
        case (paramName, Full(paramValue)) if(paramValue.trim.size > 0
          ) => s"$paramName=$paramValue"
      }
      .mkString("?", "&", "")
    
    //Note: ?content=static&content=dynamic
    // if there are two content parameters there, only the first one is valid for the api call. 
    // so requestParams have the high priority 
    val requestParams = List("tags", "locale", "functions", "content", CacheModifier)
        .map(paramName => (paramName, S.param(paramName)))
        .collect{
          case (paramName, Full(paramValue)) if(paramValue.trim.size > 0) => s"$paramName=$paramValue"
        }
        .mkString("?", "&", "")
    
    //If api side set resource_docs_requires_role = true, then only the users who have CanReadResourceDoc role can see the resourceDoc
    //So we set CanReadResourceDoc role as the staticResourcesDocs cache key
    val userHasCanReadResourceDocRole = getEntitlementsV300.map(_.list.map(_.role_name)).map(_.contains("CanReadResourceDoc")).openOr(false)
    val canReadResourceDocRole = userHasCanReadResourceDocRole
    
    lazy val staticResourcesDocs = getStaticResourceDocs(apiVersion, requestParams, canReadResourceDocRole, OAuthClient.loggedIn)
    
    lazy val dynamicResourcesDocs = getDynamicResourceDocs(apiVersion,requestParams, canReadResourceDocRole, OAuthClient.loggedIn)

    //If the api-collection-id in the URL, it will ignore all other parameters, so here we first check it:
    if(apiCollectionIdParam.contains(ApiCollectionId + "=")) {
      getResourceDocsByApiCollectionId(apiVersion, apiCollectionIdParam)
    }else if(requestParams.contains(ContentEqualStatic)) {
      staticResourcesDocs
    } else if (requestParams.contains(ContentEqualDynamic)){
      dynamicResourcesDocs
    } else{
      if(showDynamicResourceDocs){
        for{
            staticResourcesDocsList <-staticResourcesDocs
            dynamicResourcesDocsList <-dynamicResourcesDocs
          } yield {
            staticResourcesDocsList++dynamicResourcesDocsList
          }
      }else{
        staticResourcesDocs
      }
    }
  }
  
  // Returns all bank level dynamic resources
  def getStaticAndAllBankLevelDynamicResourceDocs(apiVersion : String) = {

    val apiCollectionIdParam = List(ApiCollectionId)
      .map(paramName => (paramName, S.param(paramName)))
      .collect{
        case (paramName, Full(paramValue)) if(paramValue.trim.size > 0) => s"$paramName=$paramValue"
      }
      .mkString("?", "&", "")
    
    //Note: ?content=static&content=dynamic
    // if there are two content parameters there, only the first one is valid for the api call. 
    // so requestParams have the high priority 
    val requestParams = List("tags", "locale", "functions", CacheModifier)
        .map(paramName => (paramName, S.param(paramName)))
        .collect{
          case (paramName, Full(paramValue)) if(paramValue.trim.size > 0) => s"$paramName=$paramValue"
        }
        .mkString("?", "&", "")
    
    //If api side set resource_docs_requires_role = true, then only the users who have CanReadResourceDoc role can see the resourceDoc
    //So we set CanReadResourceDoc role as the staticResourcesDocs cache key
    val userHasCanReadResourceDocRole = getEntitlementsV300.map(_.list.map(_.role_name)).map(_.contains("CanReadResourceDoc")).openOr(false)
    val canReadResourceDocRole = userHasCanReadResourceDocRole
    
    val canReadDynamicResourceDocsAtOneBankEntitlementBankIds = getMySpaces.map(_.bank_ids).getOrElse(Nil)
    
    lazy val staticResourcesDocs = getStaticResourceDocs(apiVersion, requestParams, canReadResourceDocRole, OAuthClient.loggedIn)
    
    lazy val dynamicResourcesDocs = tryo (canReadDynamicResourceDocsAtOneBankEntitlementBankIds.map(
      bankId => getBankLevelDynamicResourceDocs(apiVersion,bankId,requestParams)).flatten.flatten)
    
    //If the api-collection-id in the URL, it will ignore all other parameters, so here we first check it:
    if(apiCollectionIdParam.contains(ApiCollectionId + "=")) {
      getResourceDocsByApiCollectionId(apiVersion, apiCollectionIdParam)
    }else if(requestParams.contains(ContentEqualStatic)) {
      staticResourcesDocs
    } else if (requestParams.contains(ContentEqualDynamic)){
      dynamicResourcesDocs
    } else{
      for{
        staticResourcesDocsList <-staticResourcesDocs
        dynamicResourcesDocsList <-dynamicResourcesDocs
      } yield {
        staticResourcesDocsList++dynamicResourcesDocsList
      }
    }
  }

  // Returns only the bank level resource docs
  def getOneBankLevelResourceDocsJson(apiVersion : String, bankId:String) = {
    val apiCollectionIdParam = List(ApiCollectionId)
      .map(paramName => (paramName, S.param(paramName)))
      .collect{
        case (paramName, Full(paramValue)) if(paramValue.trim.size > 0) => s"$paramName=$paramValue"
      }
      .mkString("?", "&", "")

    //Note: ?content=static&content=dynamic
    // if there are two content parameters there, only the first one is valid for the api call. 
    // so requestParams have the high priority 
    val requestParams = List("tags", "locale", "functions", "content", CacheModifier)
      .map(paramName => (paramName, S.param(paramName)))
      .collect{
        case (paramName, Full(paramValue)) if(paramValue.trim.size > 0) => s"$paramName=$paramValue"
      }
      .mkString("?", "&", "")

    getBankLevelDynamicResourceDocs(apiVersion, bankId, requestParams)
  }

  private val DAYS_365 = 31536000
  //  static resourceDocs can be cached for a long time, only be changed when new deployment.
  val getStaticResourceDocsJsonTTL: FiniteDuration = Helper.getPropsAsIntValue("static_resource_docs_json.cache.ttl.seconds", DAYS_365) seconds
  def getStaticResourceDocs(apiVersion : String, requestParams: String, canReadResourceDocRole: Boolean, isLoggedIn: Boolean): Box[List[ResourceDocJson]] =  {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(getStaticResourceDocsJsonTTL) {
        val requestParamsRemovedContent = requestParams.replace(ContentEqualStatic,"")
        getResourceDocs(apiVersion, requestParamsRemovedContent, "static")
      }
    }
  }

  //  dynamic resourceDocs can be cached only for short time, 1 hour 
  private val HOUR_1 = 3600
  val getDynamicResourceDocsJsonTTL: FiniteDuration = Helper.getPropsAsIntValue("dynamic_resource_docs_json.cache.ttl.seconds", HOUR_1) seconds
  def getDynamicResourceDocs(apiVersion : String, requestParams: String, canReadResourceDocRole: Boolean, isLoggedIn: Boolean): Box[List[ResourceDocJson]] =  {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(getDynamicResourceDocsJsonTTL) {
        val requestParamsRemovedContent = requestParams.replace(ContentEqualDynamic,"")
        getResourceDocs(apiVersion, requestParamsRemovedContent, "dynamic")
      }
    }
  }

  def getResourceDocs(apiVersion : String, requestParams: String, contentTag: String) = {
    logger.debug("getResourceDocs says:")
    logger.debug("apiVersion:" + apiVersion)
    logger.debug("requestParams:" + requestParams)
    getResourceDocsJValueResponse(apiVersion : String, requestParams: String, contentTag: String).map(extractResourceDocsJson).map(_.resource_docs)
  }

  def getResourceDocsJValueResponse(apiVersion : String, requestParams: String, contentTag: String) = {
    logger.debug("getResourceDocsJValueResponse says Hello")
    val result = ObpGet(s"$obpPrefix/v4.0.0/resource-docs/$apiVersion/obp$requestParams&content=$contentTag")
    logger.debug("requestParams says result is: " + requestParams)
    logger.trace("getResourceDocsJValueResponse says result is: " + result)
    result
  }

  def getBankLevelDynamicResourceDocs(apiVersion : String, bankId:String, requestParams: String) = {
    logger.debug("getBankLevelResourceDocs says:")
    logger.debug("apiVersion:" + apiVersion)
    logger.debug("bankId:" + apiVersion)
    logger.debug("requestParams:" + requestParams)
    getBankLevelDynamicResourceDocsJValueResponse(apiVersion : String, bankId:String, requestParams: String).map(extractResourceDocsJson).map(_.resource_docs)
  }
  
  def getBankLevelDynamicResourceDocsJValueResponse(apiVersion : String, bankId:String, requestParams: String) = {
    logger.debug("getBankLevelResourceDocsJValueResponse says Hello")
    val result = ObpGet(s"$obpPrefix/v4.0.0/banks/$bankId/resource-docs/$apiVersion/obp$requestParams&$CacheModifier=${UUID.randomUUID().toString}")
    logger.trace("getBankLevelResourceDocsJValueResponse says result is: " + result)
    result  
  }
  
  def getResourceDocsByApiCollectionId(apiVersion : String, requestParams: String) =
    ObpGet(s"$obpPrefix/v4.0.0/resource-docs/$apiVersion/obp$requestParams").map(extractResourceDocsJson).map(_.resource_docs)

  def getApiCollectionByIdJValueResponse(apiVersion : String) = {
    val apiCollectionIdParam = List(ApiCollectionId)
      .map(paramName => (paramName, S.param(paramName)))
      .collect{
        case (paramName, Full(paramValue)) if(paramValue.trim.size > 0) => s"$paramName=$paramValue"
      }
      .mkString("?", "&", "")
    ObpGet(s"$obpPrefix/v4.0.0/resource-docs/$apiVersion/obp$apiCollectionIdParam")
  }
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


  private val DAYS_30 = 2592000
  //  this is one month
  val getGlossaryItemsJsonTTL: FiniteDuration = Helper.getPropsAsIntValue("glossary_items_json.cache.ttl.seconds", DAYS_30) seconds
  
  def getGlossaryItemsJson : Box[GlossaryItemsJsonV300] = {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(getGlossaryItemsJsonTTL) {
        ObpGet(s"$obpPrefix/v5.1.0/api/glossary").flatMap(_.extractOpt[GlossaryItemsJsonV300])
      }
    }
  }

  //  this is one month
  val getMessageDocsJsonTTL: FiniteDuration = Helper.getPropsAsIntValue("message_docs_json.cache.ttl.seconds", DAYS_30) seconds
  
  def getMessageDocsJson(connector: String) : Box[MessageDocsJsonV220] = {
    var cacheKey = (randomUUID().toString, randomUUID().toString, randomUUID().toString)
    CacheKeyFromArguments.buildCacheKey {
      Caching.memoizeSyncWithProvider(Some(cacheKey.toString()))(getMessageDocsJsonTTL) {
  
        ObpGet(s"$obpPrefix/v2.2.0/message-docs/$connector").flatMap(_.extractOpt[MessageDocsJsonV220])
      }
    }
  }

}

// The code below is introduced in order to support Application Access via API Explorer.
// For instance using Hydra ORA as Identity Provider
object IdentityProviderRequest extends MdcLoggable {

  val clientId = Helper.getPropsValue("obp_consumer_key", "")
  val clientSecret = Helper.getPropsValue("obp_secret_key", "")
  val tokenEndpoint = Helper.getPropsValue("identity_provider_token_endpoint", "http://127.0.0.1:4444/oauth2/token")
  val integrateWithIdentityProvider = Helper.getPropsAsBoolValue("integrate_with_identity_provider", false)
  val jwsAlg = Helper.getPropsValue("oauth2.jws_alg", "ES256")
  val jwkPrivateKey = Helper.getPropsValue("oauth2.jwk_private_key")

  val jwk: Option[JWK] = jwkPrivateKey.map(JWK.parse(_))
  lazy val jwsSigner: Option[JWSSigner] = {
    if (jwkPrivateKey.isDefined) {
      if (jwsAlg.startsWith("ES")) Some(new ECDSASigner(jwk.get.asInstanceOf[ECKey]))
      else {
        if (jwsAlg.startsWith("PS")) Security.addProvider(BouncyCastleProviderSingleton.getInstance)
        val privateKey = jwk.get.asInstanceOf[RSAKey]
        Some(new RSASSASigner(privateKey))
      }
    } else {
      None
    }
  }
  

  private def signClaims(claimsSet: JWTClaimsSet) = {
    val jwt = new SignedJWT(new JWSHeader.Builder(JWSAlgorithm.parse(jwsAlg)).keyID(jwk.get.getKeyID).build, claimsSet)
    // Sign with private EC key
    try jwt.sign(jwsSigner.get)
    catch {
      case e: JOSEException =>
        println(e)
    }
    jwt.serialize
  }

  def buildClientAssertion: String = { // JWT claims
    //iss: REQUIRED. Issuer. This MUST contain the client_id of the OAuth Client.
    //sub: REQUIRED. Subject. This MUST contain the client_id of the OAuth Client.
    //aud: REQUIRED. Audience. The aud (audience) Claim. Value that identifies the Authorization Server (ORY Hydra) as an intended audience. The Authorization Server MUST verify that it is an intended audience for the token. The Audience SHOULD be the URL of the Authorization Server's Token Endpoint.
    //jti: REQUIRED. JWT ID. A unique identifier for the token, which can be used to prevent reuse of the token. These tokens MUST only be used once, unless conditions for reuse were negotiated between the parties; any such negotiation is beyond the scope of this specification.
    //exp: REQUIRED. Expiration time on or after which the ID Token MUST NOT be accepted for processing.
    //iat: OPTIONAL. Time at which the JWT was issued.
    val claimsSet = new JWTClaimsSet.Builder()
      .issuer(clientId).subject(clientId).audience(tokenEndpoint)
      .jwtID(UUID.randomUUID.toString)
      .expirationTime(new Date(new Date().getTime + 60 * 1000))
      .issueTime(new Date).build
    signClaims(claimsSet)
  }
  
  def isPublicClient() = jwkPrivateKey.isDefined
  
  def obtainAccessToken() = {
    import okhttp3.{OkHttpClient, Request, RequestBody}
    val client = new OkHttpClient().newBuilder.build
    val mediaType = MediaType.parse("application/x-www-form-urlencoded;charset=UTF-8")
    val grantType = "client_credentials"


    val body = isPublicClient match {
      case true =>
        RequestBody.create(mediaType, s"grant_type=$grantType&client_assertion_type=urn%3Aietf%3Aparams%3Aoauth%3Aclient-assertion-type%3Ajwt-bearer&client_assertion=$buildClientAssertion")
      case false =>
        RequestBody.create(mediaType, s"grant_type=$grantType&client_id=$clientId&client_secret=$clientSecret")
    }
    
    val request = new Request.Builder()
      .url(tokenEndpoint).method("POST", body)
      .addHeader("Content-Type", "application/x-www-form-urlencoded;charset=UTF-8").build
    val response = client.newCall(request).execute
    val responseBody = response.body.string
    response.close()
    responseBody
  }
  
}

case class ObpError(error :String)

object OBPRequest extends MdcLoggable {
  implicit val formats = DefaultFormats
  //returns a tuple of the status code,  response body and list of headers
  def apply(apiPath : String, jsonBody : Option[JValue], method : String, headers : List[Header]) : Box[(Int, String, List[String], List[String])] = {
    logger.debug(s"before $apiPath call:")
    logger.debug(s"headers $headers of a call:")
    
    lazy val consentHeader = headers.map(_.key).exists(_ == "Consent-JWT") || // OBP-API endpoints
      headers.map(_.key).exists(_ == "Consent-Id") || // OBP-API endpoints
      headers.map(_.key).exists(_ == "Consent-ID") // Berlin Group endpoints

    lazy val addAppAccessIfNecessary: List[Header] = {
      if(IdentityProviderRequest.integrateWithIdentityProvider) {
        tryo(obtainAccessToken) match {
          case Full(token) if !apiPath.contains("resource-docs/OBPv5.0.0/obp") && !consentHeader => 
            Header("Authorization", s"Bearer $token") :: Nil
          case _ =>
            Nil
        }
      } else {
        Nil
      }
    }
    
    lazy val addJwsIfNecessary: List[Header] = {
      if(JwsUtil.forceVerifyRequestSignResponse(apiPath)) {
        val body = jsonBody.map(i => compact(render(i)))
        val headers = JwsUtil.signRequest(body, method, apiPath, "application/json;charset=UTF-8")
        headers.map(i => Header(key = i.name, value = i.values.mkString(",")))
      } else {
        Nil
      }
    }
    
    lazy val statusAndBody = tryo {
      val credentials = OAuthClient.getAuthorizedCredential
      val apiUrl = OAuthClient.currentApiBaseUrl

      //TODO this need to be checked again.
//      val convertedApiPath = apiPath
//        .replaceAll("UKv2.0", "v2.0")
//        .replaceAll("UKv3.1", "v3.1")
//        .replaceAll("BGv1.3", "v1.3")
//        .replaceAll("BGv1", "v1")
//        .replaceAll("OBPv", "v")
//        .replaceAll("(?<![Vv]alidations/)OBPv", "v") 

      val url = apiUrl + apiPath

//      logger.info(s"OBP Server Request URL: ${apiUrl}${convertedApiPath}")

      //bleh
      val request = SSLHelper.getConnection(url) //blagh!
      request.setDoOutput(true)
      request.setRequestMethod(method)
      request.setRequestProperty("content-type", "application/json;charset=utf-8")
      request.setRequestProperty("Accept", "application/json")
      request.setRequestProperty("Accept-Charset", "UTF-8")


      // Please note that this MUST be called before we set the body of our request
      // Otherwise it can fail due to IllegalStateException("Already connected")
      // In the end we cannot see info at Request Header's Box at API Explorer 
      val requestHeaders = credentials match {
        case None => // Application access(Client Credentials)
          addAppAccessIfNecessary.foreach(header => request.setRequestProperty(header.key, header.value))
          addJwsIfNecessary.foreach(header => request.setRequestProperty(header.key, header.value))
          (addAppAccessIfNecessary  ::: addJwsIfNecessary ::: headers).map(header => (header.key, Set(header.value))) :::
            request.getRequestProperties().asScala.mapValues(_.asScala.toSet).toList
        case Some(credential) => // User access; sign the request if we have some credentials to sign it with
          credential.consumer.sign(request)
          addJwsIfNecessary.foreach(header => request.setRequestProperty(header.key, header.value))
          (addJwsIfNecessary ::: headers).map(header => (header.key, Set(header.value))) :::
            request.getRequestProperties().asScala.mapValues(_.asScala.toSet).toList
      }
      headers.foreach(header => request.setRequestProperty(header.key, header.value))

      //Set the request body
      if(jsonBody.isDefined && jsonBody.head != JNothing) {
        val output = request.getOutputStream()
        val writer = new BufferedWriter(new OutputStreamWriter(output, "UTF-8"))
        writer.write(compact(render(jsonBody.get)).toString)
        writer.flush()
        writer.close()
      }
      
      val adjustedRequestHeaders = requestHeaders.to[Set].toList
        .map(x => x._1 + ": " + x._2.mkString(", "))
        .sortWith(_ < _).filter(_.startsWith("null") == false)


      request.connect()
      val status = request.getResponseCode()
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
      (status, builder.toString(), adjustedResponseHeaders, adjustedRequestHeaders)
    }

    val urlParametersUrl = apiPath.split("\\?")
    val hasDuplicatedUrlParameters = if(urlParametersUrl.length >1) {
      val duplicatedParameters = urlParametersUrl(1).split("&").map(_.split("=")).map(_.head).toList.groupBy(identity).collect { case (x, List(_,_,_*)) => x }
      if(duplicatedParameters.size >0) {
        (true, duplicatedParameters)
      } else{ (false,"")}
    } else (false,"")
    
    val result = if(hasDuplicatedUrlParameters._1) {
      Failure(s"Duplicated URL Parameter, please check the parameter ${hasDuplicatedUrlParameters._2}")
      } else {
        statusAndBody pass {
        case Failure(msg, ex, _) => {
          val sw = new StringWriter()
          val writer = new PrintWriter(sw)
          ex.foreach(_.printStackTrace(writer))
          logger.debug("Error making api call: " + msg + ", stack trace: " + sw.toString)
        }
        case _ => Unit
        }
      }
    logger.debug(s"after $apiPath call:")
    result
  }

  private def obtainAccessToken: String = {
    val jsonResponse = IdentityProviderRequest.obtainAccessToken()
    import net.liftweb.json._
    val jsonAst = parse(jsonResponse) \ "access_token"
    val accessToken = compact(render(jsonAst)).stripPrefix("\"").stripSuffix("\"")
    accessToken
  }
}

object ObpPut {
  def apply(apiPath: String, json : JValue): Box[JValue] = {
    OBPRequest(apiPath, Some(json), "PUT", Nil) match {
      case Full((status, result, _, _)) => APIUtils.getAPIResponseBody(status, result)
      case Failure(msg, exception, chain) => Failure(msg)
      case _ =>Failure(UnknownErrorMessage)
    }
  }
}
object ObpPutWithHeader {
  def apply(apiPath: String, json : JValue, headers : List[Header] = Nil): (Box[JValue], List[String], List[String]) = {
    OBPRequest(apiPath, Some(json), "PUT", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3, value._4)
      case Failure(msg, exception, chain) => (Failure(msg), Nil,Nil)
      case _ => (Failure(UnknownErrorMessage),Nil,Nil)
    }
  }
}

object ObpPost {
  def apply(apiPath: String, json : JValue): Box[JValue] = {
    OBPRequest(apiPath, Some(json), "POST", Nil) match {
      case Full((status, result, _, _)) => APIUtils.getAPIResponseBody(status, result)
      case Failure(msg, exception, chain) => Failure(msg)
      case _ => Failure(UnknownErrorMessage)
    }
  }
}
object ObpPostWithHeader {
  def apply(apiPath: String, json : JValue, headers : List[Header] = Nil): (Box[JValue], List[String], List[String]) = {
    val requestBody = json match {
      case JNothing | JNull => None
      case v => Option(v)
    }
    OBPRequest(apiPath, requestBody, "POST", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3, value._4)
      case Failure(msg, exception, chain) => (Failure(msg), Nil, Nil)
      case _ => (Failure(UnknownErrorMessage), Nil, Nil)
    }
  }
}

object ObpDeleteBoolean {
  /**
   * @return True if the delete worked
   */
  def apply(apiPath: String): Boolean = {
    val worked = OBPRequest(apiPath, None, "DELETE", Nil).map {
      case(status, result, _, _) => APIUtils.apiResponseWorked(status, result)
    }
    worked.getOrElse(false)
  }
}



// In case we want a more raw result
// TODO
object ObpDelete {
  def apply(apiPath: String): Box[JValue] = {
    OBPRequest(apiPath, None, "DELETE", Nil) match {
      case Full((status, result, _, _)) => APIUtils.getAPIResponseBody(status, result)
      case Failure(msg, exception, chain) => Failure(msg)
      case _ => Failure(UnknownErrorMessage)
    }
  }
}
object ObpDeleteWithHeader {
  def apply(apiPath: String, headers : List[Header] = Nil): (Box[JValue], List[String], List[String]) = {
    OBPRequest(apiPath, None, "DELETE", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3, value._4)
      case Failure(msg, exception, chain) => (Failure(msg), Nil, Nil)
      case _ => (Failure(UnknownErrorMessage), Nil, Nil)
    }
  }
}



object ObpGet {
  def apply(apiPath: String, headers : List[Header] = Nil): Box[JValue] = {
    // the bankId is blank
    if(apiPath.contains("/banks//")) {
      Empty
    } else {
      OBPRequest(apiPath, None, "GET", headers) match {
        case Full((status, result, _, _)) => APIUtils.getAPIResponseBody(status, result)
        case Failure(msg, exception, chain) => Failure(msg)
        case _ => Failure(UnknownErrorMessage)
      }
    }
  }
}
object ObpHead {
  def apply(apiPath: String, headers : List[Header] = Nil): Box[JValue] = {
    // the bankId is blank, avoid sending empty bankId like this "/banks//accounts".
    if(apiPath.contains("/banks//")) {
      Empty
    } else {
      OBPRequest(apiPath, None, "HEAD", headers) match {
        case Full((status, result, _, _)) => APIUtils.getAPIResponseBody(status, result)
        case Failure(msg, exception, chain) => Failure(msg)
        case _ => Failure(UnknownErrorMessage)
      }
    }
  }
}

object ObpGetWithHeader {
  def apply(apiPath: String, headers : List[Header] = Nil): (Box[JValue], List[String], List[String]) = {
    OBPRequest(apiPath, None, "GET", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3, value._4)
      case Failure(msg, exception, chain) => (Failure(msg), Nil, Nil)
      case _ => (Failure(UnknownErrorMessage), Nil, Nil)
    }
  }
}

object ObpHeadWithHeader {
  def apply(apiPath: String, headers : List[Header] = Nil): (Box[JValue], List[String], List[String]) = {
    OBPRequest(apiPath, None, "HEAD", headers) match {
      case Full(value) => (APIUtils.getAPIResponseBody(value._1, value._2), value._3, value._4)
      case Failure(msg, exception, chain) => (Failure(msg), Nil, Nil)
      case _ => (Failure(UnknownErrorMessage), Nil, Nil)
    }
  }
}

object APIUtils extends MdcLoggable {
  implicit val formats = DefaultFormats

  def getAPIResponseBody(responseCode : Int, body : String) : Box[JValue] = {
    logger.debug("Before getAPIResponseBody(String ->JValue): ")
    processApiResponse(responseCode, body)
  }
  def deleteApiResponse(responseCode : Int, body : String) : Box[JValue] = {
    processApiResponse(responseCode, body)
  }

  private def processApiResponse(responseCode: Int, body: String) = {
    val jvalueBox = tryo {
      parse(body)
    }
    responseCode match {
      case 200 | 201 | 202 | 204 =>
        logger.debug("After getAPIResponseBody(String -> JValue): ")
        jvalueBox
      case _ =>
        val failMsg = "Bad response code (" + responseCode + ") from OBP API server: " + body
        logger.warn(failMsg)
        if (jvalueBox.isDefined) {
          Failure(Helper.renderJson(jvalueBox.head))
        } else {
          Failure(Helper.renderJson(parse(
            s"""{
              "http_code": $responseCode
              "details": "Resource Not Found .Sorry, we couldn't find the page you were looking for. Maybe this is because you don't have the right permissions."
            }""")))
        }
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
  case class HostedBy400(
    organisation : String,
    email : String,
    phone : String,
    organisation_website: String
  )
  case class HostedAt400(
    organisation : String,
    organisation_website: String
  )
  case class EnergySource400(
    organisation : String,
    organisation_website: String
  )
  case class APIInfoJson400(
    version : String,
    version_status: String,
    git_commit : String,
    connector : String,
    hosted_by : HostedBy400,
    hosted_at : HostedAt400,
    energy_source : EnergySource400,
    resource_docs_requires_role: Boolean
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


  case class MySpaces(
    bank_ids: List[String]
  )

  case class EntitlementsJson (list : List[EntitlementJson])
  
  case class ApiCollectionEndpointJson400 (
    api_collection_endpoint_id: String,
    api_collection_id: String,
    operation_id: String
  )
  
  case class ApiCollectionEndpointsJson400(
    api_collection_endpoints: List[ApiCollectionEndpointJson400]
  )

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
  case class DynamicUrlTextPairsJson (url:String, text:String)
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
                             operationId: String,
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

case class Bank(
  id : String,
  shortName : String,
  fullName : String,
  logo : String,
  website : String,
  isFeatured : Boolean
)

case class CreateEntitlementRequestJSON(bank_id: String, role_name: String)

case class PostApiCollectionJson400(
  api_collection_name: String,
  is_sharable: Boolean
)

case class PostSelectionEndpointJson400(
  operation_id: String
)

case class SelectionEndpointJson400 (
  selection_endpoint_id: String,
  selection_id: String,
  operation_id: String
)

case class UserEntitlementRequests(entitlementRequestId: String,
  roleName: String,
  bankId : String,
  username : String 
)
case class ApiCollectionJson400 (
  api_collection_id: String,
  user_id: String,
  api_collection_name: String,
  is_sharable: Boolean
)

case class ApiCollectionsJson400 (
  api_collections: List[ApiCollectionJson400]
)