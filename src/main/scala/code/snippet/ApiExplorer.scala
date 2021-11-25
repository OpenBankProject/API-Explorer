package code.snippet

import code.lib.ObpAPI._
import code.lib.ObpJson._
import code.lib.{ObpAPI, ObpGet, _}
import code.util.Helper
import code.util.Helper.{MdcLoggable, covertObpOperationIdToWebpageId}
import net.liftweb.json
import net.liftweb.util.{CssSel, Html5}
import java.net.URL
import scala.collection.immutable.{List, Nil}
import net.liftweb.http.{S, SHtml}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.{Extraction, JsonParser}

import scala.xml.{NodeSeq, Text}
// for compact render
import code.lib.ObpAPI.{allBanks, getEntitlementRequestsV300, getEntitlementsV300, 
  getGlossaryItemsJson, getMessageDocsJson,
  getAllResourceDocsJson,
  getOneBankLevelResourceDocsJson,
  getStaticAndAllBankLevelDynamicResourceDocs,
  isLoggedIn}
import net.liftweb.common._
import net.liftweb.http.CurrentReq
import net.liftweb.http.SHtml.{ajaxSelect, text}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetHtml}
import net.liftweb.json.Serialization.writePretty
import net.liftweb.json._
import net.liftweb.util.Helpers._


// see https://simply.liftweb.net/index-7.10.html on css selectors

/*
Present a list of OBP resource URLs
 */
class ApiExplorer extends MdcLoggable {


  /*

WIP to add comments on resource docs. This code copied from Sofit.

  val commentDateFormat = new SimpleDateFormat("kk:mm:ss EEE MMM dd yyyy")
  val NOOP_SELECTOR = "#i_am_an_id_that_should_never_exist" #> ""


  val user =  MinimalUserJsonV300(
                                 user_id = "Asdf",
                                 username = "simonredfern",
                                 provider = "google.com"
                               )


  def showComments = {
    val commentJsons = List(
      ResourceDocCommentJsonV300(
        id = Some("123"),
        text = Some("abc"),
        user = Some(user),
        date = Some(now),
        reply_to_id = None)
    )

    def showComments(comments: List[ResourceDocCommentJsonV300]) = {

      def orderByDateDescending =
        (comment1: ResourceDocCommentJsonV300, comment2: ResourceDocCommentJsonV300) =>
          comment1.date.getOrElse(now).before(comment2.date.getOrElse(now))

      ".commentsContainer" #> {
        "#noComments" #> "" &
          ".comment" #> comments.sortWith(orderByDateDescending).zipWithIndex.map {
            case (commentJson, position) => commentCssSel(commentJson, position + 1)
          }
      }
    }
  }


  def commentCssSel(commentJson: ResourceDocCommentJsonV300, displayPosition : Int) = {
    def commentDate: CssSel = {
      commentJson.date.map(d => {
        ".commentDate *" #> commentDateFormat.format(d)
      }) getOrElse
        ".commentDate" #> ""
    }

    def userInfo: CssSel = {
      commentJson.user.map(u => {
        ".userInfo *" #> {
          " -- " + u.username
        }
      }) getOrElse
        ".userInfo" #> ""
    }

    ".text *" #> commentJson.text.getOrElse("") &
      ".commentLink * " #> { "#" + displayPosition } &
      ".commentLink [id]" #> displayPosition &
      commentDate &
      userInfo
  }

*/


  // In case we use Extraction.decompose
  implicit val formats = net.liftweb.json.DefaultFormats
  // Get entitlements for the logged in user

  def getEntitlementsForCurrentUser: List[Entitlement] = getEntitlementsV300 match {
    case Full(x) => x.list.map(i => Entitlement(entitlementId = i.entitlement_id, roleName = i.role_name, bankId = i.bank_id))
    case _ => List()
  }


  def getUserEntitlementRequests: List[UserEntitlementRequests] = {

   val result =  getEntitlementRequestsV300 match {
      case Full(x) => x.entitlement_requests.map(i => UserEntitlementRequests(
        entitlementRequestId = i.entitlement_request_id,
        roleName = i.role_name,
        bankId = i.bank_id,
        username = i.user.username)
      )
      case _ => List()
    }
    logger.debug(s"getUserEntitlementRequests will return: $result" )
    result
  }
  val listAllBanks = S.param("list-all-banks").getOrElse("false").toBoolean
  logger.info(s"all_banks in url param is $listAllBanks")

  val currentGitCommit = gitCommit
  logger.info(s"currentGitCommit $currentGitCommit")

  val currentOperationId = S.param("operation_id").getOrElse("OBPv3_1_0-config")


  val presetBankId = S.param("bank_id").getOrElse("")
  logger.info(s"bank_id in url param is $presetBankId")

  val presetAccountId = S.param("account_id").getOrElse("")
  logger.info(s"account_id in url param is $presetAccountId")

  val presetViewId = S.param("view_id").getOrElse("")
  logger.info(s"account_id in url param is $presetViewId")

  val presetCounterpartyId = S.param("counterparty_id").getOrElse("")
  logger.info(s"counterparty_id in url param is $presetCounterpartyId")

  val presetTransactionId = S.param("transaction_id").getOrElse("")
  logger.info(s"transaction_id in url param is $presetTransactionId")


  val presetConnector = S.param("connector").getOrElse("kafka_vSept2018")
  logger.info(s"connector in url param is $presetConnector")

  val queryString = S.queryString

  def stringToOptBoolean (x: String) : Option[Boolean] = x.toLowerCase match {
    case "true" | "yes" | "1" | "-1" => Some(true)
    case "false" | "no" | "0" => Some(false)
    case _ => Empty
  }

  /** native is a query parameter to filter endpoints by implementedBy
    * Used to list newly implemented or updated functionality
    * native = true means only show endpoints that are implemented in the version we are calling.
    * native = false means only show endpoints that are not implemented in the version we are calling
    * native = None means ignore this filter, show everything in the version.
  */
  val nativeParam: Option[Boolean] = for {
    x <- S.param("native")
    y <- stringToOptBoolean(x)
  } yield y

  logger.info(s"nativeParam is $nativeParam")

  def apiCollectionIdParam = S.param(ApiCollectionId)

  logger.info(s"apiCollectionIdParam is $apiCollectionIdParam")

  val rawTagsParam = S.param("tags")

  logger.info(s"rawTagsParam is $rawTagsParam")

  val rawLanguageParam = S.param("language")

  logger.info(s"rawLanguageParam is $rawLanguageParam")

  def apiCollectionIdParamString = if (apiCollectionIdParam.isEmpty) "" else "&api-collection-id=" + apiCollectionIdParam.mkString(",")

  logger.info(s"apiCollectionIdParamString is $apiCollectionIdParamString")
  
  val tagsParamString = if (rawTagsParam.isEmpty) "" else "&tags=" + rawTagsParam.mkString(",")

  logger.info(s"tagsParamString is $rawTagsParam")

  val languagesParamString = if (rawLanguageParam.isEmpty) "" else "&language=" + rawLanguageParam.mkString(",")

  logger.info(s"languagesParamString is $languagesParamString")

  val rawContentParam = S.param("content")

  logger.info(s"contentParam is $rawContentParam")

  val contentParamString = if (rawContentParam.isEmpty) "" else "&content=" + rawContentParam.mkString(",")

  logger.info(s"contentParamString is $contentParamString")

  val tagsParam: Option[List[String]] = rawTagsParam match {
    // if tags= is supplied in the url we want to ignore it
    case Full("") => None
    case _  => {
      for {
        x <- rawTagsParam
        y <- Some(x.trim().split(",").toList)
      } yield {
        y
      }

    }
  }
  logger.info(s"tags are $tagsParam")



  def apiCollectionId : String = apiCollectionIdParam match {
    case Full(x) => x
    case _ => ""
  }

  val tagsHeadline : String = tagsParam match {
    case Some(x) if (x.length == 1) => "filtered by tag: " + x.mkString(", ")
    case Some(x) if (x.length > 1)=> s"filtered by tags: ${x.head} ..."
    case _ => ""
  }

  val languageHeadline : String = rawLanguageParam match {
    case Full(x) => x
    case _ => ""
  }

  val contentHeadline : String = rawContentParam match {
    case Full(x) => x
    case _ => ""
  }

  val implementedHereHeadline : String = nativeParam match {
    case Some(true) => "(those added or modified in this version)"
    case Some(false) => "(those inherited from previous versions)"
    case _ => ""
  }

  def stringToNodeSeq(html : String) : NodeSeq = {
    // Note! scala.xml.XML.loadString fails to parse "--" (double hyphen)
    val newHtmlString = tryo {scala.xml.XML.loadString("<div>" + html + "</div>").toString()} match {
      case Full(htmlString) =>
        htmlString
      case _ =>
        logger.error(s"stringToNodeSeq says: I cannot parse the following html plus div with scala.xml.XML.loadString:" )
        logger.error(html)
        ""
    }

    //Note: `parse` method: We much enclose the div, otherwise only the first element is returned.
    Html5.parse(newHtmlString) match {
      case Full(parsedHtml) =>
        parsedHtml
      case _ =>
        logger.error("stringToNodeSeq says: I cannot Html5.parse the following html:")
        logger.error(newHtmlString)
        NodeSeq.Empty
    }
  }


  def modifiedRequestUrl(url: String, baseVersionUrl: String, presetBankId: String, presetAccountId: String) = {

    //For OBP, only use there bits, eg: v3.1.0, but for PolishAPI it used four bits: v2.1.1.1
    val versionPattern = "v([0-9].[0-9].[0-9].[0-9])".r
    val versionInUrl = (versionPattern findFirstIn url).getOrElse(baseVersionUrl)
    // replace the version in URL, the user will see it change when they click the version.
    // Only need to modify the first version in the url.
    val url1: String = url.replaceFirst(versionInUrl, baseVersionUrl)

    // Potentially replace BANK_ID
     val url2: String = presetBankId match {
        case "" => url1
        case _ => url1.replaceAll("BANK_ID", presetBankId)
      }

    // Potentially replace ACCOUNT_ID
    val url3: String = presetAccountId match {
      case "" => url2
      case _ => url2.replaceAll("/ACCOUNT_ID", s"/$presetAccountId") // so we don't change OTHER_ACCOUNT_ID
    }

    // Potentially replace VIEW_ID
    val url4: String = presetViewId match {
      case "" => url3
      case _ => url3.replaceAll("VIEW_ID", presetViewId)
    }

    // Potentially replace OTHER_ACCOUNT_ID
    val url5: String = presetCounterpartyId match {
      case "" => url4
      case _ => url4.replaceAll("OTHER_ACCOUNT_ID", presetCounterpartyId).replaceAll("COUNTERPARTY_ID",presetCounterpartyId)
    }

    // Potentially replace TRANSACTION_ID
    val url6: String = presetTransactionId match {
      case "" => url5
      case _ => url5.replaceAll("TRANSACTION_ID", presetTransactionId)
    }

    url6
  }


  //val myPrivateAccounts = privateAccountsCache


  case class BankId(val value : String) {
    override def toString = value
  }

  object BankId {
    def unapply(id : String) = Some(BankId(id))
  }


  // Get a list of BankIds that are relevant to the logged in user i.e. banks where the user has at least one non public account
  val myBankIds: List[BankId] = for {
      allAccountsJson <- if(isLoggedIn) ObpAPI.privateAccounts.toList else List() //No need call api for anonymous user.
      barebonesAccountJson <- allAccountsJson.accounts.toList.flatten
      bankId <- barebonesAccountJson.bank_id
    } yield BankId(bankId)

  val defaultVersion: String = Helper.getPropsValue("default.version") match {
    case Full(v)  => v
    case _ => OBPVersionV400
  }

    // Get the requested version from the url parameter and default if none
    val apiVersionRequested = S.param("version").getOrElse(defaultVersion)



    // Possible OBP Versions
  val obpVersionsSupported = List("OBPv3.1.0", OBPVersionV400)


  val otherVersionsSupported = List(BGVersionV13, UKVersionV31)

    // This is basically all versions supported
    val allSupportedVersionsInDropdownMenu = List(
      "OBPv1.2.1",
      "OBPv1.3.0",
      "OBPv1.4.0",
      "OBPv2.0.0",
      "OBPv2.1.0",
      "OBPv2.2.0",
      "OBPv3.0.0",
      "OBPv3.1.0",
      OBPVersionV400,
      UKVersionV31,
      "BGv1.3",
      "STETv1.4",
      PAPIVersionV2111,
      "AUv1.0.0",
      "0.6v1",
      "MXOFv1.0.0",
      "b1")

    // Set the version to use.
    val apiVersion: String = {
      if (obpVersionsSupported.contains(apiVersionRequested)
        || otherVersionsSupported.contains(apiVersionRequested)
        || allSupportedVersionsInDropdownMenu.contains(apiVersionRequested)) {        // Prefix with v (for OBP versions because they come just with number from API Explorer)
        // Note: We want to get rid of this "v" prefix ASAP.s
        s"$apiVersionRequested"
      } else {
        S.notice(s"Note: Requested version $apiVersionRequested is not currently supported. Set to v$defaultVersion")
        s"$defaultVersion"
      }
    }

    val apiVersionParamString = "&version=" + apiVersion
    logger.info(s"apiVersionParamString is $apiVersionParamString")


    val isObpVersion: Boolean = {
      obpVersionsSupported.contains(apiVersionRequested)
    }

    logger.info(s"apiVersion is: $apiVersion")


    // To link to API home page (this is duplicated in OAuthClient)
    val baseUrl = Helper.getPropsValue("api_hostname", S.hostName)
    //
    val apiPortalHostname = Helper.getPropsValue("api_portal_hostname", baseUrl)
  
    /**
     * https://demo.openbankproject.com/
     * https://demo-manager.openbankproject.com/
     * 
     * Here is the example for api_manager, will generate the url from api_host, just added the `-manager` to the first part.
     * 
     */
    private val hostParts: Array[String] = baseUrl.split("\\.")
    private val defaultApiManagerUrl = (Array(hostParts.head+"-manager")++ hostParts.drop(1)).mkString(".")
    val apiManagerUrl = Helper.getPropsValue("api_manager_url", defaultApiManagerUrl)
  
  
    val apiCreationAndManagementTags = Helper.getPropsValue("api_creation_and_management_tags",
      "API,Dynamic-Entity-Manage,Dynamic-Swagger-Doc-Manage,Dynamic-Resource-Doc-Manage,Aggregate-Metrics," +
        "Metric,Documentation,Method-Routing,Dynamic-Message-Doc-Manage,Api-Collection," +
        "Connector-Method,JSON-Schema-Validation,Authentication-Type-Validation,Dynamic-Entity-Manage,Endpoint-Mapping-Manage")
    val userManagementTags = Helper.getPropsValue("user_management_tags", "User,Role,Entitlement," +
      "Consent,Onboarding")
    val obpBankingModelTags = Helper.getPropsValue("obp_banking_model_tags", "Bank,Account,Transaction," +
      "FX,Customer-Message,Product-Collection,Product,ATM,Branch,Card,Person,User,Customer," +
      "" +
      "KYC,Counterparty,Transaction-Metadata,Transaction,Account-Access,Transaction-Request")


    // Use to show the developer the current base version url
    val baseVersionUrl = s"${OAuthClient.currentApiBaseUrl}"

    // Link to the API endpoint for the resource docs json TODO change apiVersion so it doesn't have a "v" prefix
    val resourceDocsPath = s"${OAuthClient.currentApiBaseUrl}/obp/v1.4.0/resource-docs/${apiVersion.stripPrefix("v")}/obp?${tagsParamString}${languagesParamString}${contentParamString}"

  // Link to the API endpoint for the swagger json
  val swaggerPath = s"${OAuthClient.currentApiBaseUrl}/obp/v1.4.0/resource-docs/${apiVersion.stripPrefix("v")}/swagger?${tagsParamString}${languagesParamString}${contentParamString}"

  val chineseVersionPath = "?language=zh"
  val allPartialFunctions = s"/partial-functions.html?${apiVersionParamString}${tagsParamString}${languagesParamString}${contentParamString}"

  //Note > this method is only for partial-functions.html .
  def showPartialFunctions =  {
    // Get a list of resource docs from the API server
    // This will throw an exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)
    // The overview contains html. Just need to convert it to a NodeSeq so the template will render it as such
    val allResources: List[ResourceDocJson] = for {
      rs <- getAllResourceDocsJson(apiVersion).openOrThrowException("Resource docs can not be empty here!")
    } yield rs
    // The list generated here might be used by an administrator as a white or black list of API calls for the API itself.
    val commaSeparatedListOfResources = allResources.map(_.operation_id).mkString("[", ", ", "]")

    "#all-partial-functions" #> commaSeparatedListOfResources
    OAuthClient.redirectToOauthLogin()
  }

  def getResponse (url : String, resourceVerb: String, json : JValue, customRequestHeader: String = "") : (String, String) = {

    // version is now included in the url
    val urlWithVersion = s"$url"
    val requestHeader = customRequestHeader.trim.isEmpty match {
      case true => Nil
      case false =>
        customRequestHeader.split("::").map(_.trim).map {
          i =>
            val key = i.split(":").toList.head
            val value = i.split(":").toList.reverse.head
            Header(key, value)
        }.toList
    }

    var headersOfCurrentCall: List[String] = Nil

    val responseBodyBox = {
      resourceVerb match {
        case "GET" =>
          val x = ObpGetWithHeader(urlWithVersion, requestHeader)
          headersOfCurrentCall = x._2
          x._1
        case "HEAD" =>
          val x = ObpHeadWithHeader(urlWithVersion, requestHeader)
          headersOfCurrentCall = x._2
          x._1
        case "DELETE" =>
          val x = ObpDeleteWithHeader(urlWithVersion, requestHeader)
          headersOfCurrentCall = x._2
          x._1
        case "POST" =>
          val x = ObpPostWithHeader(urlWithVersion, json, requestHeader)
          headersOfCurrentCall = x._2
          x._1
        case "PUT" =>
          val x = ObpPutWithHeader(urlWithVersion, json, requestHeader)
          headersOfCurrentCall = x._2
          x._1
        case _ => {
          val failMsg = s"API Explorer says: Unsupported resourceVerb: $resourceVerb. Url requested was: $url"
          logger.warn(failMsg)
          Failure(failMsg)
        }
      }
    }


    logger.debug(s"responseBodyBox is ${responseBodyBox}")

    // Handle the contents of the Box
    val responseBody =
      responseBodyBox match {
        case Full(JNothing) => "" // If httpmethod is `HEAD`, the response maybe JNothing here.
        case Full(json) => writePretty(json)
        case Empty => "Empty: API did not return anything"
        case Failure(message, _, _) => message
      }

    logger.debug(s"responseBody is $responseBody")
    (responseBody, headersOfCurrentCall.mkString("\n"))
  }


  // disabled button
  // enabled button if no response after 28 seconds, http2 protocol ideal timeout is 30 seconds
  def disabledBtn(name: String): JsCmd =  {
    val jsCode = s"""
                    |var timestamp = new Date().getTime();
                    |var btn = jQuery('input[name=$name]');
                    |setTimeout(function(){
                    |        btn.prop('lastClickTime', timestamp).prop('disabled', true);
                    |}, 0);
                    |setTimeout(function() {
                    |    if(btn.prop('lastClickTime') === timestamp){
                    |        btn.removeAttr('disabled');
                    |    }
                    |}, 28*1000)
                    |""".stripMargin.replaceAll("""[\r\n\s]+""", " ")
    Run (jsCode)
  }


  val entitlementsForCurrentUser = getEntitlementsForCurrentUser
  logger.info(s"there are ${entitlementsForCurrentUser.length} entitlementsForCurrentUser(s)")


  val canReadResourceRole: Option[Entitlement] = entitlementsForCurrentUser.find(_.roleName=="CanReadResourceDoc")

  val canReadGlossaryRole: Option[Entitlement] = entitlementsForCurrentUser.find(_.roleName=="CanReadGlossary")

  val userEntitlementRequests = getUserEntitlementRequests

  val canReadResourceRequest = userEntitlementRequests.find(_.roleName=="CanReadResourceDoc")

  val canReadGlossaryRequest = userEntitlementRequests.find(_.roleName=="CanReadGlossary")


  logger.info(s"there are ${userEntitlementRequests.length} userEntitlementRequests(s)")

  val canReadResourceDocRoleInfo = List(RoleInfo(
    role ="CanReadResourceDoc",
    requiresBankId = false,
    userHasEntitlement = canReadResourceRole.isDefined,
    userHasEntitlementRequest = canReadResourceRequest.isDefined
  ))

  val canReadGlossaryRoleInfo = List(RoleInfo(
    role ="CanReadGlossary",
    requiresBankId = false,
    userHasEntitlement = canReadGlossaryRole.isDefined,
    userHasEntitlementRequest = canReadGlossaryRequest.isDefined
  ))

  var entitlementRequestStatus = ""

  // This value is used to pre populate the form for Entitlement Request.
  // It maybe be changed by the user in the form.
  var rolesBankId = presetBankId

  var RolesResourceId = ""

  var entitlementRequestRoleName = ""

  var resourceId = ""
  var requestVerb = ""
  var requestUrl = ""
  var requestCustomHeader = ""
  var requestBody = "{}"
  var responseBody = "{}"
  var errorResponseBodies = List("")

  var isFavourites = "false"
  //Note: OperationIdFromWebpage = OBPv4_0_0-getBanks
  //But operationIdForOBP => OBPv4.0.0-getBanks (Javascript do not support '.' there.)
  //We must do the converting properly for this two ids.
  var favouritesOperationIdFromWebpage = ""
  var favouritesApiCollectionId = ""



  def processEntitlementRequest(name: String): JsCmd = {
    logger.debug(s"processEntitlementRequest entitlementRequestStatus is $entitlementRequestStatus rolesBankId is $rolesBankId")

    logger.debug(s"processEntitlementRequest  says resourceId is $resourceId")

    val entitlementRequestResponseStatusId = s"roles__entitlement_request_response_${RolesResourceId}_${entitlementRequestRoleName}"


    logger.debug(s"id to set is: $entitlementRequestResponseStatusId")


    val apiUrl = OAuthClient.currentApiBaseUrl

    val entitlementRequestsUrl = "/obp/v3.0.0/entitlement-requests"

    val createEntitlementRequest =  CreateEntitlementRequestJSON(bank_id = rolesBankId, role_name = entitlementRequestRoleName)

    // Convert case class to JValue
    implicit val formats = DefaultFormats
    val entitlementRequestJValue: JValue  = Extraction.decompose(createEntitlementRequest)

    // TODO: Put this in ObpAPI.scala
    val response : String = getResponse(entitlementRequestsUrl, "POST", entitlementRequestJValue)._1

    val result: String =
      try {
        // parse the string we get back from the API into a JValue
        val json : JValue = parse(response)
        // Convert to case class
        val entitlementRequest: EntitlementRequestJson = json.extract[EntitlementRequestJson]
        s"Entitlement Request with Id ${entitlementRequest.entitlement_request_id} was created. It will be reviewed shortly."
      }
      catch {
        case _ => {
          logger.info("")
          s"The API Explorer could not create an Entitlement Request: $response"
        }
      }

    // call url and put the response into the appropriate result div
    // SetHtml accepts an id and value
    SetHtml(entitlementRequestResponseStatusId, Text(result))
    // enable button
    val jsEnabledBtn = s"jQuery('input[name=$name]').removeAttr('disabled')"
    Run (jsEnabledBtn)
  }


  def showResources: CssSel = {
    logger.debug("before showResources:")
    def resourceDocsRequiresRole = ObpAPI.getRoot.flatMap(_.extractOpt[APIInfoJson400].map(_.resource_docs_requires_role)).openOr(false)
    val spaceBankId = S.param("space_bank_id").getOrElse("")

    // Get a list of resource docs from the API server
    // This will throw an exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)
    // The overview contains html. Just need to convert it to a NodeSeq so the template will render it as such
    val allResourcesBox = if (resourceDocsRequiresRole && !spaceBankId.isEmpty) //If resourceDocsRequiresRole == true && spaceBankId is there, we only return one bank level dynamic resource docs
      getOneBankLevelResourceDocsJson(apiVersion, spaceBankId)
    else if( resourceDocsRequiresRole && spaceBankId.isEmpty)//If resourceDocsRequiresRole == true && spaceBankId empty, we will return all static + all the banks dynamic resource docs
      getStaticAndAllBankLevelDynamicResourceDocs(apiVersion)
    else // other case, we will return
      getAllResourceDocsJson(apiVersion)
    
    //Here need to change to Nil, if we throw exception here, we can not render the other parts of the home page.
    val allResourcesList = allResourcesBox.getOrElse(Nil)
    
    val allResources = for {
      r <- allResourcesList
    } yield ResourceDocPlus(
      id = covertObpOperationIdToWebpageId(r.operation_id),
      operationId = r.operation_id,
      verb = r.request_verb,
      url = modifiedRequestUrl(
        r.specified_url, // We used to use the request_url - but we want to use the specified url i.e. the later version.
        apiVersion
          .replaceAll(UKVersionV20, "v2.0")
          .replaceAll(UKVersionV31, "v3.1")
          .replaceAll(BGVersionV133, VersionV133)
          .replaceAll("BGv1", "v1")
          .replaceAll(BGVersionV13, "v1.3")
          .replaceAll(PAPIVersionV2111, "v2.1.1.1")
          .replaceAll("OBPv", ""),
        presetBankId,
        presetAccountId
      ),
      summary = r.summary,
      description = stringToNodeSeq(r.description),
      exampleRequestBody = r.example_request_body,
      successResponseBody = r.success_response_body,
      errorResponseBodies = r.error_response_bodies,
      connectorMethods = r.connector_methods,
      implementedBy = ImplementedBy(r.implemented_by.version, r.implemented_by.function),
      tags = r.tags,
      roleInfos = r.roles.map(i => RoleInfo(role = i.role,
                                            requiresBankId = i.requires_bank_id,
                                            userHasEntitlement = {
                                              val result: Boolean = isLoggedIn match {
                                                case true =>
                                                {
                                                  val rolesFound = entitlementsForCurrentUser.map(_.roleName)
                                                  //logger.debug(s"rolesFound are $rolesFound")
                                                  // We only want to consider the user has the role if the role does not require bank id
                                                  // Otherwise users would find it difficult to request same role for different banks.
                                                  rolesFound.contains(i.role) && ! i.requires_bank_id
                                                }
                                                case _ => false
                                              }
                                              //logger.debug(s"userHasEntitlement will return: $result")
                                              result
                                            },
                                              userHasEntitlementRequest = {
                                              val result: Boolean = isLoggedIn match {
                                                case true =>
                                                {
                                                  val requestedRolesFound = userEntitlementRequests.map(_.roleName)
                                                  //logger.debug(s"requestedRolesFound are $requestedRolesFound")
                                                  // We only want to consider the user has requested the role if the role does not require bank id
                                                  // Otherwise users would find it difficult to request same role for different banks.
                                                  requestedRolesFound.contains(i.role) && ! i.requires_bank_id
                                                }
                                                case _ => false
                                              }
                                              //logger.debug(s"userHasEntitlementRequest will return: $result")
                                              result
                                            }
                                              )),
    isFeatured = r.is_featured,
    specialInstructions = stringToNodeSeq(r.special_instructions)
    )


    val filteredResources5: List[ResourceDocPlus] = nativeParam match {
      case Some(true) => {
        for {
          r <- allResources
          // apiVersion currently has an extra v which should be removed.
          if (r.implementedBy.version == apiVersion.stripPrefix("v")) // only show endpoints which have been implemented in this version.
        } yield {
          r
        }
      }
       case Some(false) => {
          for {
            r <- allResources
            // TODO apiVersion for OBP currently has an extra v which should be removed.
            if (r.implementedBy.version != apiVersion.stripPrefix("v")) // the opposite case
          } yield {
            r
          }
      }
      case _ => allResources // No preference (default) just return everything.
    }


    val resourcesToUse = filteredResources5.toSet.toList

    logger.debug(s"allResources count is ${allResources.length}")
    logger.debug(s"resourcesToUse count is ${resourcesToUse.length}")


    if (allResources.length > 0 && resourcesToUse.length == 0) {
      logger.info("tags filter reduced the list of resource docs to zero")
    }

    // Sort by the first and second tags (if any) then the summary.
    // In order to help sorting, the first tag in a call should be most general, then more specific etc.
    val resources = resourcesToUse.sortBy(r => {

      val firstTag = r.tags.headOption.getOrElse("")
      val secondTag = r.tags.take(1).toString()
      (firstTag, secondTag, r.summary.toString)
    })


    //this can be empty list, if there is no operationIds there.
    def webpageOperationIds = ObpAPI.getApiCollectionEndpointsById(apiCollectionId).map(_.api_collection_endpoints.map(_.operation_id)).openOr(List()).map(covertObpOperationIdToWebpageId)
    def myWebpageOperationIds = ObpAPI.getApiCollectionEndpoints("Favourites").map(_.api_collection_endpoints.map(_.operation_id)).openOr(List()).map(covertObpOperationIdToWebpageId)

    // Group resources by the first tag
    val unsortedGroupedResources: Map[String, List[ResourceDocPlus]] = resources.groupBy(_.tags.headOr("ToTag"))

    // Sort the groups by the Tag. Note in the rendering we sort the resources by summary.
    val groupedResources  = unsortedGroupedResources.toSeq.sortBy{ kv =>
      val tagName = kv._1
      // if tag name starts with blank character, replace blank with _, to let it order to the tail.
      tagName.replaceFirst("^\\s", "_")
    }


    // Featured /////
    val featuredResources = resources.filter(r => r.isFeatured == true)
    // Group resources by the first tag
    val unsortedFeaturedGroupedResources: Map[String, List[ResourceDocPlus]] = featuredResources.groupBy(_.tags.headOr("ToTag"))
    // Sort the groups by the Tag. Note in the rendering we sort the resources by summary.
    val groupedFeaturedResources  = unsortedFeaturedGroupedResources.toSeq.sortBy(_._1)
    /////////////////



    // Title / Headline we display including count of APIs
    val headline : String = s"""
      ${apiVersionRequested.stripPrefix("OBP").stripPrefix("BG").stripPrefix("STET").stripPrefix("UK")}
      $tagsHeadline $languageHeadline $contentHeadline $implementedHereHeadline (${resources.length} APIs)
      """.trim()



    logger.info (s"showingMessage is: $headline")


    // Used to show / hide the Views selector
    // TODO disable instead of hiding
    val displayViews = "block"

    //Only show the collections when the user logged In
    val displayCollectionsDiv = if (isLoggedIn) {
      "block"
    } else {
      "none"
    }

    val displayFeatured = if (featuredResources.length > 0 ) {
      logger.info("show featured")
      "block"
    } else {
      logger.info("not show featured")
      "none"
    }



    val showIndexObpApiManagementLink = Helper.getPropsValue("webui_show_index_obp_api_management_link", "true").toBoolean
    val showIndexObpUserManagementLink = Helper.getPropsValue("webui_show_index_obp_user_management_link", "true").toBoolean
    val showIndexAllObpLink = Helper.getPropsValue("webui_show_index_obp_all_link", "true").toBoolean
    val showIndexDynamicLink = Helper.getPropsValue("webui_show_index_dynamic_link", "true").toBoolean
    val showIndexMoreLink = Helper.getPropsValue("webui_show_index_more_link", "true").toBoolean
    val showIndexBerlinGroupLink = Helper.getPropsValue("webui_show_index_berlin_group_link", "false").toBoolean
    val showIndexUkLink = Helper.getPropsValue("webui_show_index_uk_link", "false").toBoolean
    val consentFlowLink = Helper.getPropsValue("consent_flow_link", "")
    
    val displayIndexObpApiManagementLink = if (showIndexObpApiManagementLink ) {""} else {"none"}
    val displayIndexObpUserManagementLink = if (showIndexObpUserManagementLink ) {""} else {"none"}
    val displayIndexAllObpLink = if (showIndexAllObpLink ) {""} else {"none"}
    val displayIndexDynamicLink = if (showIndexDynamicLink ) {""} else {"none"}
    val displayIndexMoreLink = if (showIndexMoreLink ) {""} else {"none"}
    val displayIndexBerlinGroupLink = if (showIndexBerlinGroupLink ) {""} else {"none"}
    val displayIndexUkLink = if (showIndexUkLink ) {""} else {"none"}
    val displayConsentFlowLink = if (consentFlowLink.nonEmpty ) {""} else {"none"}


    // Do we want to show the Request Entitlement button.
    // Should also consider if User has submitted an entitlement request or already has the role.
    val displayRequestEntitlementButton = if (isLoggedIn) {
      logger.info("show Request Entitlement button")
      "block"
    } else {
      logger.info("not show Request Entitlement button")
      "none"
    }




    // Controls when we display the request body.
    def displayRequestBody(resourceVerb : String) = {
      resourceVerb match {
        case "POST" => "block"
        case "PUT" => "block"
        case "PATCH" => "block"
        case _ => "none"
      }
    }




    //TODO, need error handling:
    val myApicollections: List[ApiCollectionJson400] = ObpAPI.getMyApiCollections.map(_.api_collections).getOrElse(List.empty[ApiCollectionJson400])


    def process(name: String): JsCmd = {


      // The DIVS in the DOM have underscores in their names.
      resourceId = resourceId.replace(".","_")

      logger.info(s"requestUrl is $requestUrl")
      logger.info(s"requestCustomHeader is $requestCustomHeader")
      logger.info(s"resourceId is $resourceId")
      logger.debug(s"requestBody is $requestBody")
      logger.debug(s"responseBody is $responseBody")
      logger.debug(s"errorResponseBodies is $errorResponseBodies")


      // Create json object from input string
      val jsonObject = JsonParser.parse(requestBody)
      val jsonResponseObject = JsonParser.parse(responseBody)
      val jsonErrorResponse = errorResponseBodies

      // the id of the element we want to populate and format.
      val resultTarget = "result_" + resourceId
      val boxTarget = "result_box_" + resourceId
      // This will highlight the json. Replace the $ sign after we've constructed the string
      val jsCommandHighlightResult : String =  s"DOLLAR_SIGN('#$boxTarget').fadeIn();DOLLAR_SIGN('#$resultTarget').each(function(i, block) { hljs.highlightBlock(block);});".replace("DOLLAR_SIGN","$")

      val jsEnabledSubmitBtn = s"jQuery('input[name=$name]').removeAttr('disabled')"

      val rolesTarget = "roles_" + resourceId
      val rolesboxTarget = "roles_box_" + resourceId

      //val jsCommandHighlightRolesResult : String =  s"DOLLAR_SIGN('#$rolesboxTarget').fadeIn();DOLLAR_SIGN('#$rolesTarget').each(function(i, block) { hljs.highlightBlock(block);});".replace("DOLLAR_SIGN","$")

      //jsCommandHighlightRolesResult.contains("afsf")

      // The id of the possible error responses box we want to hide after calling the API
      val possibleErrorResponsesBoxTarget = "possible_error_responses_box_" + resourceId

      // The id of the possible error responses box we want to hide after calling the API
      val connectorMethodsBoxTarget = "connector_methods_box_" + resourceId

      // The id of the roles responses box we want to hide after calling the API
      val requestRolesResponsesBoxTarget = "required_roles_response_box_" + resourceId
      // The javascript to hide it.
      val jsCommandHidePossibleErrorResponsesBox : String =  s"DOLLAR_SIGN('#$possibleErrorResponsesBoxTarget').fadeOut();".replace("DOLLAR_SIGN","$")
      val jsCommandHideConnectorMethodsBoxTarget : String =  s"DOLLAR_SIGN('#$connectorMethodsBoxTarget').show();".replace("DOLLAR_SIGN","$")
      val jsCommandHideRequestRolesResponsesBox : String =  s"DOLLAR_SIGN('#$requestRolesResponsesBoxTarget').fadeOut();".replace("DOLLAR_SIGN","$")

      // The id of the possible error responses box we want to hide after calling the API
      val typicalSuccessResponseBoxTarget = "typical_success_response_box_" + resourceId
      // The javascript to hide it.
      val jsCommandHideTypicalSuccessResponseBox : String =  s"DOLLAR_SIGN('#$typicalSuccessResponseBoxTarget').fadeOut();".replace("DOLLAR_SIGN","$")

      // The id of the full path
      val fullPathTarget = "full_path_" + resourceId
      val fullHeadersBox= "full_headers_box_"+resourceId
      val fullHeadersTarget = "full_headers_" + resourceId
      // The javascript to show it
      val jsCommandShowFullPath : String =  s"DOLLAR_SIGN('#$fullPathTarget').fadeIn();".replace("DOLLAR_SIGN","$")
      val jsCommandShowFullHeaders : String =
        s"DOLLAR_SIGN('#$fullHeadersBox').show();".replace("DOLLAR_SIGN","$") ++s"DOLLAR_SIGN('#$fullHeadersTarget').fadeIn();".replace("DOLLAR_SIGN","$")

      // alert('$fullPathTarget');
      //logger.info(s"jsCommand is $jsCommand")
      //logger.info(s"jsCommand2 is $jsCommandHidePossibleErrorResponsesBox")


      /////////////
      // TODO It would be nice to modify getResponse and underlying functions to return more information about the request including full path
      // For now we duplicate the construction of the fullPath
      val apiUrl = OAuthClient.currentApiBaseUrl

      val urlWithVersion =
        if (tagsParamString.equalsIgnoreCase("PSD2") == Some(true)) {
          s"$requestUrl".split("\\?").toList match {
            case url :: params :: Nil =>
              url + params + "&format=ISO20022"
            case url :: Nil =>
              url + "?format=ISO20022"
            case _ =>
              s"$requestUrl"
          }
        } else {
          s"$requestUrl"
        }
      logger.info(s"urlWithVersion is: " + urlWithVersion
        .replaceAll(UKVersionV20, "v2.0")
        .replaceAll(UKVersionV31, "v3.1")
        .replaceAll(BGVersionV133, VersionV133)
        .replaceAll("BGv1", "v1")
        .replaceAll(BGVersionV13, "v1.3")
        .replaceAll("(?<![Vv]validations/)OBPv", "") //delete OBPv, but if the OBPv is part of operationId, not to do delete, e.g: /validations/OBPv4.0.0-dynamicEndpoint_POST__account_access_consents
      )

      //val urlWithVersion = s"/$apiVersion$requestUrl"
      val fullPath = new URL(apiUrl + urlWithVersion
        .replaceAll(UKVersionV20, "v2.0")
        .replaceAll(UKVersionV31, "v3.1")
        .replaceAll(BGVersionV133, VersionV133)
        .replaceAll(BGVersionV13, "v1.3")
        .replaceAll("BGv1", "v1")
        .replaceAll("(?<![Vv]validations/)OBPv", "")) //delete OBPv, but if the OBPv is part of operationId, not to do delete, e.g: /validations/OBPv4.0.0-dynamicEndpoint_POST__account_access_consents
      //////////////

      val (body, headers) = getResponse(requestUrl, requestVerb, jsonObject, customRequestHeader = requestCustomHeader)
      // Return the commands to call the url with optional body and put the response into the appropriate result div
      SetHtml(resultTarget, Text(body)) &
     // SetHtml(rolesTarget, Text(responseRoleString)) &
      Run (jsCommandHighlightResult) &
      //Run (jsCommandHighlightRolesResult) &
      Run (jsCommandHidePossibleErrorResponsesBox) &
      Run (jsCommandHideConnectorMethodsBoxTarget) &
      Run (jsCommandHideRequestRolesResponsesBox) &
      Run (jsCommandHideTypicalSuccessResponseBox) &
      Run (jsCommandShowFullPath) &
      Run (jsCommandShowFullHeaders) &
      Run (jsEnabledSubmitBtn) &
      SetHtml(fullPathTarget, Text(fullPath.toString)) &
      SetHtml(fullHeadersTarget, Text(headers))
    }

    def processFavourites(name: String): JsCmd = {
      // enable button
      val jsEnabledBtn = s"jQuery('input[name=$name]').removeAttr('disabled')"
      //We call the getApiCollectionsForCurrentUser endpoint again, to make sure we already created or delete the record there.
      val apiFavouriteCollection = ObpAPI.getApiCollection("Favourites")
      val errorMessage = if(apiFavouriteCollection.isInstanceOf[Failure]) apiFavouriteCollection.asInstanceOf[Failure].messageChain else ""

      
      if(apiFavouriteCollection.isInstanceOf[Failure]){ // If the user is not logged in, we do not need call any apis calls. (performance enhancement)
        SetHtml(s"favourites_error_message_${favouritesOperationIdFromWebpage}", Text(errorMessage))&
          Run (jsEnabledBtn)
      } else {
        if(errorMessage.equals("")){ //If there is no error, we changed the button
          if(favouritesApiCollectionId.nonEmpty && !apiFavouriteCollection.map(_.api_collection_id).contains(favouritesApiCollectionId)){
            SetHtml(s"favourites_error_message_${favouritesOperationIdFromWebpage}", Text("You only have read access for the Favourites. You can only edit your own Favourites."))&
              Run (jsEnabledBtn)
          }else{
            if (myWebpageOperationIds.contains(favouritesOperationIdFromWebpage)) { //If we already have this operationId, we need to delete it
              val deletedBox = ObpAPI.deleteMyApiCollectionEndpoint("Favourites",favouritesOperationIdFromWebpage)
              val deleteErrorMessage = if(deletedBox.isInstanceOf[Failure]) deletedBox.asInstanceOf[Failure].messageChain else ""
              if (deletedBox.isInstanceOf[Failure]){
                SetHtml(s"favourites_error_message_${favouritesOperationIdFromWebpage}", Text(deleteErrorMessage)) &
                  Run(jsEnabledBtn)
              }else{
                Run (jsEnabledBtn) & Run(s"jQuery('#favourites_button_${favouritesOperationIdFromWebpage}').css('color','#767676')")
              }
            } else {//If we do not have this operationId, we need to create it.
              val createdBox = ObpAPI.createMyApiCollectionEndpoint("Favourites",favouritesOperationIdFromWebpage)
              val createdErrorMessage = if(createdBox.isInstanceOf[Failure]) createdBox.asInstanceOf[Failure].messageChain else ""
              if (createdBox.isInstanceOf[Failure]){
                SetHtml(s"favourites_error_message_${favouritesOperationIdFromWebpage}", Text(createdErrorMessage)) &
                  Run(jsEnabledBtn)
              }else{
                Run (jsEnabledBtn) & Run(s"jQuery('#favourites_button_${favouritesOperationIdFromWebpage}').css('color','#53C4EF')")
              }
            }
        }} else { //if there is error, we show the OBP-API error there.
          SetHtml(s"favourites_error_message_${favouritesOperationIdFromWebpage}", Text(errorMessage)) &
            Run(jsEnabledBtn)
        }
      }
    }


    val thisApplicationUrl = s"${CurrentReq.value.uri}?version=${apiVersionRequested}&list-all-banks=${listAllBanks}${tagsParamString}${languagesParamString}${contentParamString}${apiCollectionIdParamString}"


    val obpVersionUrls: List[(String, String)] = obpVersionsSupported.map(i => (i.replace("OBPv", "v"), s"?version=${i}&list-all-banks=${listAllBanks}"))



    // Create a list of (version, url) used to populate the versions whilst preserving the other parameters
    // Includes hack for Berlin Group
    val otherVersionUrls: List[(String, String)] = otherVersionsSupported.map(i => (i
      .replace("b1", "API Builder")
      .replace(BGVersionV133, "Berlin Group 1.3.3")
      .replace(BGVersionV13, "Berlin Group 1.3")
      .replace("BGv1", "Berlin Group")
      .replace(UKVersionV20, "UK 2.0")
      .replace(UKVersionV31, "UK 3.1")
      .replace("STETv1.4", "STET 1.4")
      .replace(PAPIVersionV2111, "Polish API 2.1.1.1")
      .replace("AUv1.0.0", "AU CDR v1.0.0"),
      s"${CurrentReq.value.uri}?version=${i}&list-all-banks=${listAllBanks}"))

    //TODO this need to be a method,
    val otherVersionsSupportedInDropdownMenuUrls: List[(String, String)] = allSupportedVersionsInDropdownMenu.map(i => (i
      .replace("b1", "API Builder")
      .replace("STETv", "STET ")
      .replace("PAPIv", "Polish API ")
      .replace("AUv", "AU CDR ")
      .replace("OBPv", "OBP ")
      .replace("UKv", "UK ")
      .replace("BGv", "Berlin Group "), // replace v with space
      s"${CurrentReq.value.uri}?version=${i}&list-all-banks=${listAllBanks}"))


    // So we can highlight (or maybe later exclusively show) the "active" banks in a sandbox.
    // Filter out empty string items
    val featuredBankIds = Helper.getPropsValue("featuredBankIds", "").split(",").toList.filter(i => i.length > 0)



    val banks = for {
      a <- allBanks.toList
      b <- a.bankJsons
    } yield Bank (b.id.get,
                  b.short_name.getOrElse(""),
                  b.full_name.getOrElse(""),
                  b.logo.getOrElse(""),
                  b.website.getOrElse(""),
                  featuredBankIds.contains(b.id.get) // Add a flag to say if this bank is featured.
      )

    // Banks where a user has accounts + My Spaces + Featured Banks.
    val userCanShowBankIds= (myBankIds++featuredBankIds.map(BankId(_))++ getMySpaces.map(_.bank_ids.map(BankId(_))).getOrElse(List.empty[BankId])).toSet
    
    val banksForUser =
      if (listAllBanks) // Url param says show all.
        banks
      else
        if(!userCanShowBankIds.isEmpty) // User has accounts so show those banks
          banks.filter(b => userCanShowBankIds.contains(BankId(b.id)))
        else
          // If we have a featured list of banks show those, else all.
          banks.filter(b => b.isFeatured || featuredBankIds.length == 0)


    def highlightFeatured(value: Boolean) : String = if (value) " *" else ""


    // Format and Sort banks.
    val bankOptions = banksForUser.map(b => (b.id, b.shortName + " ("  + b.id + ")" + highlightFeatured(b.isFeatured) )).sortBy(a => (a._2, a._1))


    val selectBank = ("", "Select Bank")
    val selectBankOptions = selectBank :: bankOptions




    def onBankChange (v: Any) = {
      logger.info("bank changed to " + v.toString)
      S.redirectTo(s"$thisApplicationUrl&bank_id=${v}")
    }

    def onAccountChange (v: Any) = {
      logger.info("account changed to " + v.toString)
      S.redirectTo(s"$thisApplicationUrl&bank_id=${presetBankId}&account_id=${v}")
    }

    def onViewChange (v: Any) = {
      logger.info("view changed to " + v.toString)
      S.redirectTo(s"$thisApplicationUrl&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${v}")
    }

    def onCounterpartyChange (v: Any) = {
      logger.info("counterparty changed to " + v.toString)
      S.redirectTo(s"$thisApplicationUrl&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${presetViewId}&counterparty_id=${v}")
    }

    def onTransactionChange (v: Any) = {
      logger.info("transaction changed to " + v.toString)
      S.redirectTo(s"$thisApplicationUrl&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${presetViewId}&counterparty_id=${presetCounterpartyId}&transaction_id=${v}")
    }









    // TODO create BankId case class like in the API
    type BankID = String

    val privateAccountJsons : List[(String, String)] = for {
      privateAccountsJson <- ObpAPI.allAccountsAtOneBank(presetBankId).toList
      barebonesAccountJson <- privateAccountsJson.accounts.toList.flatten
      //bankId <- barebonesAccountJson.bank_id
      accountId <- barebonesAccountJson.id
      label <- barebonesAccountJson.label
    } yield (accountId, label)

    def getAccountOptions : List[(String,String)] = {

      val selectAccount = ("", "Select Account")
      val noneFound = ("", "") // No Accounts Found

      val options: List[(String, String)] = presetBankId match {
        case "" => List(noneFound)
        case _ => for {
          allAccountsJson <- OAuthClient.loggedIn match {
            case true => ObpAPI.privateAccounts(presetBankId).toList
            case false => ObpAPI.publicAccounts(presetBankId).toList
          }
          barebonesAccountJson <- allAccountsJson.accounts.toList.flatten
          accountId <- barebonesAccountJson.id
          // Show the label if it exists, else the id
          // TODO show the AccountJson.number (if owner) in preference to the ID.
          label = barebonesAccountJson.label.getOrElse(barebonesAccountJson.id.getOrElse("NO ID"))
        } yield (accountId, label)
      }

      selectAccount :: options.sortBy(i => (i._2, i._1)) // Sort by the field the user sees
    }

    def getViewOptions : List[(String,String)] = {

      val selectOne = OAuthClient.loggedIn match {
        case true => ("", "Select View")
        case false => ("", "Login for Views")
      }

      val noneFound = ("", "") // No Views Found

      // TODO Should check for both presetBankId and presetAccountId
      // Logged in user required?
      val options: List[(String, String)] = presetAccountId match {
        case "" => List(noneFound)
        case _ => for {
          views <- ObpAPI.getViewsForBankAccount(presetBankId, presetAccountId).toList
          view <- views.views.toList.flatten
          viewId <- view.id
          shortName <- view.short_name
        } yield (viewId, shortName)
      }

      selectOne :: options.sortBy(i => (i._2, i._1)) // Sort by the field the user sees
    }


    def getCounterpartyOptions : List[(String,String)] = {

      val selectOne = OAuthClient.loggedIn match {
        case true => ("", "Select Counterparty")
        case false => ("", "Login for CPs")
      }
      val noneFound = ("", "") // No Counterparties Found

      // TODO Should check for both presetBankId and presetAccountId
      val options: List[(String, String)] = presetViewId match {
        case "" => List(noneFound)
        case _ =>{
          val implicitCounterparties = for {
            counterpartiesJson <- ObpAPI.getCounterparties(presetBankId, presetAccountId, presetViewId).toList
            counterparty <- counterpartiesJson.other_accounts
          } yield (counterparty.id, counterparty.holder.name)

          val explictCounterparties = for {
            counterpartiesJson <- ObpAPI.getExplictCounterparties(presetBankId, presetAccountId, presetViewId).toList
            counterparty <- counterpartiesJson.counterparties
          } yield (counterparty.counterparty_id, counterparty.name)

          implicitCounterparties ++ explictCounterparties
        }
      }

      selectOne :: options.sortBy(i => (i._2, i._1)) // Sort by the field the user sees
    }



    def getTransactionOptions : List[(String,String)] = {

      val selectOne = OAuthClient.loggedIn match {
        case true => ("", "Select Transaction")
        case false => ("", "Login for Trans")
      }
      val noneFound = ("", "") // No Transactions Found

      // TODO Should check for both presetBankId and presetAccountId
      val options: List[(String, String)] = presetViewId match {
        case "" => List(noneFound)
        case _ => for {
          transactionsJson <- ObpAPI.transactions300(presetBankId, presetAccountId, presetViewId, None,None,None,None,None).toList
          transaction <- transactionsJson.transactions
        } yield (transaction.id, s"${transaction.other_account.holder.name} ${transaction.details.value.amount}")
      }

      selectOne :: options.sortBy(i => (i._2, i._1)) // Sort by the field the user sees
    }




    // Drop down box to select bank. Selected item taken from url param.
    def doBankSelect(in: NodeSeq) = ajaxSelect(selectBankOptions,
      Full(presetBankId),
      v => onBankChange(v))

    // Drop down box to select account. Selected item taken from url param.
    def doAccountSelect(in: NodeSeq) = ajaxSelect(getAccountOptions,
      Full(presetAccountId),
      v => onAccountChange(v))

    // Drop down box to select view for bank/account. Selected item taken from url param.
    def doViewSelect(in: NodeSeq) = ajaxSelect(getViewOptions,
      Full(presetViewId),
      v => onViewChange(v))

    // Drop down box to select view for bank/account. Selected item taken from url param.
    def doCounterpartySelect(in: NodeSeq) = ajaxSelect(getCounterpartyOptions,
      Full(presetCounterpartyId),
      v => onCounterpartyChange(v))


    // Drop down box to select transaction for bank/account. Selected item taken from url param.
    def doTransactionSelect(in: NodeSeq) = ajaxSelect(getTransactionOptions,
      Full(presetTransactionId),
      v => onTransactionChange(v))



    def loggedInStatusMessage = {
      if (OAuthClient.loggedIn) "" else "Some options and calls require login."
    }


    /*

          "#register-consumer-input" #> "" & {
        val hasDummyUsers = getWebUiPropsValue("webui_dummy_user_logins", "").nonEmpty
        val isShowDummyUserTokens = getWebUiPropsValue("webui_show_dummy_user_tokens", "false").toBoolean
        if(hasDummyUsers && isShowDummyUserTokens) {
          "#create-directlogin a [href]" #> s"dummy-user-tokens?consumer_key=${consumer.key.get}"
        } else {
          "#dummy-user-tokens" #> ""
        }
      }



    */



    /**
     * If it is the Favourates Resource Docs, we need to show all the resourceDocs.
     * we need to skip the case: &api-collection-id=&
     */
    def isCollectionOfResourceDocs_? = {
      S.param(ApiCollectionId).isDefined && S.param(ApiCollectionId).getOrElse("").nonEmpty
    }
    val glossaryItems = getGlossaryItemsJson.map(_.glossary_items).getOrElse(List())

    val cssResult = "#login_status_message" #> loggedInStatusMessage &
    "#bank_selector" #> doBankSelect _ &
    "#account_selector" #> doAccountSelect _ &
    "#view_selector" #> doViewSelect _ &
    "#counterparty_selector" #> doCounterpartySelect _ &
    "#transaction_selector" #> doTransactionSelect _ &
    //
    //
    // Below we render the resources into a (nested) table.
    // Notes on escaping strings:
    // To have a $ in the resulting string use two: $$
    // Can't escape " with \" or use triple quoted string in the string interpolation so may need to use the replace hack
    //
    // Note: This is the return of the function.
    // All the replacements you want to do *must be chained here together at the end of the function*.
    // Also, you can't use "replaceWith" (the alias for #>) to chain
    //
    // See the following for some examples.
    // http://blog.knoldus.com/2013/03/08/lift-web-basics-of-using-snippets-for-a-beginner/
    // http://simply.liftweb.net/index-7.10.html
    //
    // Show the version to the user.
    // Append to the content child of id="version" e.g. the fixed text "Version:" is replacedWith "Version: 1.2.3"
    shownVersions() &
    "#version *+" #> apiVersion &
    "@obp_versions" #> obpVersionUrls.map { i =>
      "@obp_version *" #> s"OBP ${i._1} " &
      "@obp_version [href]" #> s"${i._2}"
    } &
      "@other_versions" #> otherVersionUrls.map { i =>
        "@other_version *" #> s" ${i._1} " &
          "@other_version [href]" #> s"${i._2}"
      } &
      "@custom_api_collections" #> ObpAPI.getApiCollectionsFromProps.openOr(Nil).map { i =>
        ".version *" #> s"${i._1}" &
          ".version [href]" #> s"${i._2}"
      } &
      "@dropdown_versions" #> otherVersionsSupportedInDropdownMenuUrls.map { i =>
        ".dropdown-item *" #> s" ${i._1} " &
          ".dropdown-item [href]" #> s"${i._2}"
      } &
      "@dropdown_space" #> getMySpaces.map(_.bank_ids).getOrElse(Nil).map { i =>
        ".dropdown-item *" #> s" $i " &
          ".dropdown-item [href]" #> s"?space_bank_id=${i}"
      } &
    ".info-box__headline *" #> s"$headline"  &
    "@version_path *" #> s"$baseVersionUrl" &
    "@version_path [href]" #> s"$baseVersionUrl" &
    "@resource_docs_path [href]" #> s"$resourceDocsPath" &
    "@swagger_path [href]" #> s"$swaggerPath" &
    "@git_commit [href]" #> s"https://github.com/OpenBankProject/API-Explorer/commit/$currentGitCommit" &
    "@chinese_version_path [href]" #> s"$chineseVersionPath" &
    "@all_partial_functions [href]" #> s"$allPartialFunctions" &
    "#api_creation_and_management_link [href]" #> s"./?tags=$apiCreationAndManagementTags" &
    "#api_creation_and_management_link_div [style]"  #> s"display: $displayIndexObpApiManagementLink;" &
    "#dynamic_link_div [style]"  #> s"display: $displayIndexDynamicLink;" &
    "#user_management_link [href]" #> s"./?tags=$userManagementTags" &
    "#user_management_link_div [style]"  #> s"display: $displayIndexObpUserManagementLink;" &
    "#obp_banking_model_link [href]" #> s"./?tags=$obpBankingModelTags" &
    "#all_obp_link_div [style]"  #> s"display: $displayIndexAllObpLink;" &
    "#berlin_group_link_div [style]"  #> s"display: $displayIndexBerlinGroupLink;" &
    "#uk_link_div [style]"  #> s"display: $displayIndexUkLink;" &
    "#More [style]"  #> s"display: $displayIndexMoreLink;" &
    "#onboard_link [href]" #> s"$apiPortalHostname/user_mgt/sign_up?after-signup=link-to-customer" &
    "#consent_flow_link_div [style]" #> s"display: $displayConsentFlowLink;" &
    "#consent_flow_link [href]" #> s"$consentFlowLink" & 
    "#api_home_link [href]" #> s"$apiPortalHostname" &
    "#api_manager_link [href]" #> s"$apiManagerUrl" &
    "@views_box [style]" #> s"display: $displayViews;" &
    "@favourites_group_item [style]" #> s"display: $displayCollectionsDiv;" &
    // Show / hide featured
    "@featured_box [style]" #> s"display: $displayFeatured;" &
      "@featured_list [style]" #> s"display: $displayFeatured;" &
      // List the Featured resources grouped by the first tag
      "@featured_api_group_item" #> groupedFeaturedResources.map { i =>
        "@featured_api_group_name *" #> s"${i._1.replace("-"," ")}" &
          // Within each group (first tag), list the resources
          "@featured_api_list_item" #> i._2.sortBy(_.summary.toString()).map { i =>
            // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
            "@featured_api_list_item_link [href]" #> s"#${i.id}" &
              "@featured_api_list_item_link *" #> i.summary &
              "@featured_api_list_item_link [id]" #> s"index_of_${i.id}"
          }
      } &
      // List the resources grouped by the first tag
      "@favourites_list_item" #> myApicollections.map { i =>
        "@favourites_list_item_link [href]" #> s"?api-collection-id=${i.api_collection_id}" &
         "@favourites_list_item_link *" #> i.api_collection_name &
         "@favourites_list_item_link [id]" #> s"index_of_${i.api_collection_name}"
      } &
    // List the resources grouped by the first tag
      "@api_group_item" #> groupedResources.map { i =>
          "@api_group_name *" #> s"${i._1.replace("-"," ")}" &
            // Set an anchor (href and id) for a group
            "@api_group_name [href]" #> s"#group-${i._1}" &
            "@api_group_name [id]" #> s"group-${i._1}" &
            // Set an anchor (href and id) for a group
            "@api_glossary_item_link [href]" #> s"/glossary?#${i._1}" &
            "@api_glossary_item_link [id]" #> s"glossary-${i._1}" &
            "@api_glossary_item_link * " #>{
              val description = glossaryItems.find(_.title == i._1.replaceAll("-"," ")).map(_.description.markdown).getOrElse("")
              if (description.length > 100)
                "More..."
              else
                "" //If there is no description, we will show empty here.
            } &
            "@api_glossary_item_text * " #>{
              val description = glossaryItems.find(_.title == i._1.replaceAll("-"," ")).map(_.description.markdown).getOrElse("")
              if (description.length > 100)
                description.substring(0,100).trim()
              else
                "" //If there is no description, we will show empty here.
            } &
            // Within each group (first tag), list the resources
            "@api_list_item" #> i._2.sortBy(_.summary.toString()).map { i =>
              // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
                "@api_list_item_link [href]" #>
                  (if (rawTagsParam.isDefined && !rawTagsParam.getOrElse("").isEmpty) //If the tags are in the URL, we just need to show the anchor, no need the parameters.
                    s"#${i.id}"
                  else if (isCollectionOfResourceDocs_?) 
                    s"#${i.id}"
                  else if (resources.find(_.id == currentOperationId).map(_.tags.headOption.getOrElse("API"))==resources.find(_.id == i.id).map(_.tags.headOption.getOrElse("API"))) //If the Tag is the current Tag.We do not need parameters.
                    s"#${i.id}"
                  else {
                    def urlParams = {
                      val verionRequestedParam = if (apiVersionRequested == "" ) "?" else s"?version=$apiVersionRequested"
                      val operationIdParam = if (i.id == "" ) "" else s"&operation_id=${i.id}"
                      val currentTagParam = if (i.tags.head == "" ) "" else s"&currentTag=${i.tags.head}"
                      val bankIdParam = if (presetBankId == "" ) "" else s"&bank_id=${presetBankId}"
                      val accountIdParam = if (presetAccountId == "" ) "" else s"&account_id=${presetAccountId}"
                      val viewIdParam = if (presetViewId == "" ) "" else s"&view_id=${presetViewId}"
                      val counterpartyIdParam = if (presetCounterpartyId == "" ) "" else s"&counterparty_id=${presetCounterpartyId}"
                      val transactionIdParam = if (presetTransactionId == "" ) "" else s"&transaction_id=${presetTransactionId}"
                      s"$verionRequestedParam$operationIdParam$currentTagParam$apiCollectionIdParamString$bankIdParam$accountIdParam$viewIdParam$counterpartyIdParam$transactionIdParam"
                    }
                    s"$urlParams#${i.id}"
                  }) &
                  "@api_list_item_link [style]" #>
                  (if (i.id == currentOperationId && queryString.isDefined)
                    s"font-weight: bold;"
                  else
                    s"font-weight: normal;") &
                  "@api_list_item_link *" #> i.summary &
                  "@api_list_item_link [id]" #> s"index_of_${i.id}"
                  // ".content-box__available-since *" #> s"Implmented in ${i.implementedBy.version} by ${i.implementedBy.function}"
        }
      } &
      // The `api_group_item_small_screen` is all for the small screen,
      "@api_group_item_small_screen" #> groupedResources.map { i =>
        "@api_group_item_small_screen [data-target]" #> s"#group-collapse_small_screen-${i._1.replaceAll(" ","_").replaceAll("""\(""","_").replaceAll("""\)""","")}" &
          "@api_group_name_small_screen *" #> s"${i._1.replace("-"," ")}" &
          "@api_group_name_collapse_small_screen [id]" #> s"group-collapse_small_screen-${i._1.replaceAll(" ","_").replaceAll("""\(""","_").replaceAll("""\)""","")}" &
          // Set an anchor (href and id) for a group
//          "@api_group_name_small_screen [href]" #> s"#group_small_screen-${i._1}" &
          "@api_group_name_small_screen [id]" #> s"group_small_screen-${i._1.replaceAll(" ","_").replaceAll("""\(""","_").replaceAll("""\)""","")}" &
          // Within each group (first tag), list the resources
          "@api_list_item_small_screen" #> i._2.sortBy(_.summary.toString()).map { i =>
            // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
            "@api_list_item_link_small_screen [href]" #>
              (if (rawTagsParam.isDefined && !rawTagsParam.getOrElse("").isEmpty) //If the tags are in the URL, we just need to show the anchor, no need the parameters.
                s"#${i.id}"
              else if (isCollectionOfResourceDocs_?) 
                s"#${i.id}"
              else if (resources.find(_.id == currentOperationId).map(_.tags.headOption.getOrElse("API"))==resources.find(_.id == i.id).map(_.tags.headOption.getOrElse("API"))) //If the Tag is the current Tag.We do not need parameters.
                s"#${i.id}"
              else
                s"?operation_id=${i.id}&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${presetViewId}&counterparty_id=${presetCounterpartyId}&transaction_id=${presetTransactionId}#${i.id}") &
              "@api_list_item_link_small_screen span" #> i.summary &
              "@api_list_item_link_small_screen [id]" #> s"index_of__small_screen${i.id}"
          }
      } &
    // This is used by API administrators who want to create white or black lists of API calls to use in Props for the API.
//    ".comma_separated_api_call_list *" #> commaSeparatedListOfResources &
      // replace the node identified by the class "resource" with the following
      // This creates the list of resources in the DOM
    {
      if(allResourcesBox.isInstanceOf[Failure]) {
      ".resource [style]" #> s"display: none" &
        ".resource-error [style]" #> s"display: block" &
        ".content-box__headline *" #> {
          allResourcesBox.asInstanceOf[Failure].msg
        }&
        {
          if(allResourcesBox.asInstanceOf[Failure].msg.contains("CanReadResourceDoc")){
            //required roles and related user information
            "@roles_box [id]" #> s"roles_box_canReadResourceDocRoleInfo" &
              "@roles_box [style]" #> {s"display: block"} &
              // We generate mulutiple .role_items from roleInfos (including the form defined in index.html)
              ".role_item" #> canReadResourceDocRoleInfo.map { r =>
                "@roles__status" #> {if (! isLoggedIn)
                  s" - Please login to request this Role"
                else if  (r.userHasEntitlement)
                  s" - You have this Role."
                else if (r.userHasEntitlementRequest)
                  s" - You have requested this Role. Please contact Open Bank Project team to grant your this role."
                else
                  s" - You can request this Role."} &
                  "@roles__role_name" #> s"${r.role}" &
                  // ajaxSubmit will submit the form.
                  // The value of rolesBankId is given to bank_id_input field and the value of bank_id_input entered by user is given back to rolesBankId
                  "@roles__bank_id_input" #> SHtml.text({if (r.requiresBankId) rolesBankId else ""}, rolesBankId = _, if (r.requiresBankId) "type" -> "text" else "type" -> "hidden") &
                  "@roles__role_input" #> SHtml.text(s"${r.role}", entitlementRequestRoleName = _, "type" -> "hidden" ) &
                  "@roles__resource_id_input" #> text("canReadResourceDocRoleInfo", s => RolesResourceId = s, "type" -> "hidden", "id" -> s"roles__resource_id_input_${canReadResourceDocRoleInfo}") &
                  "@roles__request_entitlement_button" #> Helper.ajaxSubmit("Request", disabledBtn, processEntitlementRequest) &
                  "@roles__entitlement_request_response [id]" #> s"roles__entitlement_request_response_${canReadResourceDocRoleInfo}_${r.role}" &
                  "@roles__entitlement_request_button_box [style]" #> { if (! isLoggedIn || r.userHasEntitlement || r.userHasEntitlementRequest)
                    s"display: none"
                  else
                    s"display: block"
                  }
              }
          } else{
            "@roles_box [style]" #> s"display: none"
            }
        }
      }else if(allResourcesBox.isEmpty || allResourcesBox.openOr(Nil).length ==0){
        ".resource [style]" #> s"display: none" &
          ".resource-error [style]" #> s"display: block" &
          ".content-box__headline *" #> {
            "Sorry, we could not return any Resource Docs."
          }&
          ".content-box__info-box [style]" #> s"display: none"
      }
      else {
        //The default tag is the first tag of the resource, if it is empty, we use the API Tag.
        val theResourcesFirstTag = resources.map(_.tags.headOption).flatten.headOption.getOrElse("API")
        val currentTag = resources.find(_.id == currentOperationId).map(_.tags.headOption.getOrElse("API")).getOrElse(theResourcesFirstTag)
        val authenticationTypeValidations: Box[Map[String, List[String]]] = getAuthenticationTypeValidations()
        val jsonSchemaValidations: Box[Map[String, json.JObject]] = getJsonSchemaValidations()

        ".resource" #> (if (rawTagsParam.isDefined && !rawTagsParam.getOrElse("").isEmpty)  resources else {
          //For the Favourites collections, we will show all the resource in the page, ignore the perfermance at the moment.
          val resourcesShowedInPage = if(isCollectionOfResourceDocs_?) resources else resources.filter(_.tags.headOption.getOrElse("API") == currentTag)
          resourcesShowedInPage
        }).map { i =>
          // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
          ".end-point-anchor [href]" #> s"#${i.id}" &
          ".content-box__headline *" #> i.summary &
          ".content-box__headline [id]" #> i.id & // id for the anchor to find
          // Replace attribute named overview_text with the value (whole div/span element is replaced leaving just the text)
          "@description *" #> i.description &
          "@special_instructions *" #> i.specialInstructions &
          "@resource_description [id]" #> s"description_${i.id}" &
          ".url_caller [id]" #> s"url_caller_${i.id}" &
          "@result [id]" #> s"result_${i.id}" &
          "@result_box [id]" #> s"result_box_${i.id}" &
          "@example_request_body [id]" #> s"example_request_body_${i.id}" &
          "@example_request_body [style]" #> s"display: ${displayRequestBody(i.verb)};" &
          //////
          // The form field (on the left) is bound to the variable (requestUrl)
          // (However, updating the var here does not seem to update the form field value)
          // We provide a default value (i.url) and bind the user input to requestUrl. requestURL is available in the function process
          // text creates a text box and we can capture its input in requestUrl
          "@request_url_input" #> text(i.url, s => requestUrl = s, "aria-label"->s"${i.summary}","maxlength" -> "512", "size" -> "100", "id" -> s"request_url_input_${i.id}") &
          "@request_header_input" #> text("", s => requestCustomHeader = s, "maxlength" -> "2048", "size" -> "100", "id" -> s"request_header_input${i.id}") &
          "@full_path [id]" #> s"full_path_${i.id}" &
          "#full_headers_box [id]" #> s"full_headers_box_${i.id}" &
          "@full_headers [id]" #> s"full_headers_${i.id}" &
          // Extraction.decompose creates json representation of JObject.
          "@example_request_body_input" #> text(Helper.renderJson(i.exampleRequestBody), s => requestBody = s, "maxlength" -> "100000", "size" -> "100", "type" -> "text") &
          //"@request_body_input" #> textarea((""), s => requestBody = s, "cols" -> "1000", "rows" -> "10","style"->"border:none") &
          //
          // Typical Success Response
          "@typical_success_response_box [id]" #> s"typical_success_response_box_${i.id}" &
          //"@typical_success_response [id]" #> s"typical_success_response_${i.id}" &
          "@typical_success_response *" #> Helper.renderJson(i.successResponseBody) & {
            // Possible Validations
            if(jsonSchemaValidations.isDefined || authenticationTypeValidations.isDefined) {
              val cssSelSchemaValidationRole = if (jsonSchemaValidations.isDefined) {
                ".required_json_validation" #>
                  (if (jsonSchemaValidations.exists(_.contains(i.operationId))) "Yes" else "No")
              } else {
                ".required_json_validation" #> "Invisible to anonymous users"
              }

              val cssSelAuthTypeValidationRole = if (authenticationTypeValidations.isDefined) {
                ".allowed_authentication_types" #>
                  authenticationTypeValidations.flatMap(_.get(i.operationId)).map(_.mkString("[", ", ", "]")).openOr("Not set")
              } else {
                ".allowed_authentication_types" #> "Invisible to anonymous users"
              }

              cssSelSchemaValidationRole & cssSelAuthTypeValidationRole
            } else {
              "@possible_validations_box" #> ""
            }
          } &
          // Possible Errors
          "@possible_error_responses_box [id]" #> s"possible_error_responses_box_${i.id}" &
          // This class gets a list of several possible error response items
          ".possible_error_item" #> i.errorResponseBodies.map { i =>
              ".possible_error_item *" #> i
          } &
          "#connector_methods_box [id]" #> s"connector_methods_box_${i.id}" &
          // This class gets a list of connector methods
          ".connector_method_item" #> i.connectorMethods.map { i=>
            // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
            ".connector_method_item_link [href]" #> s"message-docs?connector=stored_procedure_vDec2019#${urlEncode(i.replaceAll(" ", "-"))}" &
              ".connector_method_item_link *" #> i
          } &
          //required roles and related user information
          "@roles_box [id]" #> s"roles_box_${i.id}" &
          "@roles_box [style]" #> { if (i.roleInfos.isEmpty)
              s"display: none"
            else
              s"display: block"
            } &
          // We generate multiple .role_items from roleInfos (including the form defined in index.html)
          ".role_item" #> i.roleInfos.map { r =>
            "@roles__status" #> {if (! isLoggedIn)
                                  s" - Please login to request this Role"
                                else if  (r.userHasEntitlement)
                                  s" - You have this Role."
                                else if (r.userHasEntitlementRequest)
                                  s" - You have requested this Role. Please contact Open Bank Project team to grant your this role."
                                else
                                  s" - You can request this Role."} &
            "@roles__role_name" #> s"${r.role}" &
            // ajaxSubmit will submit the form.
            // The value of rolesBankId is given to bank_id_input field and the value of bank_id_input entered by user is given back to rolesBankId
            "@roles__bank_id_input" #> SHtml.text({if (r.requiresBankId) rolesBankId else ""}, rolesBankId = _, if (r.requiresBankId) "type" -> "text" else "type" -> "hidden") &
            "@roles__role_input" #> SHtml.text(s"${r.role}", entitlementRequestRoleName = _, "type" -> "hidden" ) &
            "@roles__resource_id_input" #> text(i.id.toString, s => RolesResourceId = s, "type" -> "hidden", "id" -> s"roles__resource_id_input_${i.id}_${r.role}") &
            "@roles__request_entitlement_button" #> Helper.ajaxSubmit("Request", disabledBtn, processEntitlementRequest) &
            "@roles__entitlement_request_response [id]" #> s"roles__entitlement_request_response_${i.id}_${r.role}" &
            "@roles__entitlement_request_button_box [style]" #> { if (! isLoggedIn || r.userHasEntitlement || r.userHasEntitlementRequest)
                s"display: none"
              else
                s"display: block"
            }
          } &
          //
          "@request_verb_input" #> text(i.verb, s => requestVerb = s, "type" -> "hidden", "id" -> s"request_verb_input_${i.id}") &
          "@resource_id_input" #> text(i.id.toString, s => resourceId = s, "type" -> "hidden", "id" -> s"resource_id_input_${i.id}") &
          // Replace the type=submit with Javascript that makes the ajax call.
           "@success_response_body [id]" #> s"success_response_body_${i.id}" &
          // The button. First argument is the text of the button (GET, POST etc). Second argument is function to call. Arguments to the func could be sent in third argument
            "@call_button" #> Helper.ajaxSubmit(i.verb, disabledBtn, process) &
            ".favourites_operation_id" #> text(i.id.toString, s => favouritesOperationIdFromWebpage = s,  "type" -> "hidden","class" -> "favourites_operation_id") &
            ".favourites_api_collection_id" #> text(apiCollectionId, s => favouritesApiCollectionId = s,  "type" -> "hidden","class" -> "favourites_api_collection_id") &
            ".favourites_button" #> Helper.ajaxSubmit("★", disabledBtn, processFavourites, "id" -> s"favourites_button_${i.id.toString}",  
              if(apiCollectionIdParam.isDefined && webpageOperationIds.nonEmpty) {"style" -> "color:#53C4EF"} 
              else if(myWebpageOperationIds.contains(i.id.toString)) {"style" -> "color:#53C4EF"} 
              else {"style" -> "color:#767676"}
            ) &
            ".favourites_error_message [id]" #> s"favourites_error_message_${i.id}" &
            "@content-box__available-since_version [href]" #> s"/?version=${i.implementedBy.version}" &
            "@content-box__available-since_version *" #> s"${i.implementedBy.version},  " &
            "@content-box__available-since_span *" #> s"function_name: by ${i.implementedBy.function},  operation_id: ${i.operationId}  "&
            "@content-box__available-since_tags" #> i.tags.map { tag =>
                "@content-box__available-since_tags [href]" #> s"/?tags=$tag" & 
                  "@content-box__available-since_tags *" #> s"$tag, " 
          }
        }
      }   
    }
    logger.debug("after showResources:")
    if(Helper.getPropsAsBoolValue("sso.enabled", false)) {
      logger.debug("Single Sign On is enabled")
      if(OAuthClient.loggedIn) cssResult else OAuthClient.redirectToOauthLogin()
    } else {
      cssResult
    }
  }

  def showGlossary = {

    logger.debug("before showResources:")
    def resourceDocsRequiresRole = ObpAPI.getRoot.flatMap(_.extractOpt[APIInfoJson400].map(_.resource_docs_requires_role)).openOr(false)
    // Get a list of resource docs from the API server
    // This will throw an exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)
    // The overview contains html. Just need to convert it to a NodeSeq so the template will render it as such
    val allResourcesBox = getAllResourceDocsJson(apiVersion)

    //Here need to change to Nil, if we throw exception here, we can not render the other parts of the home page.
    val allResourcesList = allResourcesBox.getOrElse(Nil)

    
    // logger.info(s"showGlossary hello ")

    val glossaryItems = getGlossaryItemsJson.map(_.glossary_items).getOrElse(List())

    if(glossaryItems.length==0) {
      ".resource [style]" #> s"display: none" &
        ".resource-error [style]" #> s"display: block" &
        ".content-box__headline *" #> {
          if(!isLoggedIn)//If no resources, first check the login, 
            "Sorry, we could not return any Glossary Items. Note: OBP-20001: User not logged in."
          else if(isLoggedIn && canReadGlossaryRole.isEmpty) //Then check the missing role
            "Sorry, we could not return any Glossary Items. Note: OBP-20006: User is missing one or more roles: CanReadGlossary"
          else // all other cases throw the general error.
            "Sorry, we could not return any Glossary Items."
        }&{
        if(isLoggedIn && canReadGlossaryRole.isEmpty){
          //required roles and related user information
          "@roles_box [id]" #> s"roles_box_CanReadGlossaryRoleInfo" &
            "@roles_box [style]" #> {s"display: block"} &
            // We generate multiple .role_items from roleInfos (including the form defined in index.html)
            ".role_item" #> canReadGlossaryRoleInfo.map { r =>
              "@roles__status" #> {if (! isLoggedIn)
                s" - Please login to request this Role"
              else if  (r.userHasEntitlement)
                s" - You have this Role."
              else if (r.userHasEntitlementRequest)
                s" - You have requested this Role. Please contact the administrators to grant you this role."
              else
                s" - You can request this Role."} &
                "@roles__role_name" #> s"${r.role}" &
                // ajaxSubmit will submit the form.
                // The value of rolesBankId is given to bank_id_input field and the value of bank_id_input entered by user is given back to rolesBankId
                "@roles__bank_id_input" #> SHtml.text({if (r.requiresBankId) rolesBankId else ""}, rolesBankId = _, if (r.requiresBankId) "type" -> "text" else "type" -> "hidden") &
                "@roles__role_input" #> SHtml.text(s"${r.role}", entitlementRequestRoleName = _, "type" -> "hidden" ) &
                "@roles__request_entitlement_button" #> Helper.ajaxSubmit("Request", disabledBtn, processEntitlementRequest) &
                "@roles__entitlement_request_response [id]" #> s"roles__entitlement_request_response_${canReadGlossaryRoleInfo}_${r.role}" &
                "@roles__entitlement_request_button_box [style]" #> { if (! isLoggedIn || r.userHasEntitlement || r.userHasEntitlementRequest)
                  s"display: none"
                else
                  s"display: block"
                }
            }
        }else{
          "@roles_box [style]" #> s"display: none"
        }
      }
    }else{
      ".glossary" #> glossaryItems.map  { i =>
        //from the tag, we need the oprationId, 
        val tag = i.title.replaceAll(" ", "-")
        val operationId = allResourcesList.find(_.tags.head == tag).map(_.operation_id).getOrElse("")
      // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
      ".end-point-anchor [href]" #> s"#${urlEncode(i.title.replaceAll(" ", "-"))}" &
        ".content-box__headline *" #> i.title &
        ".content-box__headline [id]" #> i.title.replaceAll(" ", "-") & // id for the anchor to find
        //i.title must be a proper tag, and will prepare the URL for it ...
        ".glossary_item_apis [href]" #> {
          s"./?operation_id=${operationId.replace(".","_").replaceAll(" ","_")}#group-${tag}"
        }&
        ".glossary_item_apis *" #> {
          if(operationId == "") "" else "Go Back to "+i.title +" APIs"
        } &
        // Replace attribute named overview_text with the value (whole div/span element is replaced leaving just the text)
        // This is the main description text. We use the html version of the description
        ".content-box__text-box *" #> stringToNodeSeq(i.description.html)
    } &
      // This is the left hand (title) list of glossary items
      ".api_list_item" #> glossaryItems.map { i =>
        // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
        ".api_list_item_link [href]" #> s"#${urlEncode(i.title.replaceAll(" ", "-"))}" &
          ".api_list_item_link *" #> i.title &
          ".api_list_item_link [id]" #> s"index_of_${urlEncode(i.title.replaceAll(" ", "-"))}"
      }
    } 
  }

  def showMessageDocs = {

    val messageDocs = getMessageDocsJson(presetConnector).map(_.message_docs).getOrElse(List()).sortBy(i => (i.adapter_implementation.group, i.adapter_implementation.suggested_order))

    // Group message docs by group in adapter implementation.
    val unsortedGroupedmessageDocs: Map[String, List[MessageDocJsonV220]] = messageDocs.groupBy(_.adapter_implementation.group)

    // Sort by group
    val groupedMessageDocs  = unsortedGroupedmessageDocs.toSeq.sortBy(_._1)

    ".info-box__headline *" #> s"Message Docs for Connector: $presetConnector" &
    "@api_group_item" #> groupedMessageDocs.map { i =>
      "@api_group_name *" #> s"${i._1.replace("-"," ")}" &
        // Within each group, and sort by suggested_order. List the message docs
        "@api_list_item" #> i._2.sortBy(_.adapter_implementation.suggested_order).map { i =>
          "@api_list_item_link [href]" #> s"#${i.process}" &
            "@api_list_item_link *" #> s"${i.process}" & //  ${i.adapter_implementation.suggested_order}" &
            "@api_list_item_link [id]" #> s"index_of_${i.process}"
        }
    } &
    ".message-doc" #> messageDocs.map  { i =>
      ".end-point-anchor [href]" #> s"#${urlEncode(i.process.replaceAll(" ", "-"))}" &
        ".content-box__headline *" #> i.process &
        ".content-box__headline [id]" #> i.process.replaceAll(" ", "-") & // id for the anchor to find
        ".outbound-topic *" #> stringToNodeSeq(i.outbound_topic.getOrElse("")) &
        ".inbound-topic *" #> stringToNodeSeq(i.inbound_topic.getOrElse("")) &
        ".outbound-message *" #> stringToNodeSeq(Helper.renderJson(i.example_outbound_message)) &
        ".inbound-message *" #> stringToNodeSeq(Helper.renderJson(i.example_inbound_message)) &
        ".description *" #> stringToNodeSeq((i.description)) &
        ".inbound-required-fields *" #> stringToNodeSeq(Helper.renderJson(i.requiredFieldInfo.getOrElse(JNothing))) &
          ".dependent-endpoints *" #>
              <ul>{i.dependent_endpoints.map { endpointInfo =>
                    val link = s"/?version=${endpointInfo.version}&list-all-banks=false#${endpointInfo.version.replace('.', '_')}-${endpointInfo.name}"
                    <li>{endpointInfo.version}: <a style="color: white;" href={link}>{endpointInfo.name}</a></li>
                  }
                }</ul>
    }
  }

  private lazy val shownVersionNamesInMainPage: Set[String] = {
    val shownLinks =  Helper.getPropsValue("main.included.links") match {
      case Full(v) if(v.trim.size > 0) => v.trim
      case _ => "OBP_PSD2, OBP_3.1.0, OBP_4.0.0, Glossary"
    }

    shownLinks.split("""\s*,\s*""")
      .map(_.replaceAll("\\s+", "_"))
      .filterNot(_.isEmpty)
      .toSet
  }



  private def shownVersions(): CssSel = {
    val requestUrl = S.uri.replaceFirst("""/?\?.*""", "") // remove request param part: /?param=.. or ?param=...
    requestUrl match {
      case "/"| "/index" if(shownVersionNamesInMainPage.nonEmpty)=> shownVersionNamesInMainPage
        .map(id => s"#$id [style]" #> "").reduce( _ & _)
      case _  => "#notExists_this_is_just_do_nothing" #> "" // a placeholder of do nothing
    }
  }

  /*
    Return the git commit. If we can't for some reason (not a git root etc) then log and return ""
   */
  def gitCommit : String = {
    val commit = try {
      val properties = new java.util.Properties()
      logger.debug("Before getResourceAsStream git.properties")
      properties.load(getClass().getClassLoader().getResourceAsStream("git.properties"))
      logger.debug("Before get Property git.commit.id")
      properties.getProperty("git.commit.id", "")
    } catch {
      case e : Throwable => {
        logger.warn("gitCommit says: Could not return git commit. Does resources/git.properties exist?")
        logger.error(s"Exception in gitCommit: $e")
        "" // Return empty string
      }
    }
    commit
  }

}


