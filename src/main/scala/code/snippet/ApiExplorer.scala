package code.snippet

import java.net.URL

import code.lib.ObpJson._
import code.lib._
import code.util.Helper
import code.util.Helper.MdcLoggable
import net.liftweb.util.{CssSel, Html5, Props}

import scala.collection.immutable.{List, Nil}

//import code.snippet.CallUrlForm._
import net.liftweb.http.{S, SHtml}
import net.liftweb.json.JsonAST.{JObject, JValue}
import net.liftweb.json.{Extraction, JsonParser}

import scala.xml.{NodeSeq, Text}
// for compact render
import code.lib.ObpAPI.{allBanks, getEntitlementRequestsV300, getEntitlementsV300, getGlossaryItemsJson, getResourceDocsJson, isLoggedIn, getMessageDocsJson}
import net.liftweb.common._
import net.liftweb.http.CurrentReq
import net.liftweb.http.SHtml.{ajaxSelect, ajaxSubmit, text}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetHtml}
import net.liftweb.json.Serialization.writePretty
import net.liftweb.json._
import net.liftweb.util.Helpers._


// see https://simply.liftweb.net/index-7.10.html on css selectors


case class Bank(
                 id : String,
                 shortName : String,
                 fullName : String,
                 logo : String,
                 website : String,
                 isFeatured : Boolean
               )

                // showToUser : Boolean)


case class CreateEntitlementRequestJSON(bank_id: String, role_name: String)


case class UserEntitlementRequests(entitlementRequestId: String,
                                   roleName: String,
                                   bankId : String,
                                   username : String )






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

  val currentTag = S.param("CurrentTag").getOrElse("Account")
  

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



  def stringToOptBoolean (x: String) : Option[Boolean] = x.toLowerCase match {
    case "true" | "yes" | "1" | "-1" => Some(true)
    case "false" | "no" | "0" => Some(false)
    case _ => Empty
  }

  val showCoreParam: Option[Boolean] = for {
    x <- S.param("core")
    y <- stringToOptBoolean(x)
  } yield y

  logger.info(s"showCore is $showCoreParam")

  val showPSD2Param: Option[Boolean] = for {
    x <- S.param("psd2")
    y <- stringToOptBoolean(x)
  } yield y

  logger.info(s"showPSD2 is $showPSD2Param")

  val showOBWGParam: Option[Boolean] = for {
    x <- S.param("obwg")
    y <- stringToOptBoolean(x)
  } yield y

  logger.info(s"showOBWG is $showOBWGParam")

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

  val rawTagsParam = S.param("tags")

  logger.info(s"rawTagsParam is $rawTagsParam")

  val rawLanguageParam = S.param("language")

  logger.info(s"rawLanguageParam is $rawLanguageParam")
  
  val tagsParamString = "&tags=" + rawTagsParam.mkString(",")

  logger.info(s"tagsParamString is $rawTagsParam")

  val languagesParamString = "&language=" + rawLanguageParam.mkString(",")

  logger.info(s"languagesParamString is $languagesParamString")
  
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



  val tagsHeadline : String = tagsParam match {
    case Some(x) => "filtered by tag: " + x.mkString(", ")
    case _ => ""
  }

  val languageHeadline : String = rawLanguageParam match {
    case Full(x) => x
    case _ => ""
  }

  val implementedHereHeadline : String = nativeParam match {
    case Some(true) => "(those added or modified in this version)"
    case Some(false) => "(those inherited from previous versions)"
    case _ => ""
  }


  // TODO We should remove this if we remove defaultCatalog
  // This parameter stops the default catalog kicking in
  val ignoreDefaultCatalog: Option[Boolean] = for {
    x <- S.param("ignoredefcat")
    y <- stringToOptBoolean(x)
  } yield y

  logger.info(s"ignoreDefaultCatalog is $ignoreDefaultCatalog")




  // TODO Probably we should remove this as its not often used.
  // If there is a main purpose of the sandbox, then know that.
  val defaultCatalog = Helper.getPropsValue("defaultCatalog", "")


  val showCore = showCoreParam

  // If no catalog preferences have been set and not asking for all and we are on an OBWG sandbox, set that preference
  val showOBWG : Option[Boolean] =  if (showCoreParam == None && showPSD2Param == None && showOBWGParam == None && (defaultCatalog == "OBWG") && !ignoreDefaultCatalog.getOrElse(false)) {
    Some(true)
  } else {
    showOBWGParam
  }

  val showPSD2 : Option[Boolean] =  if (showCoreParam == None && showPSD2Param == None && showOBWGParam == None && (defaultCatalog == "PSD2") && !ignoreDefaultCatalog.getOrElse(false)) {
    Some(true)
  } else {
    showPSD2Param
  }



  val showString = showCore.map(i => s"core=$showCore&").toString + showPSD2.map(i => s"psd2=$showPSD2").toString

  // Used for links to the Resource Docs / Swagger json (don't include default default catalog)
  // Note: No leading &
  val pureCatalogParams = s"core=${showCore.getOrElse("")}&psd2=${showPSD2.getOrElse("")}&obwg=${showOBWG.getOrElse("")}"


  val resetCatalogParams = s"&core=&psd2=&obwg="


  // Used for links in this web application
  val catalogParams = s"&$pureCatalogParams&ignoredefcat=${ignoreDefaultCatalog.getOrElse("")}"

  println(catalogParams)



  def stringToNodeSeq(html : String) : NodeSeq = {
    val newHtmlString =scala.xml.XML.loadString("<div>" + html + "</div>").toString()

    //Note: `parse` method: We much enclose the div, otherwise only the first element is returned. 
    Html5.parse(newHtmlString) match {
      case Full(parsedHtml) =>
        parsedHtml
      case _ => 
        logger.error("Cannot parse HTML:")
        logger.error(html)
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
      allAccountsJson <- ObpAPI.privateAccounts.toList
      barebonesAccountJson <- allAccountsJson.accounts.toList.flatten
      bankId <- barebonesAccountJson.bank_id
    } yield BankId(bankId)


  def showResources = {

    val defaultVersion: String = "OBPv4.0.0"

    // Get the requested version from the url parameter and default if none
    val apiVersionRequested = S.param("version").getOrElse(defaultVersion)



    // Possible OBP Versions
    val obpVersionsSupported = List("OBPv2.2.0", "OBPv3.0.0", "OBPv3.1.0", "OBPv4.0.0")

    val otherVersionsSupported = List("BGv1.3", "UKv3.1", "UKv2.0", "STETv1.4", "PAPIv2.1.1.1", "b1", "AUv1.0.0")

    // Set the version to use.
    val apiVersion: String = {
      if (obpVersionsSupported.contains(apiVersionRequested)) {
        // Prefix with v (for OBP versions because they come just with number from API Explorer)
        // Note: We want to get rid of this "v" prefix ASAP.
        s"v$apiVersionRequested"
      } else
      if (otherVersionsSupported.contains(apiVersionRequested)) {
          s"$apiVersionRequested"
        } else {
        S.notice(s"Note: Requested version $apiVersionRequested is not currently supported. Set to v$defaultVersion")
        s"v$defaultVersion"
      }
    }

    val isObpVersion: Boolean = {
      obpVersionsSupported.contains(apiVersionRequested)
    }

    logger.info(s"apiVersion is: $apiVersion")


    // To link to API home page (this is duplicated in OAuthClient)
    val baseUrl = Helper.getPropsValue("api_hostname", S.hostName)
    //
    val apiPortalHostname = Helper.getPropsValue("api_portal_hostname", baseUrl)


    // Use to show the developer the current base version url
    val baseVersionUrl = s"${OAuthClient.currentApiBaseUrl}"

    // Link to the API endpoint for the resource docs json TODO change apiVersion so it doesn't have a "v" prefix
    val resourceDocsPath = s"${OAuthClient.currentApiBaseUrl}/obp/v1.4.0/resource-docs/${apiVersion.stripPrefix("v")}/obp?$pureCatalogParams${tagsParamString}${languagesParamString}"

    // Link to the API endpoint for the swagger json
    val swaggerPath = s"${OAuthClient.currentApiBaseUrl}/obp/v1.4.0/resource-docs/${apiVersion.stripPrefix("v")}/swagger?$pureCatalogParams${tagsParamString}${languagesParamString}"
    
    val chineseVersionPath = "?language=zh"



    val entitlementsForCurrentUser = getEntitlementsForCurrentUser
    logger.info(s"there are ${entitlementsForCurrentUser.length} entitlementsForCurrentUser(s)")
    
    val hasTheCanReadResourceDocRole = entitlementsForCurrentUser.map(_.roleName).contains("CanReadResourceDoc")


    val userEntitlementRequests = getUserEntitlementRequests



    logger.info(s"there are ${userEntitlementRequests.length} userEntitlementRequests(s)")




    // Get a list of resource docs from the API server
    // This will throw an exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)
    // The overview contains html. Just need to convert it to a NodeSeq so the template will render it as such
    val allResources = for {
      rs <- getResourceDocsJson(apiVersion).toList
      r <- rs.resource_docs
    } yield ResourceDocPlus(
       //in OBP-API, before it returned v3_1_0, but now, only return v3.1.0
      //But this filed will be used in JavaScript, so need clean the field.
      id = r.operation_id.replace(".","_"),
      verb = r.request_verb,
      url = modifiedRequestUrl(
        r.specified_url, // We used to use the request_url - but we want to use the specified url i.e. the later version.
        apiVersion
          .replaceAll("UKv2.0", "v2.0")
          .replaceAll("UKv3.1", "v3.1")
          .replaceAll("BGv1.3.3", "v1.3.3")
          .replaceAll("BGv1", "v1")
          .replaceAll("BGv1.3", "v1.3")
          .replaceAll("PAPIv2.1.1.1", "v2.1.1.1")
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
      isCore = r.is_core,
      isPSD2 = r.is_psd2,
      isOBWG = r.is_obwg,
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

    // The list generated here might be used by an administrator as a white or black list of API calls for the API itself.
    val commaSeparatedListOfResources = resources.map(_.implementedBy.function).mkString("[", ", ", "]")


    // Headline and Description of the search
    val (catalogHeadline, catalogDescription) = List(showCore, showOBWG, showPSD2)  match {

      // Core / Minimal (basically same as PSD2)
      case List(Some(true), None, None) => ("Core OBP",
        "A core set of customer facing APIs built on core banking services.")

      // Non Core (this query not used by API Explorer)
      case List(Some(false), None, None) => ("Non-Core OBP",
        "These APIs don't assume the owner of the account is accessing the account related resources (so they support accountant, auditor, public, admin access) " +
          "Meta data, data sharing, data redaction and entitlements is included. ")

      // All
      case List(None, None, None) => isObpVersion match {
        case true => ("OBP", "Open Bank Project")
        case false if apiVersionRequested == ("BGv1.3.3") => ("Berlin Group", "Berlin Group")
        case false if apiVersionRequested == ("BGv1") => ("Berlin Group", "Berlin Group")
        case false if apiVersionRequested == ("BGv1.3") => ("Berlin Group", "Berlin Group")
        case false if apiVersionRequested == ("UKv3.1") => ("UK", "UK")
        case false if apiVersionRequested == ("UKv2.0") => ("UK", "UK")
        case false if apiVersionRequested == ("STETv1.4") => ("STET", "STET")
        case false if apiVersionRequested == ("PAPIv2.1.1.1") => ("Polish", "Polish")
        case false if apiVersionRequested == ("b1") => ("Builder", "Builder")
        case _  => ("", "")
      }


      // UK OBWG
      case List(None, Some(true), None) => ("PSD2 + Open Data APIs", "PSD2 + Open Data: Access to Accounts, Payments and Open Data related to the Bank.")

      // PSD2
      case List(None,  None, Some(true)) => ("OBP PSD2 Catalog", "PSD2: AIS and PIS")

      // Intersection
      case List(Some(true), Some(true), Some(true)) => ("Intersection of all Catalogs",
        "APIs common to Catalogs")

      case _ => ("APIs", "")
    }


    logger.info (s"catalogHeadline is: $catalogHeadline")
    logger.info (s"catalogDescription is: $catalogDescription")



    // Title / Headline we display including count of APIs
    val headline : String = s"""
      $catalogHeadline
      ${apiVersionRequested.stripPrefix("OBP").stripPrefix("BG").stripPrefix("STET").stripPrefix("UK")}
      $tagsHeadline $languageHeadline $implementedHereHeadline (${resources.length} APIs)
      """.trim()



    logger.info (s"showingMessage is: $headline")


    // Used to show / hide the Views selector
    // TODO disable instead of hiding
    val displayViews = if (showCore.getOrElse(false) || showOBWG.getOrElse(false)) {
      logger.info("not show views drop down")
      "none"
    } else {
      logger.info("show views drop down")
      "block"
    }


    val displayFeatured = if (featuredResources.length > 0 ) {
      logger.info("show featured")
      "block"
    } else {
      logger.info("not show featured")
      "none"
    }






    // Do we want to show the Request Entitlement button.
    // Should also consider if User has submitted an entitlment request or already has the role.
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

    var resourceId = ""
    var requestVerb = ""
    var requestUrl = ""
    var requestBody = "{}"
    var responseBody = "{}"
    var errorResponseBodies = List("")





    var entitlementRequestStatus = ""

    // This value is used to pre populate the form for Entitlement Request.
    // It maybe be changed by the user in the form.
    var rolesBankId = presetBankId

    var RolesResourceId = ""


    var entitlementRequestRoleName = ""

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

    def process(name: String): JsCmd = {


      // The DIVS in the DOM have underscores in their names.
      resourceId = resourceId.replace(".","_")

      logger.info(s"requestUrl is $requestUrl")
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
      val jsCommandHideConnectorMethodsBoxTarget : String =  s"DOLLAR_SIGN('#$connectorMethodsBoxTarget').fadeOut();".replace("DOLLAR_SIGN","$")

      val jsCommandHideRequestRolesResponsesBox : String =  s"DOLLAR_SIGN('#$requestRolesResponsesBoxTarget').fadeOut();".replace("DOLLAR_SIGN","$")

      // The id of the possible error responses box we want to hide after calling the API
      val typicalSuccessResponseBoxTarget = "typical_success_response_box_" + resourceId
      // The javascript to hide it.
      val jsCommandHideTypicalSuccessResponseBox : String =  s"DOLLAR_SIGN('#$typicalSuccessResponseBoxTarget').fadeOut();".replace("DOLLAR_SIGN","$")

      // The id of the full path
      val fullPathTarget = "full_path_" + resourceId
      val fullHeadersTarget = "full_headers_" + resourceId
      // The javascript to show it

      val jsCommandShowFullPath : String =  s"DOLLAR_SIGN('#$fullPathTarget').fadeIn();".replace("DOLLAR_SIGN","$")
      val jsCommandShowFullHeaders : String =  s"DOLLAR_SIGN('#$fullHeadersTarget').fadeIn();".replace("DOLLAR_SIGN","$")

      // alert('$fullPathTarget');
      //logger.info(s"jsCommand is $jsCommand")
      //logger.info(s"jsCommand2 is $jsCommandHidePossibleErrorResponsesBox")


      /////////////
      // TODO It would be nice to modify getResponse and underlying functions to return more information about the request including full path
      // For now we duplicate the construction of the fullPath
      val apiUrl = OAuthClient.currentApiBaseUrl
      
      val urlWithVersion =      
        if (showPSD2 == Some(true)) {
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
        .replaceAll("UKv2.0", "v2.0")
        .replaceAll("UKv3.1", "v3.1")
        .replaceAll("BGv1.3.3", "v1.3.3")
        .replaceAll("BGv1", "v1")
        .replaceAll("BGv1.3", "v1.3")
        .replaceAll("OBPv", "")
      )

      //val urlWithVersion = s"/$apiVersion$requestUrl"
      val fullPath = new URL(apiUrl + urlWithVersion
        .replaceAll("UKv2.0", "v2.0")
        .replaceAll("UKv3.1", "v3.1")
        .replaceAll("BGv1.3.3", "v1.3.3")
        .replaceAll("BGv1.3", "v1.3")
        .replaceAll("BGv1", "v1")
        .replaceAll("OBPv", ""))
      //////////////

      val (body, headers) = getResponse(requestUrl, requestVerb, jsonObject)
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



    def getResponse (url : String, resourceVerb: String, json : JValue) : (String, String) = {

      implicit val formats = net.liftweb.json.DefaultFormats

      // version is now included in the url
      val urlWithVersion = s"$url"

      var headersOfCurrentCall: List[String] = Nil

      val responseBodyBox = {
        resourceVerb match {
          case "GET" =>
            val x = ObpGetWithHeader(urlWithVersion)
            headersOfCurrentCall = x._2
            x._1
          case "DELETE" =>
            val x = ObpDeleteWithHeader(urlWithVersion)
            headersOfCurrentCall = x._2
            x._1
          case "POST" =>
            val x = ObpPostWithHeader(urlWithVersion, json)
            headersOfCurrentCall = x._2
            x._1
          case "PUT" =>
            val x = ObpPutWithHeader(urlWithVersion, json)
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
          case Full(json) => writePretty(json)
          case Empty => "Empty: API did not return anything"
          case Failure(message, _, _) => message
        }

      logger.debug(s"responseBody is $responseBody")
      (responseBody, headersOfCurrentCall.mkString("\n"))
    }


    val thisApplicationUrl = s"${CurrentReq.value.uri}?version=${apiVersionRequested}&list-all-banks=${listAllBanks}${catalogParams}${tagsParamString}${languagesParamString}"


    // Create a list of (version, url) used to populate the versions whilst preserving the other parameters
    //val obpVersionUrls: List[(String, String)] = obpVersionsSupported.map(i => (i.replace("OBPv", "v"), s"${CurrentReq.value.uri}?version=${i}&list-all-banks=${listAllBanks}${catalogParams}"))

    val obpVersionUrls: List[(String, String)] = obpVersionsSupported.map(i => (i.replace("OBPv", "v"), s"?version=${i}&list-all-banks=${listAllBanks}${resetCatalogParams}"))



    // Create a list of (version, url) used to populate the versions whilst preserving the other parameters except catalog
    // Includes hack for Berlin Group
    val otherVersionUrls: List[(String, String)] = otherVersionsSupported.map(i => (i
      .replace("b1", "API Builder")
      .replace("BGv1.3.3", "Berlin Group 1.3.3")
      .replace("BGv1.3", "Berlin Group 1.3")
      .replace("BGv1", "Berlin Group")
      .replace("UKv2.0", "UK 2.0")
      .replace("UKv3.1", "UK 3.1")
      .replace("STETv1.4", "STET 1.4")
      .replace("PAPIv2.1.1.1", "Polish API 2.1.1.1")
      .replace("AUv1.0.0", "AU CDR v1.0.0"),
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

    val banksForUser =
      if (listAllBanks) // Url param says show all.
        banks
      else
        if(!myBankIds.isEmpty) // User has accounts so show those banks
          banks.filter(b => myBankIds.contains(BankId(b.id)))
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


    // In case we use Extraction.decompose
    implicit val formats = net.liftweb.json.DefaultFormats

    "#login_status_message" #> loggedInStatusMessage &
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
      "@obp_version *" #> s" ${i._1} " &
      "@obp_version [href]" #> s"${i._2}"
    } &
      "@other_versions" #> otherVersionUrls.map { i =>
        "@other_version *" #> s" ${i._1} " &
          "@other_version [href]" #> s"${i._2}"
      } &
    ".info-box__headline *" #> s"$headline"  &
    "@version_path *" #> s"$baseVersionUrl" &
    "@version_path [href]" #> s"$baseVersionUrl" &
    "@resource_docs_path [href]" #> s"$resourceDocsPath" &
    "@swagger_path [href]" #> s"$swaggerPath" &
    "@chinese_version_path [href]" #> s"$chineseVersionPath" &
    "#api_home_link [href]" #> s"$apiPortalHostname" &
    "@views_box [style]" #> s"display: $displayViews;" &
    "@catalog_description *" #> s"$catalogDescription" &
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
      "@api_group_item" #> groupedResources.map { i =>
          "@api_group_name *" #> s"${i._1.replace("-"," ")}" &
            // Set an anchor (href and id) for a group
            "@api_group_name [href]" #> s"#group-${i._1}" &
            "@api_group_name [id]" #> s"group-${i._1}" &
            // Within each group (first tag), list the resources
            "@api_list_item" #> i._2.sortBy(_.summary.toString()).map { i =>
              // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
                "@api_list_item_link [href]" #> s"?CurrentTag=${i.tags.head}#${i.id}" &
                "@api_list_item_link [href]" #> s"?CurrentTag=${i.tags.head}&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${presetViewId}&counterparty_id=${presetCounterpartyId}&transaction_id=${presetTransactionId}#${i.id}" &
                  "@api_list_item_link *" #> i.summary &
                  "@api_list_item_link [id]" #> s"index_of_${i.id}"
                  // ".content-box__available-since *" #> s"Implmented in ${i.implementedBy.version} by ${i.implementedBy.function}"
        }
      } &
    // This is used by API administrators who want to create white or black lists of API calls to use in Props for the API.
    ".comma_separated_api_call_list *" #> commaSeparatedListOfResources &
      // replace the node identified by the class "resource" with the following
      // This creates the list of resources in the DOM
    {
      if(resources.length==0) {
      ".resource [style]" #> s"display: none" &
        ".resource-error [style]" #> s"display: block" &
        ".content-box__headline *" #> {
          if(!isLoggedIn)//If no resources, first check the login, 
            "OBP-20001: User not logged in. Authentication is required!"
          else if(isLoggedIn && !hasTheCanReadResourceDocRole) //Then check the missing role
            "OBP-20006: User is missing one or more roles: CanReadResourceDoc"     
          else // all other cases throw the gernal error.
            "There are no resource docs in the current Sandbox for this request!"
        }
      }
      else {
        ".resource" #> resources.filter(_.tags.head==currentTag).map { i =>
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
          "@request_url_input" #> text(i.url, s => requestUrl = s, "maxlength" -> "512", "size" -> "100", "id" -> s"request_url_input_${i.id}") &
          "@full_path [id]" #> s"full_path_${i.id}" &
          "@full_headers [id]" #> s"full_headers_${i.id}" &
          // Extraction.decompose creates json representation of JObject.
          "@example_request_body_input" #> text(Helper.renderJson(i.exampleRequestBody), s => requestBody = s, "maxlength" -> "100000", "size" -> "100", "type" -> "text") &
          //"@request_body_input" #> textarea((""), s => requestBody = s, "cols" -> "1000", "rows" -> "10","style"->"border:none") &
          //
          // Typical Success Response
          "@typical_success_response_box [id]" #> s"typical_success_response_box_${i.id}" &
          //"@typical_success_response [id]" #> s"typical_success_response_${i.id}" &
          "@typical_success_response *" #> Helper.renderJson(i.successResponseBody) &
          // Possible Errors
          "@possible_error_responses_box [id]" #> s"possible_error_responses_box_${i.id}" &
          // This class gets a list of several possible error reponse items
          ".possible_error_item" #> i.errorResponseBodies.map { i =>
              ".possible_error_item *" #> i
          } &
          "@connector_methods_box [id]" #> s"connector_methods_box_${i.id}" &
          // This class gets a list of connector methods
          ".connector_method_item" #> i.connectorMethods.map { i=>
            // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
            ".connector_method_item_link [href]" #> s"message-docs?connector=rest_vMar2019#${urlEncode(i.replaceAll(" ", "-"))}" &
              ".connector_method_item_link *" #> i
          } &
          //required roles and related user information
          "@roles_box [id]" #> s"roles_box_${i.id}" &
          "@roles_box [style]" #> { if (i.roleInfos.isEmpty)
              s"display: none"
            else
              s"display: block"
            } &
          // We generate mulutiple .role_items from roleInfos (including the form defined in index.html)
          ".role_item" #> i.roleInfos.map { r =>
            "@roles__status" #> {if (! isLoggedIn)
                                  s" - Please login to request this Role"
                                else if  (r.userHasEntitlement)
                                  s" - You have this Role."
                                else if (r.userHasEntitlementRequest)
                                  s" - You have requested this Role."
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
          ".content-box__available-since *" #> s"Implemented in ${i.implementedBy.version} by ${i.implementedBy.function}"
        }
      }   
    }
  }

  def showGlossary = {

    // logger.info(s"showGlossary hello ")

    // TODO cache this.
    val glossaryItems = getGlossaryItemsJson.map(_.glossary_items).getOrElse(List())


    ".glossary" #> glossaryItems.map  { i =>
      // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
      ".end-point-anchor [href]" #> s"#${urlEncode(i.title.replaceAll(" ", "-"))}" &
        ".content-box__headline *" #> i.title &
        ".content-box__headline [id]" #> i.title.replaceAll(" ", "-") & // id for the anchor to find
        //   // Replace attribute named overview_text with the value (whole div/span element is replaced leaving just the text)
        ".content-box__text-box *" #> stringToNodeSeq(i.description.html)
    } &
      ".api_list_item" #> glossaryItems.map { i =>
        // append the anchor to the current url. Maybe need to set the catalogue to all etc else another user might not find if the link is sent to them.
        ".api_list_item_link [href]" #> s"#${urlEncode(i.title.replaceAll(" ", "-"))}" &
          ".api_list_item_link *" #> i.title &
          ".api_list_item_link [id]" #> s"index_of_${urlEncode(i.title.replaceAll(" ", "-"))}"
      }
  }

  def showMessageDocs = {

    // TODO cache this.
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
        ".inbound-required-fields *" #> stringToNodeSeq(Helper.renderJson(i.requiredFieldInfo.getOrElse(JNothing)))
    }
      }

  private lazy val shownVersionNamesInMainPage: Set[String] = {
    val shownLinks =  Helper.getPropsValue("main.included.links") match {
      case Full(v) if(v.trim.size > 0) => v.trim
      case _ => "OBP_PSD2, OBP_3.1.0, OBP_4.0.0"
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

}


