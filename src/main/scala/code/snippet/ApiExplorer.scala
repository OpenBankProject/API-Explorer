package code.snippet

import _root_.net.liftweb._
import code.lib.ObpJson.{ImplementedBy, BarebonesAccountJson, BarebonesAccountsJson, ResourceDoc}
import code.lib._
import net.liftweb.http.js.jquery.JqJsCmds.DisplayMessage
import net.liftweb.util.Props

import scala.collection.immutable.Nil

//import code.snippet.CallUrlForm._
import net.liftweb.http.{SHtml, S}

import net.liftweb.json.{Extraction, JsonParser, JsonAST}
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import scala.xml.{XML, NodeSeq, Text}


import net.liftweb._
// for compact render
import net.liftweb.json._


import common._

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.{text,ajaxSubmit, textarea, select, ajaxSelect}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetHtml}

import net.liftweb.json.Serialization.writePretty

import code.lib.ObpAPI.{getResourceDocsJson, allBanks, allAccountsAtOneBank}

import net.liftweb.http.CurrentReq


case class Bank(
                 id : String,
                 shortName : String,
                 fullName : String,
                 logo : String,
                 website : String,
                 isFeatured : Boolean)




/*
Present a list of OBP resource URLs
 */
class ApiExplorer extends Loggable {




  val listAllBanks = S.param("list-all-banks").getOrElse("false").toBoolean
  logger.info(s"all_banks in url param is $listAllBanks")


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





  def stringToOptBoolean (x: String) : Option[Boolean] = x.toLowerCase match {
    case "true" | "yes" | "1" | "-1" => Some(true)
    case "false" | "no" | "0" => Some(false)
    case _ => Empty
  }

  val showCore: Option[Boolean] = for {
    x <- S.param("core")
    y <- stringToOptBoolean(x)
  } yield y

  logger.info(s"showCore is $showCore")

  val showPSD2: Option[Boolean] = for {
    x <- S.param("psd2")
    y <- stringToOptBoolean(x)
  } yield y


  logger.info(s"showPSD2 is $showPSD2")


  val showOBWG: Option[Boolean] = for {
    x <- S.param("obwg")
    y <- stringToOptBoolean(x)
  } yield y


  logger.info(s"showOBWG is $showOBWG")


  val showString = showCore.map(i => s"core=$showCore&").toString + showPSD2.map(i => s"psd2=$showPSD2").toString


  val showParams = s"&core=${showCore.getOrElse("")}&psd2=${showPSD2.getOrElse("")}&obwg=${showOBWG.getOrElse("")}"

  println(showParams)



  def stringToNodeSeq(html : String) : NodeSeq = {
    scala.xml.XML.loadString("<div>" + html + "</div>")
  }


  def modifiedRequestUrl(url: String, presetBankId: String, presetAccountId: String) = {
     // Potentially replace BANK_ID
     val url2: String = presetBankId match {
        case "" => url
        case _ => url.replaceAll("BANK_ID", presetBankId)
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
      case _ => url4.replaceAll("OTHER_ACCOUNT_ID", presetCounterpartyId)
    }

    // Potentially replace TRANSACTION_ID
    val url6: String = presetTransactionId match {
      case "" => url5
      case _ => url5.replaceAll("TRANSACTION_ID", presetTransactionId)
    }

    url6
  }

  def showResources = {

    val defaultVersion : String = "2.0.0"

    // Get the requested version from the url parameter and default if none
    val apiVersionRequested = S.param("version").getOrElse(defaultVersion)


    val supportedApiVersions = List ("1.2.1", "1.3.0", "1.4.0", "2.0.0")


    val apiVersion : String = {
      if (supportedApiVersions.contains(apiVersionRequested)) {
        s"v$apiVersionRequested"
      } else {
        S.notice(s"Note: Requested version $apiVersionRequested is not currently supported. Set to v$defaultVersion")
        s"v$defaultVersion"
      }
    }

    logger.info (s"API version to use is: $apiVersion")

    // Use to show the developer the current base version url
    val baseVersionUrl = s"${OAuthClient.currentApiBaseUrl}/$apiVersion"


    // Get a list of resource docs from the API server
    // This will throw an exception if resource_docs key is not populated
    // Convert the json representation to ResourceDoc (pretty much a one to one mapping)
    // The overview contains html. Just need to convert it to a NodeSeq so the template will render it as such
    val allResources = for {
      r <- getResourceDocsJson(apiVersion).map(_.resource_docs).get
    } yield ResourceDoc(
      id = r.operation_id,
      verb = r.request_verb,
      url = modifiedRequestUrl(r.request_url, presetBankId, presetAccountId),
      summary = r.summary,
      description = stringToNodeSeq(r.description),
      example_request_body = r.example_request_body,
      implementedBy = ImplementedBy(r.implemented_by.version, r.implemented_by.function),
      isCore = r.is_core,
      isPSD2 = r.is_psd2,
      isOBWG = r.is_obwg,
      tags = r.tags
    )


    // Filter
    val filteredResources1 : List[ResourceDoc] = showCore match {
      case Some(true) => allResources.filter(x => x.isCore == true)
      case Some(false) => allResources.filter(x => x.isCore == false)
      case _ => allResources
    }

    val filteredResources2 : List[ResourceDoc] = showPSD2 match {
      case Some(true) => filteredResources1.filter(x => x.isPSD2 == true)
      case Some(false) => filteredResources1.filter(x => x.isPSD2 == false)
      case _ => filteredResources1
    }

    val filteredResources3 : List[ResourceDoc] = showOBWG match {
      case Some(true) => filteredResources2.filter(x => x.isOBWG == true)
      case Some(false) => filteredResources2.filter(x => x.isOBWG == false)
      case _ => filteredResources2
    }

    val resources = filteredResources3


    // Headline and Description of the search
    val (catalogHeadline, catalogDescription) = List(showCore, showOBWG, showPSD2)  match {
      case List(Some(true), Some(true), Some(true)) => ("Intersection of OBP Core, UK Open Banking and PSD2",
        "API calls that are common to the three catalogs")

      case List(Some(false), None, None) => ("Non-Core OBP",
        "These APIs don't assume the owner of the account is accessing the account related resources (so they support accountant, auditor, public, admin access) " +
          "Meta data, data sharing, data redaction and entitlements is included. ")

      case List(None, None, None) => ("OBP",
        "These APIs support the full range of functionality including: Transaction history, payments, counterparty and transaction metadata, delegated account access, data redaction and entitlements.")

      case List(Some(true), None, None) => ("Core OBP",
        "This core set of APIs is chosen to support common customer facing applications that rely on existing core banking services only. " +
          "Customer data (accounts, transactions etc.) is provided only from the perspective of the account owner. " +
          "Bank Branches, ATMs and Products are available too.")

      case List(None, Some(true), None) => ("UK Open Banking",
        "These APIs support customer account and transaction data (from the perspective of the account holder), payments and some of the bank's open data too. ")

      case List(None,  None, Some(true)) => ("PSD2",
        "These APIs support customer account and transaction history, payments and pricing transparency.")

      case _ => ("APIs", "")
    }


    logger.info (s"catalogHeadline is: $catalogHeadline")
    logger.info (s"catalogDescription is: $catalogDescription")



    // Headline we display including count of APIs
    val headline : String = s"$catalogHeadline APIs (${resources.length})".trim()
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

    def process(): JsCmd = {
      logger.info(s"requestUrl is $requestUrl")
      logger.info(s"resourceId is $resourceId")
      logger.info(s"requestBody is $requestBody")


      // Create json object from input string
      val jsonObject = JsonParser.parse(requestBody).asInstanceOf[JObject]

      // the id of the element we want to populate and format.
      val result_target = "result_" + resourceId


      val box_target = "result_box_" + resourceId


      // This will highlight the json. Replace the $ sign after we've constructed the string

      val jsCommand : String =  s"DOLLAR_SIGN('#$box_target').fadeIn();DOLLAR_SIGN('#$result_target').each(function(i, block) { hljs.highlightBlock(block);});".replace("DOLLAR_SIGN","$")

      logger.info(s"command is $jsCommand")

      // Return the commands to call the url with optional body and put the response into the appropriate result div
      SetHtml(result_target, Text(getResponse(apiVersion, requestUrl, requestVerb, jsonObject))) &
      Run (jsCommand)
    }


    def getResponse (apiVersion : String, url : String, resourceVerb: String, json : JValue) : String = {

      implicit val formats = net.liftweb.json.DefaultFormats

      val urlWithVersion = s"/$apiVersion$url"

      val responseBodyBox = {
        resourceVerb match {
          case "GET" => ObpGet(urlWithVersion)
          case "DELETE" => ObpDelete(urlWithVersion)
          case "POST" => ObpPost(urlWithVersion, json)
          case "PUT" => ObpPut(urlWithVersion, json)
          case _ => {
            val failMsg = s"API Explorer says: Unsupported resourceVerb: $resourceVerb. Url requested was: $url"
            logger.warn(failMsg)
            Failure(failMsg)
          }
        }
      }


      logger.info(s"responseBodyBox is ${responseBodyBox}")

      // Handle the contents of the Box
      val responseBody =
        responseBodyBox match {
          case Full(json) => writePretty(json)
          case Empty => "Empty: API did not return anything"
          case Failure(message, _, _) => message
        }

      logger.info(s"responseBody is $responseBody")
      responseBody
    }


    val url = s"${CurrentReq.value.uri}?version=${apiVersionRequested}&list-all-banks=${listAllBanks}${showParams}"

    // So we can highlight (or maybe later exclusively show) the "active" banks in a sandbox.
    // Filter out empty string items
    val featuredBankIds = Props.get("featuredBankIds", "").split(",").toList.filter(i => i.length > 0)



    val banks = for {
      a <- allBanks.toList
      b <- a.bankJsons
    // This filtering could be turned on/off by Props setting
    // Filter out banks if we have a list of ones to use, else use all of them.
    // Also, show all if requested by url parameter
    // if featuredBankIds.length == 0  || featuredBankIds.contains(b.id.get)  || listAllBanks
    } yield Bank (b.id.get,
                  b.short_name.getOrElse(""),
                  b.full_name.getOrElse(""),
                  b.logo.getOrElse(""),
                  b.website.getOrElse(""),
                  featuredBankIds.contains(b.id.get)) // Add a flag to say if this bank is featured.

    // TODO dehardcode the redirect path.

    def onBankChange (v: Any) = {
      logger.info("bank changed to " + v.toString)
      S.redirectTo(s"$url&bank_id=${v}")
    }

    def onAccountChange (v: Any) = {
      logger.info("account changed to " + v.toString)
      S.redirectTo(s"$url&bank_id=${presetBankId}&account_id=${v}")
    }

    def onViewChange (v: Any) = {
      logger.info("view changed to " + v.toString)
      S.redirectTo(s"$url&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${v}")
    }

    def onCounterpartyChange (v: Any) = {
      logger.info("counterparty changed to " + v.toString)
      S.redirectTo(s"$url&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${presetViewId}&counterparty_id=${v}")
    }

    def onTransactionChange (v: Any) = {
      logger.info("transaction changed to " + v.toString)
      S.redirectTo(s"$url&bank_id=${presetBankId}&account_id=${presetAccountId}&view_id=${presetViewId}&counterparty_id=${presetCounterpartyId}&transaction_id=${v}")
    }


    // Get a list of tuples List(("bank short name", "id"),("bank two", "id2")) to populate the drop down select list.
    // TODO Add this again if banks is empty getOrElse(List(("", "No Banks")))
    //val bankOptions = ("", "Select Bank") :: banks.map(b => b.bankJsons.map(bj => (bj.id.getOrElse(""), bj.short_name.getOrElse("") + " (" + bj.id.getOrElse("") + ")"))).getOrElse(List(("", "No Banks")))


    val selectBank = ("", "Select Bank")

    def highlightFeatured(value: Boolean) : String = if (value) " *" else ""

    val bankOptions = banks.map(b => (b.id, b.shortName + " ("  + b.id + ")" + highlightFeatured(b.isFeatured) )).sortBy(a => (a._2, a._1))


    val selectBankOptions = selectBank :: bankOptions
    

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
          allAccountsJson <- ObpAPI.allAccountsAtOneBank(presetBankId).toList
          barebonesAccountJson <- allAccountsJson.accounts.toList.flatten
          accountId <- barebonesAccountJson.id
          label <- barebonesAccountJson.label
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
        case _ => for {
          counterpartiesJson <- ObpAPI.getCounterparties(presetBankId, presetAccountId, presetViewId).toList
          counterparty <- counterpartiesJson.other_accounts
        } yield (counterparty.id, counterparty.holder.name)
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
          transactionsJson <- ObpAPI.transactions121(presetBankId, presetAccountId, presetViewId, None,None,None,None,None).toList
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
    "#version *+" #> apiVersion &
    // replace the node identified by the class "resource" with the following
    // This creates the list of resources in the DOM
    ".info-box__headline *" #> s"$headline"  &
    "@version_path *" #> s"$baseVersionUrl" &
    "@version_path [href]" #> s"$baseVersionUrl" &
    "@views_box [style]" #> s"display: $displayViews;" &
    ".info-box__about_selected *" #> s"$catalogDescription" &
    ".resource" #> resources.map { i =>
      ".end-point-anchor [href]" #> s"#${i.id}" & // append the anchor to the current uurl
      ".content-box__headline *" #> i.summary &
      ".content-box__headline [id]" #> i.id & // id for the anchor to find
      ".resource_summary [href]" #> s"#${i.id}" &
      ".resource_summary [name]" #> s"${i.id}" &
      // Replace attribute named overview_text with the value (whole div/span element is replaced leaving just the text)
      ".content-box__text-box *" #> i.description &
      "@resource_description [id]" #> s"description_${i.id}" &
      ".url_caller [id]" #> s"url_caller_${i.id}" &
      // ".try_me_button [onclick]" #> s"$$(DOUBLE-QUOTE#url_caller_${i.id}DOUBLE-QUOTE).fadeToggle();".replaceAll("DOUBLE-QUOTE","""") &
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
      // Extraction.decompose creates json representation of JObject.
      "@example_request_body_input" #> text(pretty(render(i.example_request_body)), s => requestBody = s, "maxlength" -> "4096", "size" -> "100", "type" -> "text") &
      // TODO get this working. requestBody is not populated with textarea value "@request_body_input" #> textarea(pretty(render(i.example_request_body)), s => requestBody = s, "cols" -> "90", "rows" -> "5") &
      // We're not using the id at the moment
      "@request_verb_input" #> text(i.verb, s => requestVerb = s, "type" -> "hidden", "id" -> s"request_verb_input_${i.id}") &
      "@resource_id_input" #> text(i.id.toString, s => resourceId = s, "type" -> "hidden", "id" -> s"resource_id_input_${i.id}") &
      // Replace the type=submit with Javascript that makes the ajax call.
       "@success_response_body [id]" #> s"success_response_body_${i.id}" &
      // The button. First argument is the text of the button (GET, POST etc). Second argument is function to call. Arguments to the func could be sent in third argument
      "@call_button" #> ajaxSubmit(i.verb, process) &
      ".content-box__available-since *" #> s"Implmented in ${i.implementedBy.version} by ${i.implementedBy.function}"
    }
  }
}