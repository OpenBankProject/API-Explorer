package code.util

import code.lib.ObpAPI._
import code.lib.ObpJson.AccountJson
import code.util.Branding.activeBrand
import net.liftweb.common._
import net.liftweb.http.SHtml.ElemAttr
import net.liftweb.http.S.addFunctionMap
import net.liftweb.http.js.JsCmd
import net.liftweb.json.JsonAST.JNothing
import net.liftweb.json.{JValue, pretty, render}
import net.liftweb.util.Helpers.{asLong, toBoolean, toInt, tryo}
import net.liftweb.util.{Helpers, Props}

import scala.xml.{Elem, MetaData, Null, Text, UnprefixedAttribute}
import net.liftweb.util.Helpers.pairToUnprefixed


object Helper extends Loggable {



  // From bankId / accountId
  def getAccountTitle (bankId: String, accountId: String) : String = {
    val accountJsonBox = getAccount(bankId, accountId, "owner")

    val accountTitle = accountJsonBox match {
      case Full(accountJson) => getAccountTitle(accountJson)
      case _ => "Unknown Account"
    }
    accountTitle
  }


/*
Returns a string which can be used for the title of the account
*/
  def getAccountTitle(accountJson: AccountJson ) : String = {
  // Rewrite in more idiomatic style?
      var title = accountJson.label.getOrElse("")
      if (title.isEmpty) {
        if (hasManagementAccess(accountJson))
          title = accountJson.number.getOrElse("")
        else
          title = accountJson.id.getOrElse("")
      }
    title
  }


  def hasManagementAccess (accountJson: AccountJson ) : Boolean  = {
    val availableViews = accountJson.views_available.toList.flatten
    availableViews.exists(view => view.id == Some("owner"))
  }


  def getHostname(): String = {
    Helper.getPropsValue("base_url", "") match {
      case s: String if s.nonEmpty => s.split(":").lift(1) match {
        case Some(s) => s.replaceAll("\\/", "").replaceAll("\\.", "-")
        case None => "unknown"
      }
      case _ => "unknown"
    }
  }

  trait MdcLoggable extends Loggable {
    MDC.put("host" -> getHostname)
  }
  
    /**
    * This function is implemented in order to support encrypted values in props file.
    * Please note that some value is considered as encrypted if has an encryption mark property in addition to regular props value in props file e.g
    *  db.url=Helpers.base64Encode(SOME_ENCRYPTED_VALUE)
    *  db.url.is_encrypted=true
    *  getDecryptedPropsValue("db.url") = jdbc:postgresql://localhost:5432/han_obp_api_9?user=han_obp_api&password=mypassword
    *  Encrypt/Decrypt workflow:
    *  Encrypt: Array[Byte] -> Helpers.base64Encode(encrypted) -> Props file: String -> Helpers.base64Decode(encryptedValue) -> Decrypt: Array[Byte]
    * @param nameOfProperty Name of property which value should be decrypted
    * @return Decrypted value of a property
    */
  def getPropsValue(nameOfProperty: String): Box[String] = {

    /*
    For bank specific branding and possibly other customisations, if we have an active brand (in url param, form field, session),
    we will look for property_FOR_BRAND_<BANK_ID>
    We also check that the property exists, else return the standard property name.
    */
    def getBrandSpecificPropertyName(nameOfProperty: String) : String = {
      // If we have an active brand, construct a target property name to look for.
      activeBrand() match {
        case Some(brand) => s"${nameOfProperty}_FOR_BRAND_${brand}"
        case _ => nameOfProperty
      }
  
    }
    
    val brandSpecificPropertyName = getBrandSpecificPropertyName(nameOfProperty)

    //All the property will first check from system environment, if not find then from the liftweb props file 
    //Replace "." with "_" (environment vars cannot include ".") and convert to upper case
    val sysEnvironmentPropertyName = brandSpecificPropertyName.replace('.', '_').toUpperCase()
    val sysEnvironmentPropertyValue: Box[String] = tryo{sys.env(sysEnvironmentPropertyName)}
    sysEnvironmentPropertyValue match {
      case Full(_) => 
        logger.debug("System environment property value found for: " + sysEnvironmentPropertyName)
        sysEnvironmentPropertyValue
      case _  => Props.get(brandSpecificPropertyName)
    }
  } 


  def getPropsValue(nameOfProperty: String, defaultValue: String): String = {
    getPropsValue(nameOfProperty) openOr(defaultValue)
  }

  def getPropsAsBoolValue(nameOfProperty: String, defaultValue: Boolean): Boolean = {
    getPropsValue(nameOfProperty) map(toBoolean) openOr(defaultValue)
  }
  def getPropsAsIntValue(nameOfProperty: String): Box[Int] = {
    getPropsValue(nameOfProperty) map(toInt)
  }
  def getPropsAsIntValue(nameOfProperty: String, defaultValue: Int): Int = {
    getPropsAsIntValue(nameOfProperty) openOr(defaultValue)
  }
  def getPropsAsLongValue(nameOfProperty: String): Box[Long] = {
    getPropsValue(nameOfProperty) flatMap(asLong)
  }
  def getPropsAsLongValue(nameOfProperty: String, defaultValue: Long): Long = {
    getPropsAsLongValue(nameOfProperty) openOr(defaultValue)
  }

  /**
   * Constructs an Ajax submit button that can be used inside ajax forms.
   * Multiple buttons can be used in the same form.
   * Note:
   * This is enhancement of method net.liftweb.http.SHtml#ajaxSubmit
   *
   * @param value - the button text
   * @param before - the function to be called before ajax function, single String param is name of input
   * @param func - the ajax function to be called, single String param is name of input
   * @param attrs - button attributes
   *
   */
  def ajaxSubmit(value: String, before: String => JsCmd, func: String => JsCmd, attrs: ElemAttr*): Elem = {
    val funcName = "z" + Helpers.nextFuncName

    addFunctionMap(funcName, ()=>func(funcName))
    (attrs.foldLeft(<input type="submit" name={funcName}/>)(_ % _)) %
      new UnprefixedAttribute("value", Text(value), Null) %
      ("onclick" -> (s"${before(funcName).toJsCmd}; liftAjax.lift_uriSuffix = '$funcName=_'; return true;"))

  }

  def renderJson(jValue: JValue): String = jValue match {
    case JNothing => ""
    case v => pretty(render(v))
  }

  //in OBP-API, before it returned v3_1_0, but now, only return v3.1.0
  //But this field will be used in JavaScript/Webpage html id attribute, so need clean the field. 
  //To use any of the meta-characters (such as !"#$%&'()*+,./:;<=>?@[\]^`{|}~) as a literal part of a name, 
  //it must be escaped with with two backslashes: \\. For example, an element with id="foo.bar", 
  //can use the selector $("#foo\\.bar").
  //So in Leftweb:
  // eg: SetHtml(s"OBPv4.0.0_getBank", Text("Wrong OperationId")) --> not working
  // eg: SetHtml(s"OBPv4_0_0_getBank", Text("Wrong OperationId")) --> working, 
  def covertObpOperationIdToWebpageId(operation_id: String) = {
    operation_id.replace(".", "_").replaceAll(" ", "_")
  }


  def covertWebpageIdToObpOperationId(web_page_operation_id: String) = {
    web_page_operation_id.replace("_", ".")
  }

}
