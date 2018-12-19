package code.util


import code.util.Helper.MdcLoggable
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.util.Helpers.{asLong, toBoolean, toInt}
import net.liftweb.util.Props


// This file contains a few functions related to Props and Branding copied from OBP API

object Branding extends MdcLoggable {



  /** Check the id values from GUI, such as ACCOUNT_ID, BANK_ID ...  */
  def isValidID(id :String):Boolean= {
    val regex = """^([A-Za-z0-9\-_.]+)$""".r
    id match {
      case regex(e) if(e.length<256) => true
      case _ => false
    }
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

    val brandSpecificPropertyName = getBrandSpecificPropertyName(nameOfProperty)

    logger.debug(s"Standard property $nameOfProperty has bankSpecificPropertyName: $brandSpecificPropertyName")

    // Note: See OBP API for code to encrypt Props

    Props.get(brandSpecificPropertyName)
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

  /*
    Get any brand specified in url parameter or form field, validate it, and if all good, set the session
    Else just return the session
    Note there are Read and Write side effects here!
  */
  def activeBrand() : Option[String] = {

    val brandParameter = "brand"

    // Use brand in parameter (query or form)
    val brand : Option[String] = S.param(brandParameter) match {
      case Full(value) => {
        // If found, and has a valid format, set the session.
        if (isValidID(value)) {
          S.setSessionAttribute(brandParameter, value)
          logger.debug(s"activeBrand says: I found a $brandParameter param. $brandParameter session has been set to: ${S.getSessionAttribute(brandParameter)}")
          Some(value)
        } else {
          logger.warn (s"activeBrand says: Not a valid Id.")
          None
        }
      }
      case _ =>  {
        // Else look in the session
        S.getSessionAttribute(brandParameter)
      }
    }
    brand
  }


  /*
  For bank specific branding and possibly other customisations, if we have an active brand (in url param, form field, session),
  we will look for property_FOR_BRAND_<BANK_ID>
  We also check that the property exists, else return the standard property name.
  */
  def getBrandSpecificPropertyName(nameOfProperty: String) : String = {
    // If we have an active brand, construct a target property name to look for.
    val brandSpecificPropertyName = activeBrand() match {
      case Some(brand) => s"${nameOfProperty}_FOR_BRAND_${brand}"
      case _ => nameOfProperty
    }

    // Check if the property actually exits, if not, return the default / standard property name
    val propertyToUse = Props.get(brandSpecificPropertyName) match {
      case Full(value) => brandSpecificPropertyName
      case _ => nameOfProperty
    }

    propertyToUse
  }



}