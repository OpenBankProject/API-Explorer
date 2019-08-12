package code.util


import code.util.Helper.MdcLoggable
import net.liftweb.common._
import net.liftweb.http.S

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


  



}