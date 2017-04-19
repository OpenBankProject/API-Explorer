package code.util

import code.lib.ObpAPI._
import code.lib.ObpJson.AccountJson
import net.liftweb.common._
import net.liftweb.util.Props


object Helper {



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
    Props.get("base_url", "") match {
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



}
