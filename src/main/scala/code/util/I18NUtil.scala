package code.util

import java.util.{Date, Locale}

import net.liftweb.common.Full
import net.liftweb.http.S
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.util.Props

object I18NUtil {
  // Copied from Sofit
  def getLocalDate(date: Date): String = {
    import java.text.DateFormat
    val df = DateFormat.getDateInstance(DateFormat.LONG, currentLocale())
    val formattedDate = df.format(date)
    formattedDate
  }

  def getLocale(): Locale = Locale.getAvailableLocales().toList.filter { l =>
    l.toLanguageTag == Props.get("language_tag", "en-GB")
  }.headOption.getOrElse(Locale.ENGLISH)

  def currentLocale() : Locale = {
    // Cookie name
    val localeCookieName = "SELECTED_LOCALE"
    S.param("locale") match {
      // 1st choice: Use query parameter as a source of truth if any
      case Full(requestedLocale) if requestedLocale != null => {
        val computedLocale = I18NUtil.computeLocale(requestedLocale)
        S.addCookie(HTTPCookie(localeCookieName, requestedLocale))
        computedLocale
      }
      // 2nd choice: Otherwise use the cookie  
      case _ =>
        S.findCookie(localeCookieName).flatMap {
          cookie => cookie.value.map(computeLocale)
        } openOr getLocale()
    }
  }
  // Properly convert a language tag to a Locale
  def computeLocale(tag : String) = tag.split(Array('-', '_')) match {
    case Array(lang) => new Locale(lang)
    case Array(lang, country) => new Locale(lang, country)
    case Array(lang, country, variant) => new Locale(lang, country, variant)
  }

}
