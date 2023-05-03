/**
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Email: contact@tesobe.com
TESOBE / Music Pictures Ltd
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com

 */

package code.lib

import code.util.Helper
import code.util.Helper.MdcLoggable
import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.{LiftResponse, S, SessionVar}
import net.liftweb.util.Helpers.tryo
import oauth.signpost.basic.{DefaultOAuthConsumer, DefaultOAuthProvider}
import oauth.signpost.signature.HmacSha256MessageSigner
import oauth.signpost.{OAuthConsumer, OAuthProvider}

sealed trait Provider {
  val name : String

  val apiBaseUrl : String
  val requestTokenUrl : String
  val accessTokenUrl : String
  val authorizeUrl : String
  val signupUrl : Option[String]

  /**
   * Can't do oAuthProvider = new DefaultOAuthProvider(requestTokenUrl, accessTokenUrl, authorizeUrl)
   * here as the Strings all evaluate at null at this point in object creation
   */
  val oAuthProvider : OAuthProvider

  val consumerKey : String
  val consumerSecret : String
}

trait DefaultProvider extends Provider with MdcLoggable {
  val name = "The Open Bank Project Demo"
  
  val oauthBaseUrl = Helper.getPropsValue("oauth_1.hostname") match {
    case Full(v) =>
      v
    case _ =>
      logger.warn("==========>> THERE IS NO THE VALUE FOR PROPS oauth_1.hostname <<====================")
      Helper.getPropsValue("api_hostname") match {
      case Full(v) => 
        v
      case _ =>
        logger.warn("==========>> THERE IS NO THE VALUE FOR PROPS api_hostname <<====================")
        logger.warn("==========>> DEFAULT VALUE: " + S.hostName + " <<====================")
        S.hostName
    }
  }
  val oauthBaseUrlPortal = Helper.getPropsValue("api_portal_hostname").getOrElse(oauthBaseUrl)

  // To link to API home page (this is duplicated in OAuthClient)
  val baseUrl = Helper.getPropsValue("api_hostname", S.hostName)
  val apiBaseUrl = baseUrl + "" // Was "/obp"
  val requestTokenUrl = oauthBaseUrl + "/oauth/initiate"
  val accessTokenUrl = oauthBaseUrl + "/oauth/token"
  val authorizeUrl = oauthBaseUrlPortal + "/oauth/authorize"
  val signupUrl = Some(oauthBaseUrl + "/user_mgt/sign_up")

  lazy val oAuthProvider : OAuthProvider = new ObpOAuthProvider(requestTokenUrl, accessTokenUrl, authorizeUrl)

  val consumerKey = Helper.getPropsValue("obp_consumer_key", "")
  val consumerSecret = Helper.getPropsValue("obp_secret_key", "")
}

object OBPProvider extends DefaultProvider

object AddBankAccountProvider extends DefaultProvider {
  override val name = "The Open Bank Project Demo - Add Bank Account"

  //The "login" prefix before /oauth means that we will use the oauth flow that will ask the user
  //to connect a bank account
  override val requestTokenUrl = oauthBaseUrl + "/login/oauth/initiate"
  override val accessTokenUrl = oauthBaseUrl + "/login/oauth/token"
  override val authorizeUrl = oauthBaseUrl + "/login/oauth/authorize"
}

case class Consumer(consumerKey : String, consumerSecret : String) {
  val oAuthConsumer : OAuthConsumer = new DefaultOAuthConsumer(consumerKey, consumerSecret)
}

case class Credential(provider : Provider, consumer : OAuthConsumer, readyToSign : Boolean)

object credentials extends SessionVar[Option[Credential]](None)
object mostRecentLoginAttemptProvider extends SessionVar[Box[Provider]](Empty)

object OAuthClient extends MdcLoggable {

  def getAuthorizedCredential() : Option[Credential] = {
    credentials.filter(_.readyToSign)
  }

  def currentApiBaseUrl : String = {
    getAuthorizedCredential().map(_.provider.apiBaseUrl).getOrElse(OBPProvider.apiBaseUrl)
  }

  def setNewCredential(provider : Provider) : Credential = {
    val consumer = new DefaultOAuthConsumer(provider.consumerKey, provider.consumerSecret)
    val credential = Credential(provider, consumer, false)

    credentials.set(Some(credential))
    credential
  }

  def handleCallback(): Box[LiftResponse] = {

    val success = for {
      verifier <- S.param("oauth_verifier") ?~ "No oauth verifier found"
      provider <- mostRecentLoginAttemptProvider.get ?~ "No provider found for callback"
      consumer <- Box(credentials.map(_.consumer)) ?~ "No consumer found for callback"
      //eg: authUrl = http://127.0.0.1:8080/oauth/authorize?oauth_token=LK5N1WBQZGXHMQXJT35KDHAJXUP1EMQCGBQFQQNG
      //This step is will call `provider.authorizeUrl = baseUrl + "/oauth/authorize"` endpoint, and get the request token back.
      _<-Full(logger.debug("oauth.provider.name            = " + provider.name           ))
      _<-Full(logger.debug("oauth.provider.apiBaseUrl      = " + provider.apiBaseUrl     ))
      _<-Full(logger.debug("oauth.provider.requestTokenUrl = " + provider.requestTokenUrl))
      _<-Full(logger.debug("oauth.provider.accessTokenUrl  = " + provider.accessTokenUrl ))
      _<-Full(logger.debug("oauth.provider.authorizeUrl    = " + provider.authorizeUrl   ))
      _<-Full(logger.debug("oauth.provider.signupUrl       = " + provider.signupUrl      ))
      _<-Full(logger.debug("oauth.provider.oAuthProvider.getRequestTokenEndpointUrl   = " + provider.oAuthProvider.getRequestTokenEndpointUrl  ))     //http://127.0.0.1:8080/oauth/initiate    
      _<-Full(logger.debug("oauth.provider.oAuthProvider.getAccessTokenEndpointUrl   = " + provider.oAuthProvider.getAccessTokenEndpointUrl  ))     //http://127.0.0.1:8080/oauth/token    
      _<-Full(logger.debug("oauth.provider.oAuthProvider.getAuthorizationWebsiteUrl   = " + provider.oAuthProvider.getAuthorizationWebsiteUrl  ))    //http://127.0.0.1:8080/oauth/authorize     
      _<-Full(logger.debug("oauth.provider.consumerKey     = " + provider.consumerKey    ))
      _<-Full(logger.debug("oauth.provider.consumerSecret  = " + provider.consumerSecret ))
      _<-Full(logger.debug("oauth.consumer.getToken(request) = " + consumer.getToken))
      _<-Full(logger.debug("oauth.consumer.getTokenSecret(request)  = " + consumer.getTokenSecret))
      //after this, consumer is ready to sign requests
      _ <- tryo{provider.oAuthProvider.retrieveAccessToken(consumer, verifier)} 
    } yield {
      //update the session credentials
      val newCredential = Credential(provider, consumer, true)
      val updateCredential = credentials.set(Some(newCredential))
      logger.debug("oauth.credential.getToken(access) = " + newCredential.consumer.getToken)
      logger.debug("oauth.credential.getTokenSecret(access) = " + newCredential.consumer.getTokenSecret)
      updateCredential
    }

    success match {
      case Full(_) => S.redirectTo("/") //TODO: Allow this redirect to be customised
      case Failure(msg, exception, failure) => {
        S.notice("redirect-link","Go To Home Page")
        logger.warn(s"Oauth1.0 Failure: $msg, ${failure}, ${exception}")
      } 
      case _ => logger.warn("Something went wrong in an oauth callback and there was no error message set for it")
    }
    Empty
  }

  def redirectToOauthLogin() = {
    redirect(OBPProvider)
  }

  private def redirect(provider : Provider) = {
    mostRecentLoginAttemptProvider.set(Full(provider))
    val credential = setNewCredential(provider)

    val oauthcallbackUrl = Helper.getPropsValue("base_url", S.hostName) + "/oauthcallback"
    logger.debug("redirect says: credential.consumer.getConsumerKey: " + credential.consumer.getConsumerKey)
    logger.debug("redirect says: credential.consumer.getToken: " + credential.consumer.getToken)
    logger.debug("redirect says: credential.provider: " + credential.provider)
    logger.debug("redirect says: oauthcallbackUrl: " + oauthcallbackUrl)
    credential.consumer.setMessageSigner(new HmacSha256MessageSigner())
    val authUrlBox = tryo {provider.oAuthProvider.retrieveRequestToken(credential.consumer, oauthcallbackUrl)}

    if(authUrlBox.isInstanceOf[Failure]) {
      val errorMessage = "Critical exception happened on the backend: " + authUrlBox.asInstanceOf[Failure].messageChain
      logger.error(errorMessage)
      throw new Exception(errorMessage)
    } else if(authUrlBox.isEmpty){
      logger.error("Critical exception happened on backend: oauth callback Url is empty! Please check the consumer key and secret first.")
      throw new Exception("Critical exception happened on backend: oauth callback Url is empty! Please check the consumer key and secret first.")
    } else{
      logger.debug("redirect says: authUrlBox: " + authUrlBox.head)
      S.redirectTo(authUrlBox.head)
    }
    
  }

  def redirectToConnectBankAccount() = {
    redirect(AddBankAccountProvider)
  }

  def loggedIn : Boolean = credentials.map(_.readyToSign).getOrElse(false)

  def logoutAll() = {
    val (apiExplorerHost: String, obpApiHost: String) = logoutApiExplorer
    S.redirectTo(s"$obpApiHost/user_mgt/logout?redirect=$apiExplorerHost")
  }
  
  def logoutApiExplorerAndObpApi() = {
    val (_: String, obpApiHost: String) = logoutApiExplorer
    S.redirectTo(s"$obpApiHost/user_mgt/logout")
  }

  private def logoutApiExplorer = {
    val apiExplorerHost = {
      Helper.getPropsValue("base_url", S.hostName)
    }
    val obpApiHost = Helper.getPropsValue("api_portal_hostname")
      .getOrElse(Helper.getPropsValue("api_hostname", "Unknown"))
    credentials.set(None)
    (apiExplorerHost, obpApiHost)
  }
}