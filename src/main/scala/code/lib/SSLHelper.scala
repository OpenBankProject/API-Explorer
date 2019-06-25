package code.lib

import java.io.FileInputStream
import java.net.{HttpURLConnection, URL}
import java.security.{KeyStore, SecureRandom}
import java.security.cert.{CertificateFactory, X509Certificate}

import javax.net.ssl.{TrustManagerFactory, _}
import net.liftweb.util.Props

object SSLHelper {


  private lazy val sSLSocketFactory = {
    val keystoreFile = Props.get("obp_certificate_file_path").openOrThrowException("props value of obp_certificate_file_path is missing")

    val cf = CertificateFactory.getInstance("X.509")
    val inputStream = new FileInputStream(keystoreFile)
    val caCert: X509Certificate = try {
      cf.generateCertificate(inputStream).asInstanceOf[X509Certificate]
    } finally {
      inputStream.close()
    }

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    ks.load(null) // You don't need the KeyStore instance to come from a file.

    ks.setCertificateEntry("caCert", caCert)

    tmf.init(ks)

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(null, tmf.getTrustManagers, new SecureRandom())

    val hostnameVerifier: HostnameVerifier = new HostnameVerifier {
      override def verify(host: String, sslSession: SSLSession): Boolean = true
    }

    HttpsURLConnection.setDefaultHostnameVerifier(hostnameVerifier)

    sslContext.getSocketFactory
  }

  def getConnection(url: String): HttpURLConnection = {
    Props.get("obp_certificate_auth_activate", "false") match {
      case "true" => {
        val httpsUrl = if (url.startsWith("https://")) url else url.replaceFirst("^http://", "https://")

        val connection = new URL(httpsUrl).openConnection().asInstanceOf[HttpsURLConnection]
        connection.setSSLSocketFactory(sSLSocketFactory)
        connection
      }
      case "false" => new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      case errorValue => sys.error(s"obp_certificate_activate props should be true or false, current value is: $errorValue")
    }
  }

}
