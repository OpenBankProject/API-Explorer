/**
Open Bank Project - API Explorer
Copyright (C) 2011-2018, TESOBE Ltd

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
TESOBE Ltd
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)

  */

package code.snippet

import code.util.Helper.MdcLoggable
import net.liftweb.http.{S, SessionVar}
import net.liftweb.util.Helpers._
import net.liftweb.util.{CssSel, Props, _}

import code.util.Helper._



class WebUI extends MdcLoggable {

  @transient protected val log = logger //Logger(this.getClass)


  // Change the main style sheet
  def mainStyleSheet: CssSel = {
    "#main_style_sheet [href]" #> scala.xml.Unparsed(getPropsValue("webui_main_style_sheet", "./media/css/style.css"))
  }


  // Add an override
  def overrideStyleSheet: CssSel = {
    val stylesheet = getPropsValue("webui_override_style_sheet", "")
    if (stylesheet.isEmpty) {
      "#override_style_sheet" #> ""
    } else {
      "#override_style_sheet [href]" #> scala.xml.Unparsed(stylesheet)
    }
  }



//  These copied from API but not all implemented.

  def headerLogoLeft = {
    "img [src]" #> getPropsValue("webui_header_logo_left_url", "https://static.openbankproject.com/images/OBP_full_web_25pc.png")
  }

  def headerLogoRight: CssSel = {
    "img [src]" #> getPropsValue("webui_header_logo_right_url", "https://static.openbankproject.com/images/obp_logo_stacked.png")
  }

  /*

  def footer2LogoLeft = {
    "img [src]" #> Helper.getPropsValue("webui_footer2_logo_left_url", "")
  }

  def footer2MiddleText: CssSel = {
    "#footer2-middle-text *" #> scala.xml.Unparsed(Helper.getPropsValue("webui_footer2_middle_text", ""))
  }

  def aboutBackground: CssSel = {
    "#main-about [style]" #> ("background-image: url(" + Helper.getPropsValue("webui_index_page_about_section_background_image_url", "") + ");")
  }

  def aboutText: CssSel = {
    "#main-about-text *" #> scala.xml.Unparsed(Helper.getPropsValue("webui_index_page_about_section_text", ""))
  }

  def topText: CssSel = {
    "#top-text *" #> scala.xml.Unparsed(Helper.getPropsValue("webui_top_text", ""))
  }

*/

  def hostedByText: CssSel = {
    "#hosted-by-text *" #> scala.xml.Unparsed(getPropsValue("webui_hosted_by_text", "Hosted by TESOBE"))
  }

  def hostedByLink: CssSel = {
    ".hosted-by-link a [href]" #> scala.xml.Unparsed(getPropsValue("webui_hosted_by_url", "https://www.tesobe.com"))
  }




}

