package clientbase.control

import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.KeyboardEvent
import scalatags.JsDom.all._

/**
  * Created by Peter Holzer on 18.01.2016.
  */
object ActiveEditField {

  def apply(typeString: String, cssClass: String, callback: String => Unit): Input = {
    val editField = input(`type` := typeString, `class` := cssClass).render
    editField.onkeydown = { key: KeyboardEvent =>
      key.keyCode match {
        case 13 => key.stopPropagation()
          callback(editField.value)
        case _=>
      }
    }
    editField
  }
}
