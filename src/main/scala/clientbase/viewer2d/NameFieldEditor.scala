package clientbase.viewer2d

import clientbase.control.{FieldEditor, SidepanelController, SiedPanelStringInputField}
import definition.data.Named
import org.scalajs.dom.html.Div

class NameFieldEditor extends FieldEditor {

  val textField = new SiedPanelStringInputField(Map(("Plane", 0), ("Room", 0), ("AreaPolygon", 9),
  ("MeasurePolyLine", 10), ("Wohnfläche", 9)), this) {
  addSearchLookup({  case n:Named=>n.name })
}

  val fieldComponents=Seq(textField)
  val allowedClassNames: Iterable[String] = textField.allowedFields.keys

  val panel: Div =SidepanelController.panelPart("Name",textField.elem)

  def getPanel = panel
}
