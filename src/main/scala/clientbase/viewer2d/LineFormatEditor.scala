package clientbase.viewer2d

import clientbase.control.{FieldEditor, SidePanelComboBox, SidePanelComponent, SidepanelController}
import definition.expression.{Constant, IntConstant}
import org.scalajs.dom.html.Div
import scalatags.JsDom.all._

class LineFormatEditor extends FieldEditor{
  val allowedClassNames: Seq[String] = Seq("LineElem", "ArcElem", "PolyElem", "EllipseElem", "AreaPolygon", "Wohnfläche")
  val widthFieldNr: Byte = 1.toByte
  val styleFieldNr: Byte = 2.toByte

  import LineStyleEditor.widthList
  lazy val widthCombo: SidePanelComboBox[Int] = new SidePanelComboBox(widthList,  this, (allowedClassNames.map((_, widthFieldNr)) :+ (("AreaPolygon", 2.toByte)) :+ (("Wohnfläche", 2.toByte))).toMap) {
    val defaultValue=0

    def getConstant(value: Int): Constant = IntConstant(value)

    def valueFromConstant(c: Constant): Int = c.toInt

    addSearchLookup({
      case t:LinearElement => t.lineWidth
    })
  }

  lazy val styleCombo: SidePanelComboBox[Int] =new SidePanelComboBox(LineStyleHandler.styles.map(_.ix),this,
    (allowedClassNames.map((_,styleFieldNr)):+(("AreaPolygon",3.toByte))).toMap) {
    val defaultValue: Int = LineStyleHandler.undefinedStyle.ix

    def getConstant(value: Int) = IntConstant(value)

    def valueFromConstant(c: Constant): Int = c.toInt

    addSearchLookup({
      case t:LinearElement => t.lineStyle
    })
  }

  lazy val fieldComponents: Seq[SidePanelComponent[_]] = Seq(widthCombo,styleCombo)

  override def  getPanel: Div= panel

  lazy val panel: Div = div(style:="float:left")(SidepanelController.panelPart("Dicke",widthCombo.elem),
    SidepanelController.panelPart("Stil",styleCombo.elem)).render
}

object LineStyleEditor{
  val widthList=List(0,10,15,20,25,35,50,70,100,200)
}
