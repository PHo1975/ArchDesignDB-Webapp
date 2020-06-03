package clientbase.building

import clientbase.control._
import clientbase.viewer2d.Handlers.{CompositionHandler, CompositionStub}
import definition.expression.{Constant, IntConstant}
import org.scalajs.dom.html.Div
import scalatags.JsDom.all._

class PartAreaFieldEditor extends FieldEditor {
  override lazy val fieldComponents: Seq[SidePanelComponent[_]] = Seq(aufbauCombo,alignEdit)

  override val allowedClassNames: Seq[String] = Seq("PartArea")



  lazy val aufbauCombo= new SidePanelComboBox[CompositionStub](CompositionHandler.compositionNames.toSeq,PartAreaFieldEditor.this,Map(("PartArea",3.toByte))) {
    override def defaultValue: CompositionStub = CompositionHandler.undefinedCompositionStub

    override def getConstant(value: CompositionStub): Constant = IntConstant(value.ref.instance)

    override def valueFromConstant(c: Constant): CompositionStub = CompositionHandler.getStub(c.toInt)

    addSearchLookup({
      case l:DecoratedLine => CompositionHandler.getStub(l.pa.aufbau)
      case p:DecoratedPartArea=>CompositionHandler.getStub(p.pa.aufbau)
    })
  }

  lazy val alignEdit=new SidePanelDoubleInputField(Map(("PartArea",4.toByte)),this){
    addSearchLookup({
      case l:DecoratedLine => l.pa.align
      case p:DecoratedPartArea=>p.pa.align
    })
  }


  override lazy val getPanel: Div  = div(style:="float:left")(SidepanelController.panelPart("Aufbau:",aufbauCombo.elem),
    SidepanelController.panelPart("Ausrichtung:",alignEdit.elem)).render
}
