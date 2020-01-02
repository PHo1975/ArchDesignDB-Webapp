package clientbase.control

import clientbase.viewer2d.AbstractViewerController
import definition.expression.VectorConstant
import definition.typ.AnswerDefinition
import org.scalajs.dom.html.Button
import org.scalajs.dom.raw.{Event, Node}
import scalatags.JsDom.all._

trait BracketListener{
  def bracketModeStarted():Unit
}

trait PointClickListener extends BracketListener {
  def pointClicked(point:VectorConstant): Unit
  def forcePrecision: Boolean
}

class PointAnswerPanel extends AnswerPanel with PointClickListener {
  var viewController:Option[AbstractViewerController]=None
  var internPointClickListener:Option[(VectorConstant)=>Unit]=None
  var forcePrecision: Boolean = true

  override def reset(): Unit = {
    internPointClickListener=None
  }

  override def focus(): Unit = for(vc<-viewController) vc.focus()

  def createButton(name:String,description:String,func:Event=>Unit): Button =button(`class`:="point-answer-button",title:=description,tabindex:="-1",onclick:=func)(name).render

  val bracketButton: Button =button(style:="float: left;margin-right: 80px;margin-left: 5px;",title:="Summenfunktion",tabindex:="-1")("Σ").render
  bracketButton.onclick= _ =>  for(vc<-viewController)
      if (vc.bracketMode) {
        DialogManager.answerGiven(answerDefinition,vc.bracketPointer)
        reset()
      } else {
        vc.startBracketMode()
      }


  panel.appendChild(bracketButton)
  panel.appendChild(createButton("dx","delta x",e=>{

  }))
  panel.appendChild(createButton("dy","delta y",e=>{

  }))
  panel.appendChild(createButton("dz","delta z", e=>{

  }))
  panel.appendChild(createButton("Gl","Globaler Punkt",e=>{

  }))
  panel.appendChild(createButton("Mi","Mittelpunkt",e=>{

  }))
  panel.appendChild(createButton("Te","Teilungspunkt",e=>{

  }))


  override def loadAnswerParam(nanswerDefinition: AnswerDefinition): Node = {
    reset()
    super.loadAnswerParam(nanswerDefinition)
    for (lc<-SelectionController.lastContainer) lc match {
      case vc:AbstractViewerController=>
        viewController=Some(vc)
        vc.askForPoint(this)
      case e=> println("PointAnswerPanel load answer, wrong last container "+e)
    }
     panel
  }

  override def pointClicked(point: VectorConstant): Unit =
    internPointClickListener match{
      case Some(listener)=> internPointClickListener=None; listener(point)
      case None => DialogManager.answerGiven(answerDefinition,point)
  }


  override def bracketModeStarted(): Unit = {

  }
}

object PointAnswerPanel {
  val STRICT_HIT = "strict"
  val NOSTRICT_HIT = "nostrict"
}
