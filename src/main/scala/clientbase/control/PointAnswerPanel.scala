package clientbase.control

import clientbase.viewer2d.AbstractViewerController
import definition.typ.AnswerDefinition
import org.scalajs.dom.html.Button
import org.scalajs.dom.raw.{Event, Node}
import scalatags.JsDom.all._

class PointAnswerPanel extends AnswerPanel {
  var viewController:Option[AbstractViewerController]=None


  override def reset(): Unit = {
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
    super.loadAnswerParam(nanswerDefinition)
    for (lc<-CreateActionList.lastContainer) lc match {
      case vc:AbstractViewerController=>
        viewController=Some(vc)
        vc.askForPoint()
      case e=> println("PointAnswerPanel load answer, wrong last container "+e)
    }
     panel
  }
}
